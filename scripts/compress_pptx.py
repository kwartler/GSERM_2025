#!/usr/bin/env python3
"""
Shrink a .pptx without changing its content.

What it does (and ONLY this):
  1. Compresses raster images: downscale to a max longest-side, re-encode
     opaque images as JPEG and truly-transparent images as optimized PNG.
     Slide XML is never touched -- images are referenced by relationship id
     (r:embed), so only the Target filename inside *.rels changes when an
     extension flips from .png to .jpeg.
  2. Re-encodes embedded videos at a lower frame rate (filename unchanged).
  3. Drops slide layouts that no slide references (parts + rels +
     sldLayoutIdLst + [Content_Types].xml overrides).

What it never does: reword/add/remove slide text, remove slides (even hidden
ones), or alter any non-media part. Every other part is copied byte-identical.

Usage:
  python3 compress_pptx.py INPUT.pptx OUTPUT.pptx \
      [--max-dim 1920] [--jpeg-quality 85] [--video-fps 15] \
      [--no-prune-layouts]
"""
import argparse
import io
import os
import re
import shutil
import subprocess
import sys
import tempfile
import zipfile
from xml.etree import ElementTree as ET

from PIL import Image

RASTER_EXT = {"png", "jpg", "jpeg", "bmp", "tif", "tiff"}
VIDEO_EXT = {"mp4", "mov", "m4v", "avi", "wmv"}

# Stamp written into every media part we (re)encode. On later runs we recognize
# our own output and pass it through untouched, so an image or video is only ever
# compressed ONCE no matter how many times the deck is re-compressed. This is the
# quality floor: it stops generation loss (each re-encode of an already-lossy
# JPEG/H.264 compounds artifacts and softens the picture).
MARKER = "cpptx1"


def _is_marked_image(im):
    """True if this image carries our compression stamp (JPEG COM or PNG text)."""
    c = im.info.get("comment")
    if isinstance(c, bytes):
        c = c.decode("latin-1", "ignore")
    if c and MARKER in c:
        return True
    return im.info.get("cpptx") == MARKER


def has_real_alpha(im):
    """True only if the image has at least one non-opaque pixel."""
    if im.mode in ("RGBA", "LA") or (im.mode == "P" and "transparency" in im.info):
        alpha = im.convert("RGBA").getchannel("A")
        return alpha.getextrema()[0] < 255
    return False


def reencode_image(name, data, max_dim, jpeg_quality, bpp_floor, log):
    """Return (new_name, new_bytes). new_name differs only when png->jpeg."""
    ext = name.rsplit(".", 1)[-1].lower()
    if ext not in RASTER_EXT:
        return name, data
    try:
        im = Image.open(io.BytesIO(data))
        im.load()
    except Exception as e:
        log.append(f"    ! {os.path.basename(name)}: cannot open ({e}); kept as-is")
        return name, data

    orig_size = len(data)
    w, h = im.size
    needs_downscale = max(w, h) > max_dim
    bpp = (orig_size * 8.0) / max(1, w * h)

    # --- Quality floor: never recompress an already-compressed image. ---
    # Re-encoding a lossy image compounds artifacts every pass. If the picture is
    # already within the size bound and is either (a) stamped by a previous run,
    # or (b) an already-lean JPEG (bits/pixel at or below the floor, i.e. it has
    # clearly been compressed before), we copy its bytes through verbatim. That
    # makes repeated runs idempotent and caps every image at a single
    # compression. We only step in when there's real, non-destructive work to do
    # (a resize, or a fat/uncompressed source).
    if not needs_downscale:
        if _is_marked_image(im):
            log.append(f"    . {os.path.basename(name)}: already compressed (stamped); untouched")
            return name, data
        if ext in ("jpg", "jpeg") and bpp <= bpp_floor:
            log.append(f"    . {os.path.basename(name)}: already-lean JPEG "
                       f"({bpp:.2f} bpp <= {bpp_floor}); untouched")
            return name, data

    if needs_downscale:
        scale = max_dim / float(max(w, h))
        im = im.resize((max(1, int(w * scale)), max(1, int(h * scale))), Image.LANCZOS)

    alpha = has_real_alpha(im)

    if alpha:
        # Keep transparency -> optimized PNG, stamped via a text chunk.
        from PIL import PngImagePlugin
        meta = PngImagePlugin.PngInfo()
        meta.add_text("cpptx", MARKER)
        buf = io.BytesIO()
        im.save(buf, "PNG", optimize=True, pnginfo=meta)
        new_name, new_bytes = name, buf.getvalue()
    else:
        # Opaque -> JPEG, stamped via the comment (COM) marker.
        buf = io.BytesIO()
        im.convert("RGB").save(buf, "JPEG", quality=jpeg_quality, optimize=True,
                               progressive=True, comment=MARKER.encode("ascii"))
        new_bytes = buf.getvalue()
        new_name = re.sub(r"\.(png|bmp|tif|tiff)$", ".jpeg", name, flags=re.I)

    # Never make a part bigger than it started.
    if len(new_bytes) >= orig_size:
        log.append(
            f"    = {os.path.basename(name)}: {orig_size//1024} KB kept "
            f"(re-encode not smaller)"
        )
        return name, data

    tag = "->jpeg" if new_name != name else ("png" if alpha else "jpeg")
    log.append(
        f"    - {os.path.basename(name)} {orig_size//1024} KB -> "
        f"{len(new_bytes)//1024} KB ({tag}, {im.size[0]}x{im.size[1]})"
    )
    return new_name, new_bytes


def reencode_video(name, data, fps, log):
    ext = name.rsplit(".", 1)[-1].lower()
    if ext not in VIDEO_EXT or shutil.which("ffmpeg") is None:
        return data
    tmpd = tempfile.mkdtemp()
    try:
        src = os.path.join(tmpd, "in." + ext)
        dst = os.path.join(tmpd, "out.mp4")
        with open(src, "wb") as f:
            f.write(data)
        # Quality floor for video: a clip we already processed carries our stamp
        # in its container metadata. Skip it so we don't re-encode H.264 again
        # (which would soften motion and add blocking every pass).
        if shutil.which("ffprobe"):
            probe = subprocess.run(
                ["ffprobe", "-v", "quiet", "-show_entries",
                 "format_tags=comment", "-of", "default=nw=1:nk=1", src],
                capture_output=True, text=True)
            if MARKER in (probe.stdout or ""):
                log.append(f"    . {os.path.basename(name)}: already compressed (stamped); untouched")
                return data
        cmd = [
            "ffmpeg", "-y", "-i", src,
            "-r", str(fps),
            "-c:v", "libx264", "-crf", "26", "-preset", "slow",
            "-c:a", "aac", "-b:a", "64k",
            "-metadata", f"comment={MARKER}",
            "-movflags", "+faststart",
            dst,
        ]
        r = subprocess.run(cmd, capture_output=True)
        if r.returncode != 0 or not os.path.exists(dst):
            log.append(f"    ! {os.path.basename(name)}: ffmpeg failed; kept as-is")
            return data
        new = open(dst, "rb").read()
        if len(new) >= len(data):
            log.append(f"    = {os.path.basename(name)}: re-encode not smaller; kept")
            return data
        log.append(
            f"    - {os.path.basename(name)} {len(data)//1024} KB -> "
            f"{len(new)//1024} KB ({fps} fps)"
        )
        return new
    finally:
        shutil.rmtree(tmpd, ignore_errors=True)


def find_unused_layouts(parts):
    """Return set of layout part names not referenced by any slide's rels."""
    all_layouts = {
        n for n in parts
        if re.fullmatch(r"ppt/slideLayouts/slideLayout\d+\.xml", n)
    }
    used = set()
    for name, data in parts.items():
        if re.fullmatch(r"ppt/slides/_rels/slide\d+\.xml\.rels", name):
            for m in re.finditer(r'Target="[^"]*?(slideLayout\d+\.xml)"', data.decode("utf-8", "replace")):
                used.add("ppt/slideLayouts/" + m.group(1))
    return all_layouts - used


def prune_layouts(parts, unused, log):
    """Remove unused layout parts and every reference to them."""
    if not unused:
        return
    P = "{http://schemas.openxmlformats.org/presentationml/2006/main}"
    R = "{http://schemas.openxmlformats.org/officeDocument/2006/relationships}"

    # Map each master to the rel-ids that point at an unused layout.
    for master in [n for n in parts if re.fullmatch(r"ppt/slideMasters/slideMaster\d+\.xml", n)]:
        rels_name = f"ppt/slideMasters/_rels/{os.path.basename(master)}.rels"
        if rels_name not in parts:
            continue
        rels_root = ET.fromstring(parts[rels_name])
        ns_rel = "http://schemas.openxmlformats.org/package/2006/relationships"
        drop_ids = set()
        for rel in list(rels_root):
            tgt = rel.get("Target", "")
            resolved = os.path.normpath(os.path.join("ppt/slideMasters", tgt)).replace("\\", "/")
            if resolved in unused:
                drop_ids.add(rel.get("Id"))
                rels_root.remove(rel)
        if drop_ids:
            parts[rels_name] = ET.tostring(rels_root, xml_declaration=True, encoding="UTF-8")
            # Strip matching <p:sldLayoutId> entries from the master.
            mroot = ET.fromstring(parts[master])
            lst = mroot.find(f"{P}sldLayoutIdLst")
            if lst is not None:
                for sld in list(lst):
                    if sld.get(f"{R}id") in drop_ids:
                        lst.remove(sld)
            parts[master] = ET.tostring(mroot, xml_declaration=True, encoding="UTF-8")

    # Remove the layout parts, their rels, and Content_Types overrides.
    ct = parts["[Content_Types].xml"].decode("utf-8")
    for layout in unused:
        parts.pop(layout, None)
        parts.pop(f"ppt/slideLayouts/_rels/{os.path.basename(layout)}.rels", None)
        ct = re.sub(rf'<Override PartName="/{re.escape(layout)}"[^>]*/>', "", ct)
    parts["[Content_Types].xml"] = ct.encode("utf-8")
    log.append(f"  Dropped {len(unused)} unused layout(s): "
               + ", ".join(sorted(os.path.basename(u) for u in unused)))


def apply_renames(parts, renames, log):
    """Update Target filenames in every .rels file for renamed media."""
    if not renames:
        return
    base_map = {os.path.basename(o): os.path.basename(n) for o, n in renames.items()}
    for name in list(parts):
        if not name.endswith(".rels"):
            continue
        text = parts[name].decode("utf-8")
        changed = False
        for old_b, new_b in base_map.items():
            needle = old_b + '"'
            if needle in text:
                text = text.replace(needle, new_b + '"')
                changed = True
        if changed:
            parts[name] = text.encode("utf-8")
    # Media parts were already moved to their new names by the caller; here we
    # only fix the relationship Target filenames that point at them.
    log.append(f"  Renamed {len(renames)} part(s) png->jpeg (relationship targets updated)")


def process(in_path, out_path, max_dim, jpeg_quality, video_fps, do_prune, bpp_floor):
    log = []
    with zipfile.ZipFile(in_path, "r") as z:
        names = z.namelist()
        infos = {i.filename: i for i in z.infolist()}
        parts = {n: z.read(n) for n in names}

    # --- Fonts: report only (we never embed; nothing to strip) ---
    embedded_fonts = [n for n in parts if n.startswith("ppt/fonts/")]
    if embedded_fonts:
        log.append(f"  Note: {len(embedded_fonts)} embedded font part(s) present (left intact).")
    else:
        log.append("  Fonts: none embedded -- already minimal, nothing to strip.")

    # --- Images ---
    log.append("  Images:")
    renames = {}
    for name in [n for n in list(parts) if n.startswith("ppt/media/")]:
        ext = name.rsplit(".", 1)[-1].lower()
        if ext in VIDEO_EXT:
            parts[name] = reencode_video(name, parts[name], video_fps, log)
            continue
        new_name, new_bytes = reencode_image(name, parts[name], max_dim, jpeg_quality, bpp_floor, log)
        if new_name != name:
            del parts[name]
            parts[new_name] = new_bytes
            renames[name] = new_name
        else:
            parts[name] = new_bytes

    apply_renames(parts, renames, log)

    # --- Layouts ---
    if do_prune:
        prune_layouts(parts, find_unused_layouts(parts), log)

    # --- Write output (preserve order; compress XML, store already-compressed media) ---
    order = [n for n in names if n in parts] + [n for n in parts if n not in names]
    with zipfile.ZipFile(out_path, "w", zipfile.ZIP_DEFLATED) as z:
        for n in order:
            data = parts[n]
            ext = n.rsplit(".", 1)[-1].lower()
            # Media is already compressed; storing avoids wasted CPU and double-compression.
            ctype = zipfile.ZIP_STORED if ext in (RASTER_EXT | VIDEO_EXT) else zipfile.ZIP_DEFLATED
            zi = zipfile.ZipInfo(n, date_time=infos[n].date_time if n in infos else (1980, 1, 1, 0, 0, 0))
            zi.compress_type = ctype
            zi.external_attr = infos[n].external_attr if n in infos else 0
            z.writestr(zi, data)
    return log


def validate(out_path):
    """Cheap integrity check: zip ok, XML parses, images re-openable."""
    with zipfile.ZipFile(out_path) as z:
        bad = z.testzip()
        if bad:
            raise RuntimeError(f"corrupt entry: {bad}")
        slides = sum(1 for n in z.namelist() if re.fullmatch(r"ppt/slides/slide\d+\.xml", n))
        for n in z.namelist():
            if n.endswith(".xml") or n.endswith(".rels"):
                ET.fromstring(z.read(n))
            elif n.startswith("ppt/media/") and n.rsplit(".", 1)[-1].lower() in RASTER_EXT:
                Image.open(io.BytesIO(z.read(n))).verify()
    return slides


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("input")
    ap.add_argument("output")
    ap.add_argument("--max-dim", type=int, default=1920)
    ap.add_argument("--jpeg-quality", type=int, default=85)
    ap.add_argument("--video-fps", type=int, default=15)
    ap.add_argument("--no-prune-layouts", action="store_true")
    ap.add_argument("--jpeg-bpp-floor", type=float, default=3.0,
                    help="JPEGs already at or below this bits/pixel are treated as "
                         "already-compressed and passed through untouched (quality floor).")
    args = ap.parse_args()

    before = os.path.getsize(args.input)
    print(f"== {os.path.basename(args.input)}  ({before/1e6:.1f} MB) ==")
    log = process(args.input, args.output, args.max_dim, args.jpeg_quality,
                  args.video_fps, not args.no_prune_layouts, args.jpeg_bpp_floor)
    for line in log:
        print(line)
    slides = validate(args.output)
    after = os.path.getsize(args.output)
    in_slides = None
    with zipfile.ZipFile(args.input) as z:
        in_slides = sum(1 for n in z.namelist() if re.fullmatch(r"ppt/slides/slide\d+\.xml", n))
    assert slides == in_slides, f"SLIDE COUNT CHANGED {in_slides}->{slides}"
    print(f"  Slides: {slides} (unchanged)  VALID")
    print(f"  {before/1e6:.1f} MB -> {after/1e6:.1f} MB  "
          f"({100*(before-after)/before:.0f}% smaller)\n")


if __name__ == "__main__":
    main()
