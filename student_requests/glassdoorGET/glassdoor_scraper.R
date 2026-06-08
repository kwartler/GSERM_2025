# =============================================================================
# glassdoor_scraper.R
# Core functions for collecting Glassdoor reviews via a real Chrome browser.
#
# IMPORTANT NOTICE FOR THE RESEARCHER USING THIS SCRIPT:
# -----------------------------------------------------------------------------
# Glassdoor's Terms of Service (Section 3) explicitly prohibit automated
# scraping of their website. Before using this script you should:
#
#   1. Read Glassdoor's ToS: https://www.glassdoor.com/about/terms.htm
#   2. Consult your institution's IRB / ethics board about data collection
#      from platforms with ToS restrictions.
#   3. Note this as a methodology caveat in any publication or thesis chapter.
#
# This script is provided for academic research purposes. Use at small scale,
# do not redistribute collected data commercially, and respect rate limits.
# The decision to proceed is yours — not your supervisor's.
# =============================================================================

library(chromote)
library(jsonlite)
library(dplyr)

# How long to pause between page requests (seconds). Increase if you get
# blocked. Decrease at your own risk — too fast triggers rate limiting.
PAUSE_BETWEEN_PAGES <- 4

# How long to wait for page content to load after navigation (seconds).
PAGE_LOAD_WAIT <- 3

# Internal state holder (tracks the Chrome process so we can shut it down later).
# You do not need to touch this.
.gd_env <- new.env(parent = emptyenv())


# -----------------------------------------------------------------------------
# gd_find_free_port  (internal helper)
#
# Finds a TCP port that nothing is currently listening on. Using a fresh port
# each run prevents us from accidentally attaching to a leftover Chrome from a
# previous (possibly failed) run — the usual cause of "End of File" / websocket
# errors on the second attempt.
# -----------------------------------------------------------------------------
gd_find_free_port <- function(tries = 50) {
  for (i in seq_len(tries)) {
    port <- sample(9223:9899, 1)
    in_use <- tryCatch({
      con <- suppressWarnings(socketConnection(
        "127.0.0.1", port, open = "r+", blocking = TRUE, timeout = 1
      ))
      close(con)
      TRUE            # connected => something is already listening
    }, error = function(e) FALSE)  # refused => port is free
    if (!in_use) return(port)
  }
  stop("Could not find a free port for Chrome after ", tries, " tries.")
}


# -----------------------------------------------------------------------------
# MARKETS
#
# Glassdoor runs one platform across many country sites. The page structure is
# identical everywhere (same data-test attributes), but the domain, the URL word
# for "Reviews", and the language differ. A "market" bundles those differences
# so the same code can scrape either site.
#
#   GD_MARKET_US : the US site (glassdoor.com), English reviews.
#                  Requires a US IP address (US VPN or proxy) if you are abroad.
#   GD_MARKET_DE : the Swiss/German site (de.glassdoor.ch), German reviews.
#                  Works directly from a Swiss/German IP — no VPN needed.
# -----------------------------------------------------------------------------
GD_MARKET_US <- list(
  key         = "US",
  base_url    = "https://www.glassdoor.com",
  rev_segment = "Reviews",                 # the word used in review URLs
  login_url   = "https://www.glassdoor.com/profile/login_input.htm",
  chrome_lang = "en-US",
  accept_lang = "en-US,en;q=0.9"
)

GD_MARKET_DE <- list(
  key         = "DE",
  base_url    = "https://de.glassdoor.ch",
  rev_segment = "Bewertungen",             # German review URLs use "Bewertungen"
  login_url   = "https://de.glassdoor.ch/index.htm",  # home page; click "Anmelden"
  chrome_lang = "de-DE",
  accept_lang = "de-DE,de;q=0.9"
)


# -----------------------------------------------------------------------------
# gd_start_session
#
# Launches a REAL, VISIBLE Chrome window and connects chromote to it.
#
# Why this is done the hard way: chromote always launches Chrome in headless
# (invisible) mode — there is no option to turn that off. Headless is no good
# here because you need to log in by hand. So instead we start Chrome ourselves
# as a normal visible window (with remote debugging enabled) and then attach
# chromote to that window.
#
# A separate, temporary Chrome profile is used so this window is independent of
# any Chrome you already have open, and so a fresh browser actually launches.
#
# FORCING THE US SITE (glassdoor.com):
# Glassdoor redirects you to a country-specific site (e.g. de.glassdoor.ch)
# based on your IP address — not your browser settings. If you are outside the
# US, the .com URLs will redirect and the pages won't be found. To stay on the
# US site you need a US IP address. Two options:
#   1. Turn on a US VPN on your computer (simplest) — then leave proxy = NULL.
#   2. Pass a US proxy here, e.g.:
#        gd_start_session(proxy = "http://USER:PASS@us-proxy-host:PORT")
#      (or "socks5://host:port"). Chrome will route all traffic through it.
#
# Args:
#   market     : GD_MARKET_US or GD_MARKET_DE — sets the browser language.
#   debug_port : port to talk to Chrome on. Default NULL = pick a free one
#                automatically (recommended — avoids clashing with leftover
#                Chrome from a previous run).
#   proxy      : optional proxy URL to route Chrome through (for a US IP).
#
# Returns a chromote session object you pass to all other functions.
# Call gd_close(session) when you are completely finished.
# -----------------------------------------------------------------------------
gd_start_session <- function(market = GD_MARKET_US, debug_port = NULL, proxy = NULL) {
  # --- Compatibility guard -----------------------------------------------
  # This approach was written and tested against chromote 0.5.1. It relies on
  # a few chromote internals to connect to a visible Chrome window. If any are
  # missing (very old or substantially changed chromote), fail with a clear
  # message instead of a cryptic internal error.
  installed_ver <- as.character(utils::packageVersion("chromote"))
  required <- c("find_chrome", "ChromeRemote", "Chromote")
  missing  <- required[!vapply(required, exists, logical(1),
                               where = asNamespace("chromote"))]
  if (length(missing) > 0) {
    stop(
      "Your installed chromote (version ", installed_ver, ") is missing: ",
      paste(missing, collapse = ", "), ".\n",
      "This script was tested on chromote 0.5.1, which provides these.\n",
      "Try updating chromote:  install.packages('chromote')",
      call. = FALSE
    )
  }
  if (!"new_session" %in% names(chromote::Chromote$public_methods)) {
    stop(
      "Your installed chromote (version ", installed_ver, ") does not expose ",
      "Chromote$new_session(), which this script needs.\n",
      "This script was tested on chromote 0.5.1. Try: install.packages('chromote')",
      call. = FALSE
    )
  }
  if (utils::compareVersion(installed_ver, "0.5.1") != 0) {
    message(
      "Note: this script was tested on chromote 0.5.1; you have ",
      installed_ver, ". It should still work, but if Chrome fails to open, ",
      "a version difference is the first thing to suspect."
    )
  }
  # -----------------------------------------------------------------------

  chrome_path <- find_chrome()
  if (is.null(chrome_path)) {
    stop("Could not find Google Chrome. Please install Chrome and try again.")
  }

  # If this R session already started a Chrome that wasn't closed cleanly, shut
  # it down first so we don't accumulate zombie browsers.
  if (!is.null(.gd_env$chrome_proc)) {
    try(.gd_env$chrome_proc$kill(), silent = TRUE)
    .gd_env$chrome_proc <- NULL
  }

  # Pick a free port automatically unless the caller forced one. A fresh port
  # each run prevents attaching to a leftover Chrome from a previous run (the
  # usual cause of "End of File" websocket errors).
  if (is.null(debug_port)) debug_port <- gd_find_free_port()

  # Glassdoor sits behind Cloudflare and can be slow to respond; give chromote
  # commands more time than the 10s default before they time out.
  options(chromote.timeout = 40)

  # Dedicated temporary profile -> forces a brand-new Chrome instance with the
  # debugging port open (otherwise Chrome may just hand off to an already-open
  # window and ignore our settings).
  user_data_dir <- file.path(tempdir(), paste0("gd_chrome_profile_", Sys.getpid()))
  dir.create(user_data_dir, showWarnings = FALSE, recursive = TRUE)

  message("Launching a visible Chrome window...")
  chrome_args <- c(
    paste0("--remote-debugging-port=", debug_port),
    "--remote-allow-origins=*",
    paste0("--user-data-dir=", user_data_dir),
    "--no-first-run",
    "--no-default-browser-check",
    # Present Chrome in the market's language (helps language; does NOT by
    # itself defeat the IP-based country redirect — see proxy note above).
    paste0("--lang=", market$chrome_lang)
  )
  if (!is.null(proxy)) {
    message("Routing Chrome through proxy: ", proxy)
    chrome_args <- c(chrome_args, paste0("--proxy-server=", proxy))
  }
  chrome_proc <- processx::process$new(
    command = chrome_path,
    args = chrome_args,
    supervise = TRUE
  )

  # Wait for Chrome's remote-debugging endpoint to come up.
  ready <- FALSE
  deadline <- Sys.time() + 20
  while (Sys.time() < deadline) {
    ok <- tryCatch({
      con <- url(sprintf("http://127.0.0.1:%d/json/version", debug_port), "rb")
      on.exit(try(close(con), silent = TRUE), add = TRUE)
      readLines(con, warn = FALSE)
      TRUE
    }, error = function(e) FALSE, warning = function(e) FALSE)
    if (isTRUE(ok)) { ready <- TRUE; break }
    Sys.sleep(0.2)
  }
  if (!ready) {
    try(chrome_proc$kill(), silent = TRUE)
    stop("Chrome started but its debugging port never became available.")
  }

  # Attach chromote to the visible Chrome window and open a controllable tab.
  remote  <- ChromeRemote$new(host = "127.0.0.1", port = debug_port)
  chr     <- Chromote$new(browser = remote)
  session <- chr$new_session()

  # Send the market's Accept-Language header on every request (nudges Glassdoor
  # to serve the right language). Does not override the IP-based country redirect.
  try({
    session$Network$enable()
    session$Network$setExtraHTTPHeaders(
      headers = list("Accept-Language" = market$accept_lang)
    )
  }, silent = TRUE)

  # Remember the market on the session so later calls inherit it automatically.
  .gd_env$market <- market

  # Remember these so gd_close() can shut everything down cleanly.
  .gd_env$chrome_proc <- chrome_proc
  .gd_env$chromote    <- chr
  .gd_env$user_data   <- user_data_dir

  message("Chrome is ready.")
  session
}


# -----------------------------------------------------------------------------
# gd_close
#
# Shuts down the browser window and cleans up. Call this when you are done.
# -----------------------------------------------------------------------------
gd_close <- function(session) {
  try(session$close(), silent = TRUE)
  if (!is.null(.gd_env$chromote))    try(.gd_env$chromote$close(), silent = TRUE)
  if (!is.null(.gd_env$chrome_proc)) try(.gd_env$chrome_proc$kill(), silent = TRUE)
  if (!is.null(.gd_env$user_data))   try(unlink(.gd_env$user_data, recursive = TRUE), silent = TRUE)
  .gd_env$chrome_proc <- NULL
  .gd_env$chromote    <- NULL
  message("Browser closed.")
  invisible(NULL)
}


# -----------------------------------------------------------------------------
# gd_kill_leftover_chrome
#
# Emergency cleanup. If a previous run crashed and left a scraper Chrome window
# open (causing "End of File" / websocket errors on the next run), call this
# once to kill any Chrome started by this scraper. It only targets Chrome
# instances launched with our temporary profile, so it will NOT close your
# normal everyday Chrome windows.
# -----------------------------------------------------------------------------
gd_kill_leftover_chrome <- function() {
  if (.Platform$OS.type == "windows") {
    # Best-effort on Windows: match our profile path in the command line.
    try(system2("taskkill", c("/F", "/IM", "chrome.exe", "/FI",
                              shQuote("WINDOWTITLE eq gd_chrome_profile*"))),
        silent = TRUE)
  } else {
    # macOS / Linux: kill only Chrome processes using our temp profile.
    try(system("pkill -f 'gd_chrome_profile_'"), silent = TRUE)
  }
  .gd_env$chrome_proc <- NULL
  .gd_env$chromote    <- NULL
  message("Any leftover scraper Chrome windows have been closed.")
  invisible(NULL)
}


# -----------------------------------------------------------------------------
# gd_wait_for_login
#
# Opens the Glassdoor login page in the browser and waits for YOU to log in
# manually. This works with any login method: email/password, Google, Facebook,
# Apple, etc. — whatever your account uses.
#
# Steps:
#   1. A Chrome window opens at the Glassdoor login page.
#   2. Log in however you normally would (including Google OAuth if needed).
#   3. Once you can see Glassdoor's home page, come back to R and press Enter.
#
# The scraper then uses your authenticated session automatically.
# -----------------------------------------------------------------------------
gd_wait_for_login <- function(session, market = .gd_env$market %||% GD_MARKET_US) {
  message("Opening Glassdoor (", market$key, ") login page in Chrome...")
  session$Page$navigate(market$login_url)
  Sys.sleep(PAGE_LOAD_WAIT)

  message(
    "\n",
    "=================================================================\n",
    " ACTION REQUIRED\n",
    " A Chrome window has opened on the Glassdoor ", market$key, " site.\n",
    " If you are not on a login screen, click the site's\n",
    " sign-in / 'Anmelden' button first.\n",
    " Log in using whichever method your account uses\n",
    " (email/password, Google, etc.).\n",
    " Complete any CAPTCHA or 2FA if prompted.\n",
    " When you can see the Glassdoor home page, return here\n",
    " and press Enter to continue.\n",
    "================================================================="
  )

  readline(prompt = "Press Enter once you are logged in > ")

  # Verify we look logged in by checking for a sign-out link
  logged_in <- session$Runtime$evaluate(
    'document.querySelector("[data-test=\'header-signin-link\']") === null'
  )$result$value

  if (isTRUE(logged_in)) {
    message("Login confirmed. Starting scrape...")
  } else {
    message(
      "Warning: could not confirm login — the login link is still visible.\n",
      "Continuing anyway, but check the browser if you get no results."
    )
  }

  invisible(session)
}


# -----------------------------------------------------------------------------
# gd_reviews_url
#
# Builds a Glassdoor reviews URL for a given company, for the chosen market.
# employer_id and employer_name come from the Glassdoor URL when you visit a
# company's reviews page manually, e.g.:
#   US: https://www.glassdoor.com/Reviews/Apple-Reviews-E1138.htm
#   DE: https://de.glassdoor.ch/Bewertungen/Apple-Bewertungen-E1138.htm
#                                               name^       seg^   id^
# -----------------------------------------------------------------------------
gd_reviews_url <- function(employer_name, employer_id, page = 1,
                           market = GD_MARKET_US) {
  seg  <- market$rev_segment
  base <- sprintf(
    "%s/%s/%s-%s-E%s.htm",
    market$base_url, seg, employer_name, seg, employer_id
  )
  if (page > 1) {
    base <- sprintf(
      "%s/%s/%s-%s-E%s_P%s.htm",
      market$base_url, seg, employer_name, seg, employer_id, page
    )
  }
  base
}


# -----------------------------------------------------------------------------
# gd_debug_page
#
# Diagnostic helper. Navigate to a reviews page (while logged in), then call
# this to (a) save the page's full HTML to a file and (b) print a summary of
# what selectors / data are present. Use this when no reviews are collected so
# the actual page structure can be inspected and the parser adjusted.
#
# Usage (after gd_wait_for_login):
#   session$Page$navigate(gd_reviews_url("Apple", "1138", 1, GD_MARKET_DE))
#   Sys.sleep(4)
#   gd_debug_page(session)
# -----------------------------------------------------------------------------
gd_debug_page <- function(session, out_dir = "output") {
  if (!dir.exists(out_dir)){
    dir.create(out_dir, recursive = TRUE)
    warning(paste('created output directory in ', getwd()))
  }
  html_path <- file.path(out_dir, "debug_page.html")

  html <- session$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value
  writeLines(html, html_path, useBytes = TRUE)

  info <- session$Runtime$evaluate('
    (function() {
      var count = function(sel) { try { return document.querySelectorAll(sel).length; } catch(e){ return -1; } };
      var scripts = Array.prototype.map.call(
        document.querySelectorAll("script[type=\'application/json\'], script[id]"),
        function(s){ return (s.id||"(no id)") + " [" + (s.type||"") + "] len=" + (s.textContent||"").length; }
      );
      return JSON.stringify({
        url: location.href,
        title: document.title,
        signin_link: count("[data-test=\'header-signin-link\']"),
        li_empReview: count("li[id^=\'empReview\']"),
        reviewsList: count("[data-test=\'reviewsList\']"),
        data_test_pros: count("[data-test=\'pros\']"),
        any_pros_text: (document.body.innerText.indexOf("Pros") >= 0) || (document.body.innerText.indexOf("Gut am") >= 0),
        json_scripts: scripts.slice(0, 20)
      });
    })()
  ')$result$value

  parsed <- tryCatch(fromJSON(info), error = function(e) NULL)
  message("\n==== PAGE DIAGNOSTICS ====")
  if (!is.null(parsed)) {
    message("URL:           ", parsed$url)
    message("Title:         ", parsed$title)
    message("Signin link:   ", parsed$signin_link, "  (0 = logged in; >0 = NOT logged in)")
    message("li#empReview*: ", parsed$li_empReview, "  (review cards found by old selector)")
    message("reviewsList:   ", parsed$reviewsList)
    message("data-test pros:", parsed$data_test_pros)
    message("Pros text seen:", parsed$any_pros_text)
    message("JSON script tags on page:")
    for (s in parsed$json_scripts) message("   - ", s)
  } else {
    message("Could not parse diagnostics; raw: ", info)
  }
  message("Full HTML saved to: ", normalizePath(html_path))
  message("==========================\n")
  invisible(html_path)
}


# -----------------------------------------------------------------------------
# gd_parse_reviews_from_page
#
# Extracts review data from the current page in the browser session.
# Returns a data frame with one row per review, or NULL if none found.
# -----------------------------------------------------------------------------
gd_parse_reviews_from_page <- function(session) {
  # Selectors match Glassdoor's current layout (verified June 2026). Each review
  # is an <article data-test="review-detail">. If Glassdoor redesigns again and
  # this returns nothing, use gd_debug_page() to capture the new structure.
  result <- session$Runtime$evaluate(
    expression = '
      (function() {
        var reviews = [];
        var cards = document.querySelectorAll("article[data-test=\'review-detail\']");
        cards.forEach(function(card) {
          var getText = function(sel) {
            var el = card.querySelector(sel);
            return el ? el.innerText.trim() : null;
          };

          // Review id lives in the data-brandviews attribute, e.g.
          // "MODULE:n=employee-reviews:eid=1138:review_id=104103486"
          var bv = card.getAttribute("data-brandviews") || "";
          var idm = bv.match(/review_id=(\\d+)/);
          var review_id = idm ? idm[1] : null;

          // The reviewer tags hold employment status and (optionally) location.
          // The location tag is the one wrapped in a location link (href _IL.).
          var emp = null, loc = null;
          var tags = card.querySelectorAll("[data-test=\'content-avatar-tag\']");
          tags.forEach(function(t) {
            var a = t.closest("a");
            if (a && /_IL\\./.test(a.getAttribute("href") || "")) {
              loc = t.innerText.trim();
            } else if (emp === null) {
              emp = t.innerText.trim();
            }
          });

          // The three sentiment icons (Recommend / CEO approval / Business
          // outlook) always render together in this order. Their sentiment is
          // encoded in the CSS class, e.g. "ExperienceRating_positive__...".
          // This is language-independent, so it works on both the US and DE
          // sites. Value is "positive", "negative", "neutral" or "none".
          var sentiment = function(el) {
            var c = el ? (el.className || "") : "";
            var m = c.match(/ExperienceRating_(positive|negative|neutral|mixed)/);
            return el ? (m ? m[1] : "none") : null;
          };
          var exp = card.querySelectorAll("[class*=\'ExperienceRating_container\']");

          reviews.push({
            review_id:         review_id,
            date:              getText("[class*=\'reviewDate\']"),
            job_title:         getText("[data-test=\'content-avatar-label\']"),
            location:          loc,
            employment_status: emp,
            rating:            getText("[data-test=\'review-rating-label\']"),
            headline:          getText("h3"),
            pros:              getText("[data-test=\'review-text-PROS\']"),
            cons:              getText("[data-test=\'review-text-CONS\']"),
            recommend:         sentiment(exp[0]),
            ceo_approval:      sentiment(exp[1]),
            business_outlook:  sentiment(exp[2])
          });
        });
        return JSON.stringify(reviews);
      })()
    '
  )

  raw <- result$result$value
  if (is.null(raw) || raw == "[]") return(NULL)

  parsed <- fromJSON(raw, simplifyDataFrame = TRUE)
  as_tibble(parsed)
}


# -----------------------------------------------------------------------------
# gd_scrape_company
#
# Main entry point. Scrapes reviews for one company across multiple pages.
#
# Args:
#   session       : chromote session from gd_start_session()
#   employer_name : name slug from Glassdoor URL (e.g. "Apple")
#   employer_id   : numeric id from Glassdoor URL (e.g. "1138")
#   max_pages     : maximum pages to collect (10 reviews/page). NULL = all.
#   market        : GD_MARKET_US or GD_MARKET_DE (defaults to the one used to
#                   start the session).
#
# Returns a tidy data frame of all reviews collected.
# -----------------------------------------------------------------------------
gd_scrape_company <- function(session, employer_name, employer_id,
                               max_pages = 5,
                               market = .gd_env$market %||% GD_MARKET_US) {
  all_reviews <- list()
  page <- 1

  repeat {
    if (!is.null(max_pages) && page > max_pages) break

    url <- gd_reviews_url(employer_name, employer_id, page, market = market)
    message(sprintf("  Fetching page %d: %s", page, url))

    session$Page$navigate(url)
    Sys.sleep(PAGE_LOAD_WAIT)

    reviews <- gd_parse_reviews_from_page(session)

    if (is.null(reviews) || nrow(reviews) == 0) {
      message("  No reviews found on this page — stopping.")
      break
    }

    reviews$page <- page
    reviews$employer_name <- employer_name
    reviews$employer_id <- employer_id
    reviews$market <- market$key
    all_reviews[[page]] <- reviews

    message(sprintf("  Collected %d reviews (page %d)", nrow(reviews), page))

    # Check for a next page before continuing. The <link rel="next"> tag is the
    # most reliable signal; fall back to the pagination button if absent.
    has_next <- session$Runtime$evaluate(
      'document.querySelector("link[rel=\'next\']") !== null ||
       (function(){ var b = document.querySelector("[data-test=\'next-page\']");
                    return b !== null && !b.disabled; })()'
    )$result$value

    if (!isTRUE(has_next)) {
      message("  No next page — done.")
      break
    }

    Sys.sleep(PAUSE_BETWEEN_PAGES)
    page <- page + 1
  }

  if (length(all_reviews) == 0) {
    message("No reviews collected. Check login status and employer_name/id.")
    return(tibble())
  }

  bind_rows(all_reviews)
}


# -----------------------------------------------------------------------------
# gd_save
#
# Saves the reviews data frame to a CSV and an RDS file.
# -----------------------------------------------------------------------------
gd_save <- function(reviews_df, out_dir = ".", prefix = "glassdoor_reviews") {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  csv_path  <- file.path(out_dir, sprintf("%s_%s.csv",  prefix, timestamp))
  rds_path  <- file.path(out_dir, sprintf("%s_%s.rds",  prefix, timestamp))
  write.csv(reviews_df, csv_path, row.names = FALSE)
  saveRDS(reviews_df,   rds_path)
  message(sprintf("Saved %d reviews to:\n  %s\n  %s", nrow(reviews_df), csv_path, rds_path))
  invisible(list(csv = csv_path, rds = rds_path))
}
