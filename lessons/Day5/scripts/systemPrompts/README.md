# Prompt Management: Agentic Workflow

`E_promptChainExample.R` runs a four-stage agent **prompt chain**. Each stage has
its own system prompt, stored as a plain text file and read in at run time. The
chain ends by writing a runnable single-page web app, `index.html`, to disk.

The chain runs in this order. Note the programmer goes **last** and synthesizes
everyone else's work into the final artifact (a spec-first / test-first pipeline):

| Order | File | Stage | Produces |
|-------|------|-------|----------|
| 1 | `01_productOwner.txt`    | Product Owner    | Functional specification |
| 2 | `02_qcTestDesigner.txt`  | QC / Test Designer | Acceptance + regression checklist |
| 3 | `03_technicalWriter.txt` | Technical Writer | End-user documentation |
| 4 | `04_programmer.txt`      | Programmer       | The final `index.html` (saved) |

## Two prompt sets: the whole point

There are **two complete copies** of these prompts:

- **`v1_naive/`**: vague, underspecified prompts with no output contract.
- **`v2_engineered/`**: precise prompts with explicit output contracts,
  required field names, the exact API request shape, and error handling.

The script picks one with a single variable:

```r
promptSet <- "v2_engineered"   # flip to "v1_naive" to A/B test
```

## The exercise

1. Run the script with `promptSet <- "v2_engineered"`. Open the saved
   `index.html` by double-clicking it. Paste your OpenRouter key, pick a model,
   type a prompt, hit Send, and it works. **An LLM assembly line just built you a
   working LLM app.**
2. Now flip to `promptSet <- "v1_naive"` and re-run. Open that `index.html`.
   It is usually broken: stray ```` ``` ```` fences left in the file, missing
   form fields, no error handling, sometimes it will not open at all.
3. **Run the QC checklist (stage 2) against both versions.** Treat that checklist
   as a fixed regression suite: same tests, different prompt set. Count the
   PASS/FAIL difference.

Same model, same chain, same code, and **only the system prompts changed.** That
gap is why system-prompt management and prompt regression testing matter.

## Why manage prompts as files?

- **Versioning**: prompts live in git; you can diff and review changes.
- **Reuse**: the same prompt is shared across scripts and teammates.
- **Separation of concerns**: edit wording without touching code logic.
- **Regression testing**: swap a set, re-run the fixed checklist, measure the
  difference. Prompts are code, and code needs tests.
