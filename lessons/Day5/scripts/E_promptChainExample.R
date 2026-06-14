#' Author: Ted Kwartler
#' June 10, 2026
#' An Agentic Workflow example using OpenRouter
#' A four-stage prompt chain that ends by BUILDING a runnable web app.
#'
#' Learning objectives:
#'   1. Prompt chaining     - each stage's output feeds the next stage.
#'   2. Prompt management    - system prompts live in external, versioned files.
#'   3. Regression testing   - swap the prompt SET and measure the difference
#'                             against the QC checklist. Same model, same chain,
#'                             only the system prompts change.
#' See systemPrompts/README.md for the exercise.

# Libraries
library(httr2)

# *** EDIT THESE *** Paths to YOUR copy of the GSERM_2025 repo.
functionFile <- "~/Desktop/GSERM_2025/openRouter_function.R"
promptPath   <- "~/Desktop/GSERM_2025/lessons/Day5/scripts/systemPrompts/"
savePath     <- "~/Desktop/GSERM_2025/personalFiles/"   # gitignored; index.html saves here

# Bring in the shared query_openrouter() helper
source(functionFile)

# *** THE A/B TOGGLE *** Flip between the two prompt sets and re-run to compare.
promptSet <- "v2_engineered"   # or "v1_naive" "v2_engineered"

# Input
llmModel <- "google/gemini-3.1-flash-lite"
#'openai/gpt-4o-mini-search-preview'
#"openai/gpt-5-chat"

# Generation settings (a full HTML file needs room, so allow more tokens)
temp      <- 0.7
maxTokens <- 2048

# Helper: read a system prompt for the CURRENT prompt set
readPrompt <- function(fileName) {
  paste(readLines(file.path(promptPath, promptSet, fileName), warn = FALSE),
        collapse = "\n")
}

# System prompts: one per stage, from the selected set (see systemPrompts/README.md)
productOwnerSystem    <- readPrompt("01_productOwner.txt")
qcTestDesignerSystem  <- readPrompt("02_qcTestDesigner.txt")
technicalWriterSystem <- readPrompt("03_technicalWriter.txt")
programmerSystem      <- readPrompt("04_programmer.txt")

# The one brief that kicks off the chain (the request to the product owner)
projectBrief <- "Build a single-page web application: a self-contained index.html chat client for the OpenRouter API. The user pastes their own OpenRouter API key, picks a model, types a prompt, clicks Send, and sees the model's response on the page."

# STEP 1: Product Owner -> functional specification
functionalSpec <- query_openrouter(prompt = projectBrief,
                                    system_prompt = productOwnerSystem,
                                    model = llmModel, temperature = temp,
                                    max_tokens = maxTokens)

# STEP 2: QC / Test Designer -> acceptance + regression checklist (from the spec)
qcChecklist <- query_openrouter(prompt = functionalSpec,
                                system_prompt = qcTestDesignerSystem,
                                model = llmModel, temperature = temp,
                                max_tokens = maxTokens)

# STEP 3: Technical Writer -> end-user docs (from the spec)
userDocs <- query_openrouter(prompt = functionalSpec,
                             system_prompt = technicalWriterSystem,
                             model = llmModel, temperature = temp,
                             max_tokens = maxTokens)

# STEP 4: Programmer -> the final index.html, synthesizing everyone's work
teamContext <- paste("## FUNCTIONAL SPECIFICATION\n", functionalSpec,
                     "\n\n## ACCEPTANCE / REGRESSION CHECKLIST\n", qcChecklist,
                     "\n\n## USER DOCUMENTATION\n", userDocs,
                     collapse = "\n")
appHTML <- query_openrouter(prompt = teamContext,
                            system_prompt = programmerSystem,
                            model = llmModel, temperature = temp,
                            max_tokens = maxTokens)

# Defensive cleanup: strip any stray Markdown code fences a weak prompt may leave
# in (the v1_naive set often does; the v2_engineered set should not). This is
# itself a teaching point - look at the raw output to see the difference.
cleanHTML <- gsub("```(html)?", "", appHTML)

# Save the runnable app. Double-click this file to try it in a browser.
indexFile <- file.path(savePath, "index.html")
writeLines(cleanHTML, indexFile)

# Show the whole team's work, then where the app landed
sapply(c(functionalSpec, qcChecklist, userDocs, appHTML), cat)
cat("\n\nSaved web app to:", normalizePath(indexFile), "\n")
cat("Prompt set used:", promptSet, "\n")

# End
