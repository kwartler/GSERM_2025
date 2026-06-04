#' Title: Intro: LLM Batch Processing & Retries
#' Purpose: Concurrent LLM Inference with Automatic Backoff Retry using httr2
#' Author: Ted Kwartler

# Libraries
library(httr2)
library(jsonlite)

# 1. Inputs & Configuration
prompts <- c(
  "Summarize the main discovery of the James Webb Space Telescope in 1 sentence.",
  "Explain quantum computing to a 10-year-old in 1 sentence.",
  "What is the capital of Switzerland? Respond in 1 word.",
  "Why is the sky blue? Respond in 1 sentence."
)

llmModel       <- "google/gemini-3.1-flash-lite"
openrouterKey <- Sys.getenv("OPENROUTER_API_KEY")

if (openrouterKey == "") {
  stop("API Key missing! Please set OPENROUTER_API_KEY in your .Renviron file.")
}

# ------------------------------------------------------------------------------
# 2. Build the Base Request Template
# ------------------------------------------------------------------------------
base_req <- request("https://openrouter.ai/api/v1/chat/completions") %>%
  req_headers(
    `Content-Type`  = "application/json",
    `Authorization` = paste("Bearer", openrouterKey),
    `HTTP-Referer`  = "http://localhost",
    `X-Title`       = "R Concurrent Batch Script"
  ) %>%
  # RETRY LOGIC: Retry up to 3 times if server returns 429 (Rate Limit) or 5xx errors
  # It automatically uses exponential backoff (waits longer between each try)
  req_retry(max_tries = 3, backoff = ~ 2^.x)

# ------------------------------------------------------------------------------
# 3. Generate a List of Custom Requests (The Batch payload)
# ------------------------------------------------------------------------------
req_list <- lapply(prompts, function(p) {
  
  # Structural API body per prompt
  body_payload <- list(
    model = llmModel,
    messages = list(
      list(role = "system", content = "You are a concise, exact AI assistant."),
      list(role = "user", content = p)
    ),
    temperature = 0.3,
    max_tokens = 150,
    stream = FALSE
  )
  
  # Pipe the unique body into the request template
  base_req %>% req_body_json(body_payload)
})

# ------------------------------------------------------------------------------
# 4. Execute Concurrently (Batch Mode)
# ------------------------------------------------------------------------------
cat("Sending", length(req_list), "requests concurrently via httr2...\n")

# req_perform_parallel executes requests simultaneously using curl multi-handling
responses <- req_perform_parallel(req_list, on_error = "continue")

# ------------------------------------------------------------------------------
# 5. Extract and Process the Output
# ------------------------------------------------------------------------------
final_results <- lapply(seq_along(responses), function(i) {
  res <- responses[[i]]
  
  # Check if the individual request succeeded
  if (inherits(res, "error") || resp_status(res) != 200) {
    return(data.frame(
      prompt = prompts[i],
      response = "ERROR: Request failed or timed out."
    ))
  }
  
  # Cleanly parse the JSON body using httr2 built-ins
  res_body <- resp_body_json(res)
  clean_text <- res_body$choices[[1]]$message$content
  
  return(data.frame(
    prompt = prompts[i],
    response = trimws(clean_text),
    stringsAsFactors = FALSE
  ))
})

# Bind into a structured dataframe
results_df <- do.call(rbind, final_results)
print(results_df)

# End