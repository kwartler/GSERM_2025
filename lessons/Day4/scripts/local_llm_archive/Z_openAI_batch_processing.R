# Load necessary libraries
# install.packages(c("httr", "jsonlite", "readr", "dplyr", "future", "furrr")) # Uncomment and run if you don't have them
library(httr)   # Changed from httr2 to httr
library(jsonlite)
library(readr)
library(dplyr)
library(future) # For setting up parallel backend
library(furrr)  # For parallel mapping functions

# --- Configuration ---
# Set your OpenAI API key. It's best practice to load this from an environment variable.
# You can set it using Sys.setenv(OPENAI_API_KEY = "YOUR_API_KEY") in your .Renviron file
# or directly in your R session for testing.
OPENAI_API_KEY <- 'sk-'

if (OPENAI_API_KEY == "") {
  stop("OpenAI API key not found. Please set the 'OPENAI_API_KEY' environment variable.")
}

# Define the model to use
MODEL_NAME <- "gpt-4o-mini"
DEFAULT_TEMPERATURE <- 1 # Added default temperature
DEFAULT_MAX_TOKENS <- 8192 # Added default max_tokens

# --- 1. Prepare your vector of prompts ---
allWorkDF <- read.csv('some_file.csv')
prompts <- allWorkDF$prompt # column of text to process is $prompt

# Append the instructions
prompts <- paste0('Please review the following text.  Extract all named people that are mentioned in the text separated by a comma. \n\nFor example: \n\nText to be analyzed:\nThe two students were learning about LLMs.  Jack was learning about vector embeddings.  Jill was learning about named entity extraction.\n\nExtracted Names: Jack, Jill\n\nDo not add any commentary to your analysis.  Do not provide anything other than the names separated by the commas.  \nHere is the text to analyze:\n', prompts)

# --- 2. Function to make a single OpenAI Chat Completion API call ---
# This function handles the request, response parsing, and basic error handling.
# It includes a retry mechanism for transient errors (like rate limits) and HTTP 5xx errors.
call_openai_chat <- function(prompt, model_name, api_key, temperature = DEFAULT_TEMPERATURE, max_tokens = DEFAULT_MAX_TOKENS) {
  # Define API endpoint
  api_url <- "https://api.openai.com/v1/chat/completions"
  
  # Define headers
  headers <- add_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )
  
  # Define request body
  body_list <- list(
    model = model_name,
    messages = list(list(role = "user", content = prompt)),
    temperature = temperature,  # Added temperature parameter
    max_tokens = max_tokens    # Added max_tokens parameter
  )
  
  max_tries <- 5 # Maximum number of retry attempts
  for (attempt in 1:max_tries) {
    tryCatch({
      # Send POST request using httr
      response <- POST(url = api_url,
                       config = headers,
                       body = body_list,
                       encode = "json")
      
      # Check for successful response status code
      if (response$status_code == 200) {
        response_body <- content(response, "parsed")
        if (!is.null(response_body$choices) && length(response_body$choices) > 0 && !is.null(response_body$choices[[1]]$message$content)) {
          return(list(status = "success", prompt = prompt, response = response_body$choices[[1]]$message$content))
        } else {
          error_message <- "No content or choices returned from API."
          if (!is.null(response_body$error)) {
            error_message <- paste(error_message, "Details:", jsonlite::toJSON(response_body$error, auto_unbox = TRUE))
          }
          return(list(status = "error", prompt = prompt, response = NA, error = error_message))
        }
      } else if (response$status_code == 429 || response$status_code >= 500) {
        # Transient error, retry after backoff
        if (attempt < max_tries) {
          sleep_time <- 2^(attempt - 1) + runif(1, 0, 1) # Exponential backoff with jitter
          message(paste("Retrying after error (Status:", response$status_code, "):", content(response, "text"), "for prompt:", substr(prompt, 1, 50), "... Sleeping for", round(sleep_time, 2), "seconds."))
          Sys.sleep(sleep_time)
        } else {
          # Max retries reached
          error_message <- paste("API call failed after", max_tries, "attempts (Status:", response$status_code, "):", content(response, "text"))
          return(list(status = "error", prompt = prompt, response = NA, error = error_message))
        }
      } else {
        # Non-retriable HTTP error
        error_message <- paste("API call failed (Status:", response$status_code, "):", content(response, "text"))
        return(list(status = "error", prompt = prompt, response = NA, error = error_message))
      }
    }, error = function(e) {
      # Catch R-level errors (e.g., network issues before receiving HTTP status)
      if (attempt < max_tries) {
        sleep_time <- 2^(attempt - 1) + runif(1, 0, 1) # Exponential backoff with jitter
        message(paste("Retrying after R error:", e$message, "for prompt:", substr(prompt, 1, 50), "... Sleeping for", round(sleep_time, 2), "seconds."))
        Sys.sleep(sleep_time)
      } else {
        return(list(status = "error", prompt = prompt, response = NA, error = paste("API call failed after", max_tries, "attempts due to R error:", e$message)))
      }
    })
  }
  # This part should ideally not be reached if max_tries is handled correctly
  return(list(status = "error", prompt = prompt, response = NA, error = "Unexpected error: Function exited retry loop without success or definitive error."))
}


# --- 3. Set up parallel processing plan ---
# Use 'multisession' for parallel execution on your local machine across multiple CPU cores.
# Adjust 'workers' based on your system's CPU count and available memory.
# Be mindful that too many parallel requests can still hit OpenAI's rate limits,
# even with the retry mechanism. Start with a moderate number (e.g., 4-8).
plan(multisession, workers = parallel::detectCores() - 1) # Use one less than total cores

cat("Starting parallel API calls...\n")

# --- 4. Execute API calls in parallel using furrr ---
# furrr::future_map applies the call_openai_chat function to each prompt in parallel.
# This is much faster than a traditional loop for a large number of prompts.
#all_results <- furrr::future_map(tmpListDF$prompt, call_openai_chat, model_name = MODEL_NAME, api_key = OPENAI_API_KEY,
#                                 temperature = DEFAULT_TEMPERATURE, max_tokens = DEFAULT_MAX_TOKENS)

st <- Sys.time()
all_results <- furrr::future_map(prompts, call_openai_chat,
                                 model_name = MODEL_NAME,
                                 api_key = OPENAI_API_KEY,
                                 .progress = TRUE)
en <- Sys.time()
en-st
tmpAllResults <- list()
for(i in 1:length(all_results)){
  x <- all_results[[i]]
  if(x$status=='error'){
    x <- data.frame(status = x$status,
                    prompt = 'NA',
                    response = 'NA')
  } else {
    x <- as.data.frame(x)
  }
  x$idx <- i
  tmpAllResults[[i]] <- x
}
tmpAllResultsDF <- do.call(rbind, tmpAllResults)


# Append the analysis back to the original data
finalDf <- cbind(allWorkDF, tmpAllResultsDF)

# End