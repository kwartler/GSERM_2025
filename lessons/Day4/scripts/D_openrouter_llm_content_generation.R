#' Title: Intro: LLM Content Generation
#' Purpose: Content Generation using OpenRouter cloud API
#' Author: Ted Kwartler 
#' June 4, 2026

# Libraries
library(httr)
library(jsonlite)

# Inputs
promptA <- 'Write a movie review as a 5 year old for Star Wars'
promptB <- 'Write a movie review as a 95 year old, senior citizen for Star Wars'
promptC <- 'Write a movie review of star wars as a pirate.'
promptD <- 'Write a movie review of Star Wars as Taylor Swift.'

# OpenRouter Specific Inputs
llmModel       <- "google/gemini-3.1-flash-lite"
openrouter_key <- Sys.getenv("OPENROUTER_API_KEY")

if (openrouter_key == "") {
  stop("API Key missing! Please set OPENROUTER_API_KEY in your .Renviron file.")
}

# Consolidated Headers
headers <- c(
  `Content-Type`  = "application/json",
  `Authorization` = paste("Bearer", openrouter_key),
  `HTTP-Referer`  = "http://localhost",
  `X-Title`       = "R Persona Script"
)

# Helper Function to reduce code duplication for students
query_openrouter <- function(user_prompt, model_name, api_headers) {
  # Organize Request
  dataLLM <- list(
    model = model_name,
    messages = list(
      list(role = "system", content = "You are a helpful, smart, kind, and efficient AI assistant. You always fulfill the user's requests to the best of your ability."),
      list(role = "user", content = user_prompt)
    ),
    temperature = 0.7,
    max_tokens = 512,
    stream = FALSE
  )
  
  # Make the POST request
  res <- httr::POST(url = "https://openrouter.ai/api/v1/chat/completions",
                    httr::add_headers(.headers = api_headers),
                    body = toJSON(dataLLM, auto_unbox = TRUE))
  
  # Extract and return the text response
  response_text <- httr::content(res)$choices[[1]]$message$content
  return(response_text)
}


# Execute Request A
llmResponseA <- query_openrouter(promptA, llmModel, headers)
cat("--- 5 Year Old Review ---\n", llmResponseA, "\n\n")

# Execute Request B
llmResponseB <- query_openrouter(promptB, llmModel, headers)
cat("--- 95 Year Old Review ---\n", llmResponseB, "\n\n")

# Execute Request C
llmResponseC <- query_openrouter(promptC, llmModel, headers)
cat("--- Pirate Review ---\n", llmResponseC, "\n\n")

# Execute Request D
llmResponseD <- query_openrouter(promptD, llmModel, headers)
cat("--- Taylor Swift Review ---\n", llmResponseD, "\n\n")

# End