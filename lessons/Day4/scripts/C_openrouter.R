#' Author: Ted Kwartler 
#' An example for students using OpenRouter.ai cloud API for model calls.
#' June 4, 2026

# Libraries
library(httr)
library(jsonlite)

# Inputs
prompt   <- "What is the capital of Brazil?"  
#prompt   <- "How many words are in your response to this message?  Use only numbers." 

# Example reading in a prompt
#prompt <- paste(readLines('https://raw.githubusercontent.com/kwartler/GSERM_2025/refs/heads/main/lessons/Day4/scripts/C_prompt_examples/B_costar_prompt.txt'), collapse = ' ')

llmModel <- "meta-llama/llama-3.2-1b-instruct"
#'openai/gpt-4o-mini-search-preview'
# "meta-llama/llama-3.2-1b-instruct"
#"openai/gpt-5-chat"
#"google/gemini-3.1-flash-lite"

# Retrieve API Key securely from the environment
openrouterKey <- Sys.getenv("OPENROUTER_API_KEY")

# Check if the key is missing to prevent silent failures
if (openrouterKey == "") {
  stop("API Key missing! Please set OPENROUTER_API_KEY in your .Renviron file.")
}

# Organize Request
dataLLM <- list(model = llmModel,
                messages = list(
                  list(role = "system", content = "You are a helpful, smart, kind, and efficient AI assistant. You always fulfill the user's requests to the best of your ability."),
                  list(role = "user", content = prompt)),
                temperature = 2,
                max_tokens = 512*2,
                stream = FALSE)

# Request header (OpenRouter requires Authorization, and recommends Referer/Title)
headers <- c(
  `Content-Type`  = "application/json",
  `Authorization` = paste("Bearer", openrouterKey),
  `HTTP-Referer`  = "http://localhost", 
  `X-Title`       = "R Student Script"   # Shows up in your OpenRouter dashboard
)

# Make the POST request to OpenRouter's endpoint
res <- httr::POST(url = "https://openrouter.ai/api/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = toJSON(dataLLM, auto_unbox = TRUE))

# Extract the response
llmResponse <- httr::content(res)$choices[[1]]$message$content
cat(llmResponse)

# End