#' Author: Ted Kwartler 
#' Description: An R script to perform text summarization using the OpenRouter cloud API

# Libraries
library(httr)
library(jsonlite)
library(stringdist) # has stringsim() 

# Inputs
articleTxt <- readLines('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/Summarization%20News%20Article%20-%20Playing%20%E2%80%98whack-a-mole%E2%80%99%20with%20Meta%20over%20my%20fraudulent%20avatars.txt')
articleTxt <- paste(articleTxt, collapse = ' ')

# OpenRouter Specific Key Retrieval
openrouter_key <- Sys.getenv("OPENROUTER_API_KEY")

if (openrouter_key == "") {
  stop("API Key missing! Please set OPENROUTER_API_KEY in your .Renviron file.")
}

# Request header specifies the content type and OpenRouter Authorization
headers <- c(
  `Content-Type`  = "application/json",
  `Authorization` = paste("Bearer", openrouter_key),
  `HTTP-Referer`  = "http://localhost",
  `X-Title`       = "R Summarization Evaluation Script"
)

# ------------------------------------------------------------------------------
# Model 1: "google/gemini-3.1-flash-lite"
# ------------------------------------------------------------------------------
llmModel <- "google/gemini-3.1-flash-lite"

# Organize the request payload for the LLM API
dataLLM <- list(
  model = llmModel,
  messages = list(
    # System message defines the AI's persona
    list(role = "system", content = "You are a helpful, smart, kind, and efficient AI assistant. You always fulfill the user's requests to the best of your ability.Summarize the following text in approximately 3 sentences, focusing on the main concepts.  Do not produce more than 3 sentences in your response.  Here is the text to summarize:\n"),
    list(role = "user", content = articleTxt)),
  temperature = 0.7, 
  max_tokens = 256,  
  stream = FALSE)

# Make the POST request to the OpenRouter API endpoint
res <- httr::POST(url = "https://openrouter.ai/api/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = toJSON(dataLLM, auto_unbox = TRUE))

# Extract the generated summary from the response
llmResponse <- httr::content(res)$choices[[1]]$message$content
cat("--- Gemini Response ---\n", llmResponse, "\n\n")

# Calculate the summarization quality metric using stringsim with Jaccard method
jaccardSim <- stringsim(articleTxt, llmResponse, method = 'jaccard', q = 1) #q-grams
cat("Gemini 3.5 Jaccard Similarity:", jaccardSim, "\n\n")


# ------------------------------------------------------------------------------
# Model 2: mistralai/ministral-3b-2512 for Comparison
# ------------------------------------------------------------------------------
llmModel <- "mistralai/ministral-3b-2512"

dataLLM <- list(
  model = llmModel,
  messages = list(
    # System message defines the AI's persona
    list(role = "system", content = "You are a helpful, smart, kind, and efficient AI assistant. You always fulfill the user's requests to the best of your ability.Summarize the following text in approximately 3 sentences, focusing on the main concepts.  Do not produce more than 3 sentences in your response.  Here is the text to summarize:\n"),
    # User message contains the summarization instruction and the text to summarize
    list(role = "user", content = articleTxt)),
  temperature = 1.2, 
  max_tokens = 256,  
  stream = FALSE)

# Make the POST request to the OpenRouter API endpoint
res <- httr::POST(url = "https://openrouter.ai/api/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = toJSON(dataLLM, auto_unbox = TRUE))

# Extract the generated summary from the response
llmResponse2 <- httr::content(res)$choices[[1]]$message$content
cat("--- Ministal 3B Response ---\n", llmResponse2, "\n\n")

# The bigger model may score lower because it followed directions and used the right number of sentences. 
# So model choice and capability matter.
jaccardSim2 <- stringsim(articleTxt, llmResponse2, method = 'jaccard', q = 1)
cat("ministral 3B Jaccard Similarity:", jaccardSim2, "\n")

# End