#' Author: Ted Kwartler (Modified by Gemini)
#' Date: June 23, 2025
#' Description: An R script to perform text summarization using a local LLM
#'              (via lm-studio) & apply one metric for summarization quality

# Libraries
library(httr)
library(jsonlite)
library(stringdist) # has stringsim() 

# Inputs
articleTxt <- readLines('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/Summarization%20News%20Article%20-%20Playing%20%E2%80%98whack-a-mole%E2%80%99%20with%20Meta%20over%20my%20fraudulent%20avatars.txt')
articleTxt <- paste(articleTxt, collapse = ' ')

# Specify the LLM model to use, assuming it's available in lm-studio
llmModel <- 'llama-3.2-1b-instruct'

# Organize the request payload for the LLM API
dataLLM <- list(
  model = llmModel,
  messages = list(
    # System message defines the AI's persona
    list(role = "system", content = "You are a helpful, smart, kind, and efficient AI assistant. You always fulfill the user's requests to the best of your ability.Summarize the following text in approximately 3 sentences, focusing on the main concepts.  Do not produce more than 3 sentences in your response.  Here is the text to summarize:\n"),
    # User message contains the summarization instruction and the text to summarize
    list(role = "user", content = articleTxt)),
  temperature = 0.7, 
  max_tokens = 256,  
  stream = FALSE)

# Request header specifies the content type as JSON
headers <- c(`Content-Type` = "application/json")

# Make the POST request to the local lm-studio API endpoint
res <- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = toJSON(dataLLM, auto_unbox = TRUE))

# Extract the generated summary from the response
llmResponse <- httr::content(res)$choices[[1]]$message$content
cat(llmResponse)

# Calculate the summarization quality metric using stringsim with Jaccard method
jaccardSim <- stringsim(articleTxt, llmResponse, method = 'jaccard', q = 1)
jaccardSim

# We can try a more sophisticated model for comparison.
llmModel <- 'qwen2.5-7b-instruct'
dataLLM <- list(
  model = llmModel,
  messages = list(
    # System message defines the AI's persona
    list(role = "system", content = "You are a helpful, smart, kind, and efficient AI assistant. You always fulfill the user's requests to the best of your ability.Summarize the following text in approximately 3 sentences, focusing on the main concepts.  Do not produce more than 3 sentences in your response.  Here is the text to summarize:\n"),
    # User message contains the summarization instruction and the text to summarize
    list(role = "user", content = articleTxt)),
  temperature = 0.7, 
  max_tokens = 256,  
  stream = FALSE)

# Make the POST request to the local lm-studio API endpoint
res <- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = toJSON(dataLLM, auto_unbox = TRUE))

# Extract the generated summary from the response
llmResponse2 <- httr::content(res)$choices[[1]]$message$content
cat(llmResponse2)

# The bigger model may score lower because it followed directions and used the right number of sentences.  This is more concise but is a trade off.
jaccardSim2 <- stringsim(articleTxt, llmResponse2, method = 'jaccard', q = 1)
jaccardSim2

# End