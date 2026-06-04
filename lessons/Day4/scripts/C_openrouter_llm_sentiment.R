#' Title: Intro: LLM Based Sentiment
#' Purpose: Feature Extraction & Sentiment using OpenRouter cloud API
#' Author: Ted Kwartler 
#' June 4, 2026

# Libraries
library(httr)
library(jsonlite)

# Input to analyze
oneDoc <- readLines('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/yelp-1-star-review.txt')
oneDoc <- paste(oneDoc, collapse = ' ')

# OpenRouter Specific Inputs
llmModel       <- "google/gemini-3.1-flash-lite"
openrouterKey <- Sys.getenv("OPENROUTER_API_KEY")

if (openrouterKey == "") {
  stop("API Key missing! Please set OPENROUTER_API_KEY in your .Renviron file.")
}

# Request header (OpenRouter requires Authorization)
headers <- c(
  `Content-Type`  = "application/json",
  `Authorization` = paste("Bearer", openrouterKey),
  `HTTP-Referer`  = "http://localhost",
  `X-Title`       = "R Sentiment Script Strict"
)

# Organize Request
dataLLM <- list(model = llmModel,
                messages = list(
                  list(role = "system", content = "You are a helpful, smart, kind, and efficient AI assistant performing sentiment analysis. You always fulfill the user's requests to the best of your ability.  For polarity you can label text as positive, negative or neutral.  For emotions, you can use labels like joy, trust, fear, surprise, sadness, disgust, anger, anticipation to label text. Do NOT add any commentary.  Do NOT add any of the original text.  Only respond with the polarity and emotion labels structured as below.  For example you are presented some text and will respond like this:\n polarity:positive\nemotion:joy\n\nBelow is the text to analyze."),
                  list(role = "user", content = oneDoc)),
                temperature = 0.7,
                max_tokens = 512,
                stream = FALSE)

# Make the POST request to OpenRouter
res <- httr::POST(url = "https://openrouter.ai/api/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = toJSON(dataLLM, auto_unbox = TRUE))

# Extract the response
llmResponse <- httr::content(res)$choices[[1]]$message$content
cat(llmResponse)

## Keep in mind we could remove lots of the standard text to possibly improve results. 
# End