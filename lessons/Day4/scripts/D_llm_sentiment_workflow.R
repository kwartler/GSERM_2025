#' Title: Intro: LLM Based Sentiment
#' Purpose: Feature Extraction & Sentiment using a local LLM
#' Author: Ted Kwartler
#' Date: June 12, 2025
#' Source: Sentiment Analysis on a single file

# Libraries
library(httr)
library(jsonlite)

# Input to analyze
oneDoc <- 'The service department here is top-notch; they\'re always quick, efficient, and really go the extra mile to explain things. However, my experience with the sales team was quite pushy and left a lot to be desired. They weren\'t very flexible on pricing either.'

# Model & request type
llmModel <- 'qwen2.5-7b-instruct' #'llama-3.2-1b-instruct'
headers <- c(`Content-Type` = "application/json")

# Extract Sys Prompt
extractSysPrompt <- 'You are a helpful, smart, kind, and efficient AI assistant performing sentiment analysis. You always fulfill the user\'s requests to the best of your ability.  You are given text and must extract the key aspects of the customer review.  Here is an example customer review:\n\nFantastic vibe and the bartenders are super attentive, making great drinks with a smile. But wow, the drink prices are sky-high, definitely makes it a special occasion spot only.\n\nYou would respond with the attributes of the customer review that are relevant.   Here is an example response from  you:\n\nservice, prices\n\nDo not respond with any additional information.  Here is customer review text to extract the main attribute:\n\n'
cat(extractSysPrompt)

# Now another system prompt for the sentiment aspect
sentimentSystPrompt <- "You are a helpful, smart, kind, and efficient AI assistant performing sentiment analysis. You always fulfill the user's requests to the best of your ability.  You are presented with a list of attributes and a customer review.  Please assign a sentiment to each attribute of the customer review. Here is an example list of attributes:\n\nservice, prices\n\nHere is the associated customer review:\n\nFantastic vibe and the bartenders are super attentive, making great drinks with a smile. But wow, the drink prices are sky-high, definitely makes it a special occasion spot only.\n\nYou are to identify the positive, negative or neutral associated with each attribute in the list.\nHere is an example response from you:\n
service:positive\nprices:negative\n\n
Do NOT add any commentary.  Do NOT add any of the original text.  Only respond with the polarity and emotion labels structured as below.  For example you are presented some text and will respond like this:\n polarity:positive\nemotion:joy\n\nBelow is the text to analyze."
cat(sentimentSystPrompt)


# Organize Request
dataLLM <- list(model = llmModel,
                messages = list(
                  list(role = "system", content = extractSysPrompt),
                  list(role = "user", content = oneDoc)),
                temperature = 0.7,
                max_tokens = 512,
                stream = FALSE)

# Make the POST request
res <- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = toJSON(dataLLM, auto_unbox = TRUE))

# Extract the response
llmResponse <- httr::content(res)$choices[[1]]$message$content
cat(llmResponse)

# Put the attributes and the review together
dataPackage <- paste(llmResponse, '\n',oneDoc)
dataLLM <- list(model = llmModel,
                messages = list(
                  list(role = "system", content = sentimentSystPrompt),
                  list(role = "user", content = dataPackage)),
                temperature = 0.7,
                max_tokens = 512,
                stream = FALSE)
res <- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = toJSON(dataLLM, auto_unbox = TRUE))

# Extract the response
llmResponse <- httr::content(res)$choices[[1]]$message$content
cat(llmResponse)


# End
