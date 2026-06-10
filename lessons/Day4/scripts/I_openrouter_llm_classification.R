#' Author: Ted Kwartler
#' Description: An R script to perform text classification using the OpenRouter cloud API

# Libraries
library(httr)
library(jsonlite)
library(pbapply)

# Obtain all the forum posts from the teaching data repo
# You would point this to your corpus 
urlA <- paste0('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/doc_class_examples/',
               101600:101609,
               '.txt')
urlB <- paste0('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/doc_class_examples/', 
               54110:54119,
               '.txt')
allFilesURLS <- c(urlA, urlB)
allFiles <- pblapply(allFilesURLS, readLines)
allFiles[[1]]

# So we must collapse each document from lines to one string but 
# not collapse them into a single document
allFiles <- lapply(allFiles, paste, collapse = '\n')
cat(allFiles[[1]])

# Let's make a custom system instruction
sysPrompt <- 'You are a document classifier.  You review text and assign specific attributes to the document. You must assign one of following tags that best describes the topic of the text.  Here are your options for document classification:\n
- Science & Technology
- Entertainment
- Automotive
- News\n\nYou will only respond with the single classification that BEST describes the text.  You will not add any additional information or commentary.  For example, after reviewing a body of text you would simple state:\n\nEducation\n\nHere is text to review and classify:\n\n'

# OpenRouter Specific Key & Model Configuration
llmModel       <- "google/gemini-3.1-flash-lite"
openrouterKey <- Sys.getenv("OPENROUTER_API_KEY")

if (openrouterKey == "") {
  stop("API Key missing! Please set OPENROUTER_API_KEY in your .Renviron file.")
}

# Request headers tailored for OpenRouter cloud authentication
headers <- c(
  `Content-Type`  = "application/json",
  `Authorization` = paste("Bearer", openrouterKey),
  `HTTP-Referer`  = "http://localhost",
  `X-Title`       = "R Batch Document Classification Script"
)

# Organize the request payload for the initial individual test
dataLLM <- list(
  model = llmModel,
  messages = list(
    list(role = "system", content = sysPrompt),
    list(role = "user", content = allFiles[[1]])),
  temperature = 0, # Low temperature ensures reproducibility in classification
  max_tokens = 256,  
  stream = FALSE)

# Make the test POST request to the cloud OpenRouter API endpoint
res <- httr::POST(url = "https://openrouter.ai/api/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = toJSON(dataLLM, auto_unbox = TRUE))

# Extract the generated classification label from the test response
llmResponse <- httr::content(res)$choices[[1]]$message$content
cat("--- Individual Test Result ---\n", llmResponse, "\n\n")


# ------------------------------------------------------------------------------
# Batch Mode: Iterating across all downloaded documents
# ------------------------------------------------------------------------------
docClasses <- list()

for(i in 1:length(allFiles)){
  print(paste("Processing document:", i))
  
  dataLLM <- list(
    model = llmModel,
    messages = list(
      list(role = "system", content = sysPrompt),
      list(role = "user", content = allFiles[[i]])),
    temperature = 0, 
    max_tokens = 256,  
    stream = FALSE)
  
  # Make the looping POST request to the cloud OpenRouter API endpoint
  res <- httr::POST(url = "https://openrouter.ai/api/v1/chat/completions",
                    httr::add_headers(.headers = headers),
                    body = toJSON(dataLLM, auto_unbox = TRUE))
  
  # Extract classification text payload
  llmResponse <- httr::content(res)$choices[[1]]$message$content
  
  docClasses[[i]] <- data.frame(urlFile = allFilesURLS[i],
                                llmClassification = llmResponse)
}

# Now organize into a unified data frame
docClassesDF <- do.call(rbind, docClasses)
head(docClassesDF)

# Generate frequency distribution table of the extracted categories
table(docClassesDF$llmClassification)

# End