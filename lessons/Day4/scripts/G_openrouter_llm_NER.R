#' Author: Ted Kwartler 
#' Description: An R script to perform named entity reco/extraction using the OpenRouter cloud API
#' Dte June 4, 2026

# Libraries
library(httr)
library(jsonlite)
library(pbapply)
library(stringr)

# Inputs
# OpenRouter model name identifier
llmModel     <- "google/gemini-3.1-flash-lite"
nCharChunk   <- 10000
chunkOverlap <- 0.1 # between 0 [distinct] - 1 [all chunks the same]

# Custom function; Chunking function based on characters
chunkDocument <- function(textVector, chunkSize = 3500, overlap = 0.1) {
  n <- nchar(textVector)
  stepSize <- chunkSize * (1 - overlap)
  startPositions <- seq(1, n, stepSize)
  endPositions <- pmin(startPositions + chunkSize - 1, n)
  chunks <- substring(textVector, startPositions, endPositions)
  return(chunks)
}

# Document paths
allFileLocations <- c('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/NER%20example%20-%20ZAF_1985_State_Department.txt',
                      'https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/SP_1985_State_Department.txt')

# Get an example document
govReports <- pblapply(allFileLocations, readLines)
govReports <- lapply(govReports, paste, collapse = '\n')

# Examine a document
cat(govReports[[2]])

# Let's make a custom system instruction
sysPrompt <- 'You are a named entity extraction AI.  You review text and identify names entities in text. You identify people, locations, and actions of people at that location.   You will not add any additional information or commentary and only respond with people, locations, and actions.  You will group people, locations and actions together.  Text that you review may have multiple entries.For example, after reviewing a body of text you would simply state:\n\nPeople:John Doe\nGroup:NA\nLocation:New York City\nAction:Went for a walk\n\nPeople:Jill Doe\nGroup:Organization A\nLocation:Atlanta\nAction:Civil Unrest\n\nPeople:NA\nGroup:Another Organization\nLocation:France\nAction:Parade\n\nYou will replace the examples with named people, locations and associated actions in your response from this text.  Here is text to review and classify:\n\n'

# OpenRouter Specific Key Retrieval
openrouterKey <- Sys.getenv("OPENROUTER_API_KEY")

if (openrouterKey == "") {
  stop("API Key missing! Please set OPENROUTER_API_KEY in your .Renviron file.")
}

# Request headers tailored for OpenRouter cloud authentication
headers <- c(
  `Content-Type`  = "application/json",
  `Authorization` = paste("Bearer", openrouterKey),
  `HTTP-Referer`  = "http://localhost",
  `X-Title`       = "R Chunked NER Script"
)


# ------------------------------------------------------------------------------
# Process across all documents in our set sequentially
# ------------------------------------------------------------------------------
docNER <- list()

for (i in 1:length(govReports)) {
  print(paste('Starting Document', i))
  
  chunks <- chunkDocument(govReports[[i]],
                          chunkSize = nCharChunk, 
                          overlap = chunkOverlap)
  
  chunkResults <- list()
  
  for (j in 1:length(chunks)) {
    print(paste('chunk:', j, 'of', length(chunks)))
    
    dataLLM <- list(
      model = llmModel,
      messages = list(
        list(role = "system", content = sysPrompt),
        list(role = "user", content = chunks[j])
      ),
      temperature = 0.7,
      max_tokens = 512,
      stream = FALSE
    )
    
    # Make the looping POST request to the cloud OpenRouter API endpoint
    res <- httr::POST(url = "https://openrouter.ai/api/v1/chat/completions",
                      httr::add_headers(.headers = headers),
                      body = toJSON(dataLLM, auto_unbox = TRUE))
    
    # Extract structural NER text payload
    llmResponse <- httr::content(res)$choices[[1]]$message$content
    
    chunkResults[[j]] <- data.frame(
      urlFile = allFileLocations[i],
      chunk_id = j,
      llmClassification = llmResponse
    )
  }
  docNER[[i]] <- do.call(rbind, chunkResults)
}

# Examine our results
# First Doc
str(docNER[[1]])
cat(docNER[[1]]$llmClassification)

# Second Doc
cat(docNER[[2]]$llmClassification)

# End