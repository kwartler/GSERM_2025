#' Author: Ted Kwartler (Modified by Gemini)
#' Date: June 23, 2025
#' Description: An R script to perform named entity reco/extraction using a local LLM
#'              (via lm-studio)

# Libraries
library(httr)
library(jsonlite)
library(pbapply)
library(stringr)

# Inputs
# Model name
llmModel <- 'llama-3.2-1b-instruct'#'qwen2.5-7b-instruct'#
nCharChunk <- 10000
chunkOverlap <- 0.1 # between 0 [distinct] - 1 [all chunks the same]

# Some Documents are is very long.  We have two options, 1 to change the context window in LM studio or "chunk" our document in R to send it in chunk-wise.
# Custom function; Chunking function based on characters
chunkDocument <- function(textVector, chunkSize = 3500, overlap = 0.1) {
  n <- nchar(textVector)
  stepSize <- chunkSize * (1 - overlap)
  startPositions <- seq(1, n, stepSize)
  endPositions <- pmin(startPositions + chunkSize - 1, n)
  chunks <- substring(textVector, startPositions, endPositions)
  return(chunks)
}

# Document paths, you can also use list.files() if working locally
allFileLocations <- c('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/NER%20example%20-%20ZAF_1985_State_Department.txt',
                      'https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/SP_1985_State_Department.txt')

# Get an example document
# You would point this to your corpus 
govReports <- pblapply(allFileLocations, readLines)
govReports <- lapply(govReports, paste, collapse = '\n')

# Examine a document
cat(govReports[[2]])

# Let's make a custom system instruction
sysPrompt <- 'You are a named entity extraction AI.  You review text and identify names entities in text. You identify people, locations, and actions of people at that location.   You will not add any additional information or commentary and only respond with people, locations, and actions.  You will group people, locations and actions together.  Text that you review may have multiple entries.For example, after reviewing a body of text you would simply state:\n\nPeople:John Doe\nGroup:NA\nLocation:New York City\nAction:Went for a walk\n\nPeople:Jill Doe\nGroup:Organization A\nLocation:Atlanta\nAction:Civil Unrest\n\nPeople:NA\nGroup:Another Organization\nLocation:France\nAction:Parade\n\nYou will replace the examples with named people, locations and associated actions in your response from this text.  Here is text to review and classify:\n\n'

# Make sure the line breaks are as intended
cat(sysPrompt)

#  Let's apply it to all documents in our set
docNER <- list()
for (i in 1:length(govReports)) {
  print(paste('Starting Document',i))
  chunks <- chunkDocument(govReports[[i]],
                          chunkSize = nCharChunk, 
                          overlap = chunkOverlap)
  chunkResults <- list()
  for (j in 1:length(chunks)) {
    print(paste('chunk:',j,'of',length(chunks)))
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
    
    headers <- c(`Content-Type` = "application/json")
    res <- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                      httr::add_headers(.headers = headers),
                      body = toJSON(dataLLM, auto_unbox = TRUE))
    
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
# The larger model takes a few minutes to run so I saved a copy
# You can load the larger model NER to save time in class
# saveRDS(docNER, '~/Desktop/GSERM_2025/lessons/Day4/docNER.RDS')
# docNER <- readRDS('~/Desktop/GSERM_2025/lessons/Day4/docNER.RDS')
# docNER <- readRDS(url('https://github.com/kwartler/GSERM_2025/raw/refs/heads/main/lessons/Day4/docNER.RDS', "rb"))

# First Doc
str(docNER[[1]])
cat(docNER[[1]]$llmClassification)

# Second Doc
cat(docNER[[2]]$llmClassification)

# End