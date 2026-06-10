#' Purpose: Using an OpenRouter cloud embedding model create a simple "vector database"
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 10, 2026
#'


# Library
library(jsonlite)
library(httr)

# Inputs
embeddingModel <- 'openai/text-embedding-3-small'
#'perplexity/pplx-embed-v1-0.6b' # small + fast
#'
savePath <- '~/Desktop/GSERM_2025/personalFiles/'

# Retrieve API Key securely from the environment
openrouterKey <- Sys.getenv("OPENROUTER_API_KEY")

# Check if the key is missing to prevent silent failures
if (openrouterKey == "") {
  stop("API Key missing! Please set OPENROUTER_API_KEY in your .Renviron file.")
}

# Request header (OpenRouter requires Authorization, and recommends Referer/Title)
headers <- c(
  `Content-Type`  = "application/json",
  `Authorization` = paste("Bearer", openrouterKey),
  `HTTP-Referer`  = "http://localhost",
  `X-Title`       = "R Student Script"   # Shows up in your OpenRouter dashboard
)

# Get data contemporary information
movieDB <- fromJSON('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/finalMovie.json')

# Review 
head(movieDB)

# First turn every movie row into a single text string to embed
allDocs <- sapply(1:nrow(movieDB), function(i) toJSON(movieDB[i,], pretty = T))

# Make embeddings in BATCHES instead of one row at a time.
# The 'input' field accepts an array, so each POST embeds many movies at once.
# This turns ~1000 slow network calls into just a handful.
batchSize  <- 100
docVectors <- list()
for(start in seq(1, length(allDocs), by = batchSize)){
  idx <- start:min(start + batchSize - 1, length(allDocs))
  print(idx)

  # Send the whole batch as an array of strings in a single request
  dataObj <- list(input = allDocs[idx], model = embeddingModel)

  # Convert the list to JSON
  data <- toJSON(dataObj, auto_unbox = TRUE)

  res <- httr::POST(
    url = "https://openrouter.ai/api/v1/embeddings",
    httr::add_headers(.headers = headers),
    body = data,
    encode = "json"
  )

  # The API returns one embedding per input, in the same order we sent them
  batchData <- httr::content(res)$data
  for(j in seq_along(batchData)){
    docVectors[[idx[j]]] <- unlist(batchData[[j]]$embedding)
  }
}

# Resulting vectors
docVectors<- do.call(rbind, docVectors)
dim(docVectors)
docVectors[1:10,1:50]

# Save embeddings making a "vector database"
write.csv(docVectors, paste0(savePath,'vectorEmbeddings.csv'), row.names = F)

# Save the actual documents too so they can be retrieved from the embeddings
write.csv(movieDB, paste0(savePath,'movieDB.csv'), row.names = F)

# End
