#' Purpose: Using OpenRouter cloud embedding + llm models create a simple RAG workflow
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 25, 2025
#'


# Library
library(jsonlite)
library(httr)
library(lsa) #calculate numeric cosine similarity

# Sci notation - off
options(scipen = 999)

# Inputs
llmModel <- 'google/gemini-3.1-flash-lite'
embeddingModel <- 'openai/text-embedding-3-small' # must match the model used in C_basicVectors.R
sysPromptRAG <- 'You are a helpful, knowledgeable AI assistant. Answer the user\'s question accurately and concisely.  Answer the user\'s question using only the information provided below:\n\n'
userPrompt <- 'In the movie Star Wars: The Mandalorian and Grogu, what mission are Din Djarin and Grogu sent on, and who hires them?'
savePath <- '~/Desktop/GSERM_2025/personalFiles/'
topN <- 3
maxTokens <- 512*4
temp <- 0.7

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

# STEP 1: Ask the base model the question with NO retrieval to show what it
# does and does not know on its own.
baseLLM <- list(model = llmModel,
                messages = list(list(role = "user", content = userPrompt)),
                temperature = temp, max_tokens = maxTokens, stream = FALSE)
baseRes <- httr::POST(url = "https://openrouter.ai/api/v1/chat/completions",
                      httr::add_headers(.headers = headers),
                      body = toJSON(baseLLM, auto_unbox = TRUE))
cat(httr::content(baseRes)$choices[[1]]$message$content)

# STEP 2: Now do RAG. Read in our "vector database"
docVectors <- read.csv(paste0(savePath,'vectorEmbeddings.csv'))

# Turn the user's prompt into a vector embedding from the same vector model first
dataObj <- list(input = userPrompt, model = embeddingModel)

# Convert the list to JSON
dataLLM <- toJSON(dataObj, auto_unbox = TRUE)

# Make the embedding request
res <- httr::POST(
  url = "https://openrouter.ai/api/v1/embeddings",
  httr::add_headers(.headers = headers),
  body = dataLLM,
  encode = "json"
)

userEmbeddings <- httr::content(res)$data
userEmbeddings <- unlist(userEmbeddings[[1]]$embedding)

# This is the vector representation of the user's prompt
userEmbeddings

# Now we have the user prompt as a vector and a "database" of document vectors
# We can use cosine similarity to find the document vector closest to our user vector
allSimilarities <- apply(docVectors, 1, lsa::cosine,userEmbeddings)

# Reorder and grab the stop N document rows; we dont need the cosine sim scores
# just the row positions
idx <- order(allSimilarities, decreasing = TRUE)[1:topN]
idx # these are the corresponding documents from the database we need to get

# Get the appropriate documents; if we were using SQL or an actual DB we could make this more efficient by writing a query not reading in the entire table
movies <- read.csv(paste0(savePath,'movieDB.csv'))

# This is where we get the top 3 documents that have a strong cosine sim to the user's prompt vector.
relevantMovies <- movies[idx,] 

# Some data manipulation to organize the data 
# and keeping column names for all chunks so we dont confuse the llm
relevantMovies <- toJSON(relevantMovies, pretty = T)
relevantMovies

# Now we integrate the user prompt and the documents back into a single prompt
ragPrompt <- paste(userPrompt, '\n\n',relevantMovies)

# Pass this augmented prompt into the LLM
dataLLM <- list(model = llmModel,
                messages = list(
                  list(role = "system", content = sysPromptRAG),
                  list(role = "user", content = ragPrompt)),
                temperature = temp,
                max_tokens = maxTokens,
                stream = FALSE)

# Make the POST request
res <- httr::POST(url = "https://openrouter.ai/api/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = toJSON(dataLLM, auto_unbox = TRUE))

# Extract the response
llmResponse <- httr::content(res)$choices[[1]]$message$content
cat(llmResponse)

# End
