#' Using embeddings and labeled data, find closest concept terms
#' Example using GloVE
#' TK
#' May 23, 2024
#' 

# Libraries
library(tm)
library(text2vec)

# Data
# https://github.com/kwartler/text_mining/blob/master/Airbnb-boston_only.zip
reviews <- read.csv('~/Desktop/ICPSR/personalFiles/Airbnb-boston_only.csv')

# Quick cleaning functions; NOT in an Corpus
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

reviewClean<-function(xVec, stops = stopwords('english')){
  xVec <- tryTolower(xVec)
  xVec <- removeWords(xVec, stops)
  xVec <- removeNumbers(xVec)
  xVec <- removePunctuation(xVec)
  xVec <- stripWhitespace(xVec)
  return(xVec)
}

# Clean
reviews <- reviewClean(reviews$comments)

# Define how do we iterate through the text?
tokenIteration <- itoken(reviews) 

# Build Vocab
vocab <- create_vocabulary(tokenIteration,
                          ngram = c(ngram_min = 1, ngram_max = 1))

# We have a vocabulary with 1-3 word combinations
# We can "vectorize" this into a DTM
vectorizerFunction <- vocab_vectorizer(vocab)

# Using the function we can make the DTM
reviewDTM <- create_dtm(tokenIteration, vectorizerFunction)
dim(reviewDTM)
summary(Matrix::rowSums(reviewDTM))

tmp <- as.matrix(reviewDTM)
tmp[1:2,1:10]

# GloVe: Global Vectors for Word Representation, is an unsupervised learning algorithm for obtaining vector representations for words based on their co-occurrence statistics in a corpus
# https://text2vec.org/glove.html

# Find the co occurance of words in a window
tokenCoOccurence <-create_tcm(tokenIteration, 
                              vectorizerFunction, 
                              skip_grams_window = 5)
tokenCoOccurence[1:10,1:10]
dim(tokenCoOccurence)

# Define GloVe algo unsupervised model fit
glove <- GlobalVectors$new(rank = 50, x_max = 10)

# Apply the definition to the tokenCoOccurence: Get the "main" embeddings
txtTransform <- glove$fit_transform(tokenCoOccurence, # data
                              n_iter = 10, # refit to get stable outcomes
                              n_threads = 8) #parallel cores to speed up comp time
dim(txtTransform)# rows= unique tokens, columns = 50 defined vectors
txtTransform[1:10,1:10]
# The model also creates a less accurate "context" set of embeddings since its a co-occurence
docContext <- glove$components
dim(docContext)
docContext[1:10,1:10] # rows= 50 defined vectors, columns = unique tokens

# The paper suggests adding the main & context embeddings for best results
finalEmbeddings <- docContext + t(txtTransform)
finalEmbeddings[1:10, 100:110]

# Now we finally have "serial numbers" for terms so we can create "concepts"
# Then measure the cosine similarity with other words
goodWalks <- finalEmbeddings[,'walk' , drop = FALSE] -
  finalEmbeddings[,'disappointed' , drop = FALSE] +
  finalEmbeddings[,'good' , drop = FALSE]
cosSim <-sim2(x = t(goodWalks), 
              y = t(finalEmbeddings), 
              method = "cosine", 
              norm = "l2")

# Organize & Order
cosineDF <- data.frame(term = colnames(cosSim),
                       cosineValueToTerm  = cosSim[1,],
                       row.names = NULL)
cosineDF <- cosineDF[order(cosineDF$cosineValueToTerm, decreasing = T),]
head(cosineDF, 10)

# Now another concept to explore
dirtySinks <- finalEmbeddings[,'sink' , drop = FALSE] -
  finalEmbeddings[,'condition' , drop = FALSE] +
  finalEmbeddings[,'dirty' , drop = FALSE]
cosSim <-sim2(x = t(dirtySinks), 
              y = t(finalEmbeddings), 
              method = "cosine", 
              norm = "l2")
cosineDF <- data.frame(term = colnames(cosSim),
                       cosineValueToTerm  = cosSim[1,],
                       row.names = NULL)
cosineDF <- cosineDF[order(cosineDF$cosineValueToTerm, decreasing = T),]
head(cosineDF, 10)
# End