#' Purpose: Use text for various HC and network visuals
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: june 14, 2025
#'

# Declare the data path
filePath  <- 'https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/studentLoan_2024.csv'

# Libs
library(tm)
#library(qdap)
library(ggplot2)
library(ggthemes)
library(igraph)
library(networkD3)

# Custom functions
# Robust to lower
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Cleaning
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}


# Sub Function
complaintSubstitutions <- function(narrativeVector){
  x <- gsub('(X{2}\\/X{2}\\/X{4})|(X{2}\\/X{2}\\/[0-9]{2,4})|([0-9]{2}\\/[0-9]{2}\\/[0-9]{2,4})', '', narrativeVector, perl = T)
  x <- gsub('(X{2}\\/X{2}\\/X{4})|(X{2}\\/X{2}\\/[0-9]{2,4})|([0-9]{2}\\/[0-9]{2}\\/[0-9]{2,4})', '', x, perl = T)
  x <- gsub('X+', '', x)
  return(x)
}

# Create custom stop words
stops <- c(stopwords('english'), 'student', 'loan')

# Read in Data, clean & organize into a TDM!!
text      <- read.csv(filePath)
text$Consumer.complaint.narrative <- complaintSubstitutions(text$Consumer.complaint.narrative)
txtCorpus <- VCorpus(VectorSource(text$Consumer.complaint.narrative))
txtCorpus <- cleanCorpus(txtCorpus, stops)
txtDTM    <- DocumentTermMatrix(txtCorpus)

# Let's explore sparsity
txtDTM #all rows [terms] are majority 0

# Reduce TDM; tm's removeSparseTerms works on small TDM but not often on large
# reducedTDM <- removeSparseTerms(t(txtDTM), sparse=.99995)
# Find term frequencies
termFreq <- colSums(as.matrix(txtDTM))

# Let's drop terms that don't appear more than this value
drops <- 400
infrequentTerms <- names(termFreq)[termFreq < drops]

# Drop the infrequent terms not appearing at least 300 times
reducedDTM <- txtDTM[,!(colnames(txtDTM) %in% infrequentTerms)]

#shoot for ~50 terms
reducedDTM

# Organize the smaller DTM but transpose it for the clustering [actually now a TDM]
reducedHC <- as.data.frame(t(as.matrix(reducedDTM)))

# Basic Hierarchical Clustering
hc <- hclust(dist(reducedHC))
plot(hc,yaxt='n')

############ Back to PPT
idx <- grep('never', text$Consumer.complaint.narrative, ignore.case = T)
onlyNever <- text$Consumer.complaint.narrative[idx]
# qdap if you can!
#word_associate(onlyNever,
#               match.string = 'never',
#               stopwords = stops,
#               network.plot = T,
#               cloud.colors = c('black','darkred'))

# MORE QDAP!
#word_associate(text$Consumer.complaint.narrative,
#               match.string = 'never',
#               stopwords = stops,
#               wordcloud = T,
#               cloud.colors = c('black','darkred'))


# Slightly different but similar word network manually built.
# Convert the DocumentTermMatrix into a Matrix
reducedDTMm <- t(as.matrix(reducedDTM))

# Let's reduce it further
# Drop terms in bottom 50th percentile
reducedDTMm <- reducedDTMm[, colSums(reducedDTMm)> mean(colSums(reducedDTMm))]
# Drop docs with less than 50 terms, adjust this for your corpus
reducedDTMm <- reducedDTMm[rowSums(reducedDTMm) > 50,]

# Compute the word co-occurrence
wordCoOccurrence <- reducedDTMm %*% t(reducedDTMm)

# Create an igraph object from a matrix
wordNetwork <- graph_from_adjacency_matrix(wordCoOccurrence,
                                           mode = "undirected", 
                                           weighted = TRUE, 
                                           diag = FALSE)

# Assign words as vertex names
V(wordNetwork)$name <- colnames(wordCoOccurrence)

# Only add labels to the graphs when the relationship is greater than 10
V(wordNetwork)$label <- ifelse(degree(wordNetwork) > 10, V(wordNetwork)$name, NA) 

# Another step to reduce edges is to focus on the most connected terms
# Determine the threshold (e.g., top 5% weights)
threshold <- quantile(E(wordNetwork)$weight, 0.8)

# Delete edges with weight less than the threshold
wordNetwork <- delete_edges(wordNetwork, E(wordNetwork)[weight < threshold])


# Try to improve the layout since it can be too dense
layout <- layout_nicely(wordNetwork)

# Removing loops if any
wordNetwork <- simplify(wordNetwork, 
                        remove.multiple = FALSE, 
                        remove.loops = TRUE)

# Plot
plot(wordNetwork,
     layout = layout,   # use the layout
     vertex.label.cex = 0.8,
     edge.color = 'blue',
     main = "Word co-occurrence network")

# Make an interactive plot
nodes <- data.frame(name = V(wordNetwork)$name, group = rep(1, length(V(wordNetwork))))
edges <- get.data.frame(wordNetwork, what = "edges")
links <- data.frame(source = match(edges$from, nodes$name) - 1, 
                    target = match(edges$to, nodes$name) - 1, 
                    value = 1)

# Interactive Network Plot
forceNetwork(Links = links, Nodes = nodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name", 
             Group = "group", fontSize = 15, 
             opacity = 0.9)

# Sankey Plot
sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name", 
              fontSize = 15)

# End

