#' Purpose: Use text for various HC and network visuals
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 14, 2025
#'

# Declare the data path
filePath  <- 'https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/studentLoan_2024.csv'

# Libs
library(tm)
library(ggplot2)
library(ggthemes)
library(igraph)
library(networkD3)

# Custom functions
# Robust to lower
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
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
txtDTM 

# Find term frequencies
termFreq <- colSums(as.matrix(txtDTM))

# Let's drop terms that don't appear more than this value
drops <- 400
infrequentTerms <- names(termFreq)[termFreq < drops]

# Drop the infrequent terms not appearing at least 400 times
reducedDTM <- txtDTM[,!(colnames(txtDTM) %in% infrequentTerms)]

# Organize the smaller DTM but transpose it for the clustering
reducedHC <- as.data.frame(t(as.matrix(reducedDTM)))

# Basic Hierarchical Clustering
hc <- hclust(dist(reducedHC))
plot(hc, yaxt='n')


# ==============================================================================
# NEW: PLOT-SPECIFIC TERM REDUCTION FOR VISUAL CLARITY
# ==============================================================================
# Network diagrams and Sankey plots become illegible with more than ~25-35 nodes.
# We isolate the top 30 most frequent terms explicitly for the network visuals.
top_n_terms   <- 30
vis_term_freq <- colSums(as.matrix(reducedDTM))
top_terms     <- names(sort(vis_term_freq, decreasing = TRUE)[1:top_n_terms])

# Subset the DTM down to just these top visual terms
networkDTM <- reducedDTM[, colnames(reducedDTM) %in% top_terms]
# ==============================================================================


# Convert our visual DTM into a Matrix and transpose (Rows = Terms, Cols = Docs)
reducedDTMm <- t(as.matrix(networkDTM))

# Compute the word co-occurrence matrix using matrix multiplication
wordCoOccurrence <- reducedDTMm %*% t(reducedDTMm)

# Create an igraph object from the adjacency matrix
wordNetwork <- graph_from_adjacency_matrix(wordCoOccurrence,
                                           mode = "undirected", 
                                           weighted = TRUE, 
                                           diag = FALSE)

# Assign words as vertex names
V(wordNetwork)$name <- colnames(wordCoOccurrence)

# Set up clean vertex labels (always show labels now since nodes are limited)
V(wordNetwork)$label <- V(wordNetwork)$name

# Focus on the most meaningful connections (edges)
# Drop weak co-occurrences by filtering out the bottom 50% of edge weights
threshold   <- quantile(E(wordNetwork)$weight, 0.5)
wordNetwork <- delete_edges(wordNetwork, E(wordNetwork)[weight < threshold])

# Calculate a clean layout for plotting
layout <- layout_with_fr(wordNetwork) # Fruchterman-Reingold forces a cleaner spread

# Simplify the network structure by removing self-loops
wordNetwork <- simplify(wordNetwork, 
                        remove.multiple = FALSE, 
                        remove.loops = TRUE)

# Plot Static Network
plot(wordNetwork,
     layout = layout,
     vertex.label.cex = 0.8,
     vertex.size = 8,
     vertex.color = "lightgray",
     edge.color = 'darkgray',
     main = "Word Co-occurrence Network (Top 30 Terms)")


# ------------------------------------------------------------------------------
# Interactive Visuals Framework (networkD3)
# ------------------------------------------------------------------------------

# Format nodes dataframe
nodes <- data.frame(name = V(wordNetwork)$name, 
                    group = rep(1, length(V(wordNetwork))))

# Format edges dataframe
edges <- as_data_frame(wordNetwork, what = "edges")

# Build links dataframe ensuring accurate 0-indexed positioning for JavaScript
links <- data.frame(source = match(edges$from, nodes$name) - 1, 
                    target = match(edges$to, nodes$name) - 1, 
                    value  = log(edges$weight))

# Interactive Network Plot
forceNetwork(Links = links, Nodes = nodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name", 
             Group = "group", fontSize = 15, 
             opacity = 0.9, bounded = TRUE)

# Sankey Plot (Clean, text-isolated pipelines)
sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name", 
              fontSize = 12, nodeWidth = 30,
              width = 900,       
              height = 400,      
              nodePadding = 15)

# End