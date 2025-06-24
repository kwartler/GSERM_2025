#' Purpose: apply k Mediod clustering to text
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 9, 2024
#' https://cran.r-project.org/web/packages/kmed/vignettes/kmedoid.html


# Libs
# kmed is for k-medoid specific functions, but for speed with fviz_nbclust, clara is better
# library(kmed)
library(tm)
library(clue) 
library(cluster) 
library(factoextra) 
library(qdapRegex) 
library(ggplot2)

# Bring in our supporting functions
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Stopwords
stops  <- c(stopwords('SMART'), 'lol')

# Read
txt <- read.csv('https://github.com/kwartler/teaching-datasets/raw/refs/heads/main/3k_exampleTweets.csv')
txt$x <- stringi::stri_encode(txt$x, "", "UTF-8")

# For brevity and this odd data set let's remove duplicates
text <- txt[!duplicated(txt$x),]

# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(text))

# Clean & Organize
txtMat <- cleanCorpus(txtCorpus, stops)
txtMat <- DocumentTermMatrix(txtMat)
txtMatm <- as.matrix(txtMat)

# Look at WSS using clara for k-medoids, which is optimized for larger datasets
# Saved a copy to folder 
# It will internally handle sampling and distance calculations for each k.
#pElbow <- fviz_nbclust(x = txtMatm,
#                         FUNcluster = clara,# Clara does distances in samples, not all data
#                         k.max = 7,
#                         method = "wss",
#                         metric = "manhattan",    
#                         samples = 5)
#
#
# What do we observe?
#print(pElbow)
nClusters <- 2
txtKMeds <- clara(x = txtMatm,              
                  k = nClusters,         
                  metric = "manhattan",  
                  samples = 5,
                  medoids.x = T,
                  keep.data = T) # Set to FALSE to save memory if data is large

# Tally the cluster assignment
table(txtKMeds$cluster)
barplot(table(txtKMeds$cluster), main = paste0('k-medoids Cluster Sizes (k=', nClusters, ')'))

# Silhouette plot
silPlot <- fviz_silhouette(txtKMeds, main = paste0("Silhouette Plot (k=", nClusters, ")"))
plot(silPlot) + coord_flip()

# Median centroid documents:
# Clara returns 'medoids' which are indices into the original data.
# These are the rows of the DTM for the mediod centers, corresponding to num of clusters
txtKMeds$medoids

mediodCenters <- rownames(txtKMeds$medoids)
print(txt[mediodCenters,])

# End