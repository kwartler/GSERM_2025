#' Purpose: apply k means clustering to text
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 9, 2024


# Libs
library(tm)
library(clue)
library(cluster)
library(dplyr)
library(factoextra)

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
stops  <- c(stopwords('SMART'), 'work')

# Read
txt <- read.csv('https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/C_Sentiment_Unsupervised/data/basicResumes.csv')
txt$text <- stringi::stri_encode(txt$text, "", "UTF-8")

# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(txt$text))

# Clean & Organize
txtMat <- cleanCorpus(txtCorpus, stops)
txtMat <- DocumentTermMatrix(txtMat) 

# Scale it
txtMat <- scale(as.matrix(txtMat)) #subtract mean  & divide by stDev

# Look at WSS...takes a long time with so many docs
pElbow <- fviz_nbclust(txtMat, 
                       kmeans, 
                       k.max=25, 
                       method = "wss")
# We expect a drop off but if you see a consistent somewhat linear view where adding another and another has small gains; points to the method being not great for the data
pElbow

# Let's just apply 3
txtKMeans <- kmeans(txtMat, 3)
txtKMeans$size
barplot(txtKMeans$size, main = 'k-means')

# Looks like 1 cluster dominates, not surprising since this isn't a great method for text.
# silhouette measures a distance from where the observation sits in the cluster to the next nearest cluster 
dissimilarityMat <- dist(txtMat)
silPlot          <- silhouette(txtKMeans$cluster, dissimilarityMat)
head(silPlot, 50) #1=well matched, 0=similar to next cluster, -1=misclassified

# The sihouette plot needs to have k cluster "shadows" that are tall and distinct.
plot(silPlot, col=1:max(txtKMeans$cluster), border=NA)


#calculate indices of closest document to each centroid
idx <- vector()
for (i in 1:max(txtKMeans$cluster)){
  
  # Calculate the absolute distance between doc & cluster center
  absDist <- abs(txtMat[which(txtKMeans$cluster==i),] -  txtKMeans$centers[i,])
  
  # Check for single doc clusters
  if(is.null(nrow(absDist))==F){
    absDist <- rowSums(absDist)
    minDist <- subset(absDist, absDist==min(absDist))
  } else {
    minDist <- txtKMeans$cluster[txtKMeans$cluster==i]
  }
  idx[i] <- as.numeric(names(minDist))
}

# Notification of closest doc to centroid
cat(paste('cluster',1:max(txtKMeans$cluster),': centroid closest doc is ', idx,'\n'))

# End
