#' Title: Topic Modeling
#' Purpose: Unsupervised LDA model building
#' Author: Ted Kwartler
#' email: edward.kwartler@fas.harvard.edu
#' Date: Aug 16, 2023
#'

# Libs
library(skmeans)
library(tm)
library(clue)
library(cluster)
library(wordcloud)
library(irlba)
library(factoextra)
library(ggthemes)
library(purrr)

# Stopwords
stops  <- c(stopwords('SMART'), 'amp')

# Custom Functions
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

# Data
txt <- read.csv('https://github.com/kwartler/teaching-datasets/raw/refs/heads/main/3k_exampleTweets.csv')
txt$x <- stringi::stri_encode(txt$x, "", "UTF-8")

#txt <- read.csv('https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/C_Sentiment_Unsupervised/data/basicResumes.csv')

# Apply tm functions
txtCorp <- VCorpus(VectorSource(txt$x))
txtCorp <- cleanCorpus(txtCorp, stops)
txtCorpDTM <- DocumentTermMatrix(txtCorp)

# You can't have blank documents which can occur sometimes so let's check
drops <- slam::row_sums(txtCorpDTM)==0
sum(drops)

# If there are blanks you have to drop them; you may have to use as.matrix() first depending on the package version
txtCorpDTM <- txtCorpDTM[!drops,]

# Apply Spherical K-Means
nClusters <- 3
txtSKMeans <- skmeans(x = txtCorpDTM, # data
                      nClusters, #clusters
                      m = 1, #"fuzziness of cluster" 1 = hard partition, >1 increases "softness"
                      control = list(nruns = 5, verbose = T))

# Examine the mix of assignments
barplot(table(txtSKMeans$cluster), main = 'spherical k-means')
dev.off()

# We can apply PCA to reduce dimensions
# then append the cluster assignments for visualization
# to see how well the clusters separate
txtCorpPCA <- prcomp_irlba(as.matrix(txtCorpDTM),
                           n = 2,
                           center = TRUE, scale. = TRUE) #fast PCA

# Append the approximated PCA results and cluster assignments
plotDF         <- as.data.frame(txtCorpPCA$x)
names(plotDF)  <- c("PC1", "PC2")
plotDF$cluster <- txtSKMeans$cluster #cluster assignment for each doc
head(plotDF)

# Visualize the separation
fviz_cluster(list(data = plotDF, cluster = plotDF$cluster),
             ellipse.type = "confidence", geom= 'point') +
  theme_gdocs()

# Another view is the silhouette plot
# Silhouette plot - some small overlap but not too bad!
sk <- silhouette(txtSKMeans)
plot(sk, col=1:3, border=NA)
dev.off()

# We can get prototypical words for each cluster
protoTypical <- t(cl_prototypes(txtSKMeans))
head(protoTypical)

# Which ones are driving the cluster centers?
clusterTerms <- list()
for(i in 1:ncol(protoTypical)){
  x <- protoTypical[,i]
  x <- x[order(x, decreasing = T)]
  x <- head(x,10)
  nam <- paste0('cluster_',i)
  clusterTerms[[nam]] <- x
}
clusterTerms$cluster_1
clusterTerms$cluster_2
clusterTerms$cluster_3

# Coincidentally this structure works with comparison cloud
comparison.cloud(protoTypical[,1:3],
                 max.words=150,
                 random.order=FALSE,
                 title.size=1,
                 colors=brewer.pal(ncol(protoTypical),"Dark2"))



# End
