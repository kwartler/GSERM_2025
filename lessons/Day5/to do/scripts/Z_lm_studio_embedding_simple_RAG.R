#' Using embeddings from an open source model to find aligned docs
#' Example using GloVE and also LM Studio
#' TK
#' May 23, 2024
#' 

# Remember to load the embedding model

# Libraries
library(httr)
library(jsonlite)

# Let's read in a bunch of documents
folderPath <- '~/Desktop/ICPSR/ICPSR_Day3/data/AutoAndElectronics/'
allFiles <- list.files(folderPath, 
                       pattern = '.txt', 
                       recursive = T, 
                       full.names = T)
# Read them in
allFiles <- lapply(allFiles, readLines)

# POST to the embedding model
headers <- c(`Content-Type` = "application/json")

# Loop through each to get the LLM embeddings
docVectors <- list()
for(i in 1:length(allFiles)){
  print(i)
  oneDoc <- paste(allFiles[[i]], collapse = ' ')
  
  dataObj <- list(input = oneDoc)
  
  # Convert the list to JSON
  data <- toJSON(dataObj, auto_unbox = TRUE)
  
  headers <- c('Content-Type' = 'application/json')
  
  res <- httr::POST(
    url = "http://localhost:1234/v1/embeddings", 
    httr::add_headers(.headers = headers), 
    body = data,
    encode = "json"
  )
  
  vectorEmbeddings <- httr::content(res)$data
  vectorEmbeddings <- unlist(vectorEmbeddings[[1]]$embedding)
  docVectors[[i]]<- vectorEmbeddings
}

# Resulting vectors
docVectors<- do.call(rbind, docVectors)
dim(docVectors)
docVectors[1:10,1:50]

# Any clustering would work here
nClusters <- 5
clusteringResults <- kmeans(docVectors, centers=nClusters)

# Cluster assignments
print(clusteringResults$cluster)
table(clusteringResults$cluster)

# Quickly ID the number of clusters
maxClusters <- 20
wss <- vector()
for (i in 1:maxClusters) {
  kmeansResult <- kmeans(docVectors, centers=i, nstart = 10)
  wss[i] <- kmeansResult$tot.withinss
  print(i)
}

# Plot WSS for each number of clusters
plot(1:maxClusters, wss, type='b', 
     xlab="Number of clusters", 
     ylab="Within-Cluster Sum of Squares",
     main="Elbow Curve for k-means clustering",
     col="blue", pch = 20, cex = 1.5)

# Now that your entire data set has embeddings you can find similar documents to a new document (assumes Euclidean distance measures, but could be others like cosine sim)
# You would read in the new doc, then send it to the embedding model; here we just chose one to show you
someNewDoc <- docVectors[42,]

# Calculate Euclidean distance; keep in mind there are other distance measures.
distances <- sqrt(rowSums((t(t(docVectors) - someNewDoc))^2))

# Find closest n rows
nDocs <- 5
closestRows <- order(distances)[1:nDocs]

# Closest docs to the new one:
# Since we used one already in the data, it is the first one, distance = 0
print(closestRows)

# End