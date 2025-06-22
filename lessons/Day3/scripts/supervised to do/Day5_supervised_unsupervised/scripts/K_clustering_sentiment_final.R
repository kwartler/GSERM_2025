#' Purpose: Apply NRC to get news source sentiment & cluster to get news topics
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 12, 2024
#' HAVE STUDENTS GET LATEST FROM REPO

# no sci notation
options(scipen = 999)

# Libs
library(skmeans)
library(tidytext)
library(tm)
library(clue)
library(cluster)
library(wordcloud)
library(lexicon)
library(dplyr)
library(plyr)
library(radarchart)
library(ggplot2)
library(ggthemes)
library(textdata)
library(tidyr)

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

# Examine Raw Text
rawTxt <- read.csv('https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/C_Sentiment_Unsupervised/data/exampleNews.csv')
rawTxt$description <- stringi::stri_encode(rawTxt$description, "", "UTF-8")
rawTxt$content <- stringi::stri_encode(rawTxt$content, "", "UTF-8")
rawTxt$title <- stringi::stri_encode(rawTxt$title, "", "UTF-8")

# Examine the meta
t(rawTxt[1,])

# Organize into a DF for TM
allInfoRaw <- data.frame(doc_id = 1:nrow(rawTxt),
                         text   = paste0(rawTxt$title, 
                                         rawTxt$description, 
                                         rawTxt$content),
                         source = rawTxt$id)

# Now the TM
stops  <- c(stopwords('SMART'),'chars') # API truncation "[+3394 chars]"

# Process
allInfo    <- VCorpus(DataframeSource(allInfoRaw))
allInfo    <- cleanCorpus(allInfo, stops) 
allInfoDTM <-  DocumentTermMatrix(allInfo)
allInfoDTM <- as.matrix(allInfoDTM)
allInfoDTM <- subset(allInfoDTM, rowSums(allInfoDTM) > 0)
dim(allInfoDTM)

#### Perform a Spherical K Means Clustering
set.seed(1234)
txtSKMeans <- skmeans(allInfoDTM, 
                      4, 
                      m = 1, #1=hard partition
                      control = list(nruns = 5, verbose = T))

# Examine Separation
barplot(table(txtSKMeans$cluster), main = 'spherical k-means')
plot(silhouette(txtSKMeans), col=1:2, border=NULL)

# What are the terms of our 2 clusters
# ID protypical terms
protoTypical           <- t(cl_prototypes(txtSKMeans))
colnames(protoTypical) <- paste0('cluster_',1:ncol(protoTypical))
head(protoTypical)

# Make a comparison cloud of word clusters
#pdf(file = "~/Desktop/GSERM_ICPSR/personalFiles/news_cluster_topics.pdf", 
#    width  = 6, 
#    height = 6) 
comparison.cloud(protoTypical, title.size=1.1, scale=c(1,.5))
#dev.off()

# Append clusters to the raw data
allInfoRaw$cluster <- txtSKMeans$cluster
head(allInfoRaw)

# align just source and cluster
metaInfo <- data.frame(allInfoRaw$source, txtSKMeans$cluster)
head(metaInfo)

#### Perform an NRC Sentiment Inner Join
tidyCorp <- tidy(DocumentTermMatrix(allInfo))
tidyCorp

# Let's understand the meta data of news source rows 1-100 are Washington Post etc
sourceID <- unique(meta(allInfo))
sourceID

# Cut documents into the 5 sources and append to the corresponding document in the tidy corp
seq(0,500,100) #example
tidyCorp <- as.data.frame(tidyCorp) # change object class
tidyCorp$source <- cut(as.numeric(tidyCorp$document), 
                       breaks = seq(0,500,100), 
                       labels = sourceID[,1])
head(tidyCorp[grep('msnbc',tidyCorp$source)-2,])

# Make sure vector types as the same
tidyCorp$document <- as.numeric(tidyCorp$document)

# Append the cluster assignment to the document
tidyCorp <- left_join(tidyCorp, 
                      allInfoRaw[,c('doc_id','cluster')],
                      by = c('document'='doc_id'))

head(tidyCorp)

# NRC load
nrc <- textdata::lexicon_nrc()

# NRC join
tidyCorp <- inner_join(tidyCorp, nrc, 
                      by = c('term'= 'word'),
                      relationship = "many-to-many")
head(tidyCorp)


# Subset to one cluster (we have 1 to 4)
whichCluster <- 2
tidyCorpOne <- subset(tidyCorp, tidyCorp$cluster==whichCluster)

# Drop positive & negative
tidyCorpOne <- subset(tidyCorpOne, 
                      tidyCorpOne$sentiment!='positive')
tidyCorpOne <- subset(tidyCorpOne, 
                      tidyCorpOne$sentiment!='negative')

emos <- aggregate(count~sentiment+source, tidyCorpOne, sum)
emos <- emos %>% 
pivot_wider(names_from = source, values_from = count) %>% as.data.frame()

# Examine
emos

# Make it proportional example
# as a proportion of the channel's effort
propSent <- prop.table(as.matrix(emos[,2:ncol(emos)]), margin = 2)
propSent <- data.frame(source =emos[,1],
                       propSent)
propSent
# Make a radar chart
chartTitle <- paste(names(which.max(protoTypical[,whichCluster])), 'cluster')
chartJSRadar(scores = propSent[,2:ncol(propSent)], 
             labs = propSent$source,
             labelSize = 10, showLegend = F,
             main = chartTitle)

# End