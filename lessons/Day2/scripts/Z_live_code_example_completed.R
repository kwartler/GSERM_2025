# Author: TK
# date: June 14, 2025
# purpose: live code a text mining project

# What words are used to describe 130k wines, can be done by country, region, points or price
# *large-ish* data: "winemag-data-130k-v2.csv"

# Data Path
wine <- 'https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/winemag-data-130k-v2.csv'

# Libs
library(wordcloud)
library(ggplot2)
library(ggthemes)
library(tm)

# Lowercase
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Corpus processing
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}


# Let's use bigrams
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "),
         use.names = FALSE)}

# Stops
stop <- c(stopwords('SMART'),'wine')

# Read in the data
text <- read.csv(wine)

# Examine the data
dim(text)
names(text)
head(text,8)
unique(text$country)
table(text$country)
summary(text)

wineCountries <- c('France','Italy','Portugal')

# Make three small corpora
frenchWine     <- subset(text, text$country=="France")
italianWine    <- subset(text, text$country=="Italy")
portuguesewine <- subset(text, text$country=="Portugal")

# Make the corpora
frCorpus <- VCorpus(VectorSource(frenchWine$description))
itCorpus <- VCorpus(VectorSource(italianWine$description))
poCorpus <- VCorpus(VectorSource(portuguesewine$description))

# Clean the corpora
frCorpus <- cleanCorpus(frCorpus,stop)
itCorpus <- cleanCorpus(itCorpus,stop)
poCorpus <- cleanCorpus(poCorpus,stop)

# Create the document term matrix
frDTM <- DocumentTermMatrix(frCorpus,control=list(tokenize=bigramTokens))
itDTM <- DocumentTermMatrix(itCorpus,control=list(tokenize=bigramTokens))
poDTM <- DocumentTermMatrix(poCorpus,control=list(tokenize=bigramTokens))

# Dimensions
dim(frDTM)
dim(itDTM)
dim(poDTM)

# change to a simple matrix
#frDTM <- as.matrix(frDTM)
#itDTM <- as.matrix(itDTM)
#poDTM <- as.matrix(poDTM)

frColSums <- slam::col_sums(frDTM)
itColSums <- slam::col_sums(itDTM)
poColSums <- slam::col_sums(poDTM)

# WFM
frWFM <- data.frame(biGrams  = names(frColSums),
                    frequency = frColSums,
                    row.names = NULL)

itWFM <- data.frame(biGrams  = names(itColSums),
                    frequency = itColSums,
                    row.names = NULL)

poWFM <- data.frame(biGrams  = names(poColSums),
                    frequency = poColSums,
                    row.names = NULL)

# Order these to be most frequent
frWFM  <- frWFM[order(frWFM$frequency, decreasing = T),]
frWFM[1:6,]
itWFM  <- itWFM[order(itWFM$frequency, decreasing = T),]
itWFM[1:6,]
poWFM  <- poWFM[order(poWFM$frequency, decreasing = T),]
poWFM[1:6,]

# Association?
#frBlackFruitAssociations <- findAssocs(frDTM, 'black fruits', 0.30)

# Simple word clouds
wordcloud(frWFM$biGrams[1:25],
          frWFM$frequency[1:25],
          random.order = F,
          colors="#bada55")
wordcloud(itWFM$biGrams[1:25],
          itWFM$frequency[1:25],
          random.order = F,
          colors="darkred")
wordcloud(poWFM$biGrams[1:25],
          poWFM$frequency[1:25],
          random.order = F,
          colors="grey")

# Comparison cloud
# Extract the plain text & collapse to one blob
frCorpusPlain <- sapply(frCorpus, NLP::content)
frCorpusPlain <- paste(frCorpusPlain, collapse = ' ')

itCorpusPlain <- sapply(itCorpus, NLP::content)
itCorpusPlain <- paste(itCorpusPlain, collapse = ' ')

poCorpusPlain <- sapply(poCorpus, NLP::content)
poCorpusPlain <- paste(poCorpusPlain, collapse = ' ')

# Combine into a new 3 document corpus
allCounties <- c(frCorpusPlain, itCorpusPlain, poCorpusPlain)
allCounties <- VCorpus((VectorSource(allCounties)))

# TDM
allCountriesTDM <- TermDocumentMatrix(allCounties)

# Append the column names
colnames(allCountriesTDM) <- c('fr','it','po')

# Comparison cloud
comparison.cloud(as.matrix(allCountriesTDM),
                 max.words=75,
                 random.order=FALSE,
                 title.size=3,
                 colors=c('tomato','goldenrod','darkgreen'))


# End
