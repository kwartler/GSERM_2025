#' Title: Live Code
#' Purpose: Lexicon Based Sentiment Analysis
#' Author: Ted Kwartler
#' Date: June 9, 2024
#' 

# Libs - tm, tidytext, dplyr, wordcloud
library(tm)
library(tidytext)
library(dplyr)
library(wordcloud)

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


# Stops
customStopwords <- c(stopwords('SMART'),'restaurant')


# Read in the data
reviews <- read.csv('https://raw.githubusercontent.com/kwartler/GSERM_2024/main/lessons/Day3_sentiment/data/yelp_live_code_sample/yelp_review_sample35K.csv')

# examine
tail(reviews, 3)

# Document level tidy org
cleanTibbles <- list()
for(i in 1:nrow(reviews)){
  x <- VCorpus(VectorSource(reviews[i,2])) #declare as a corpus
  x <- cleanCorpus(x, customStopwords) #clean each corpus
  x <- DocumentTermMatrix(x) #make a DTM
  x <- tidy(x) #change orientation
  x$document <- i
  cleanTibbles[[i]] <- x #put it into the list
  print(paste('complete with',i))
}

# Now to organize into a long triplet
oneTriplet <- do.call(rbind, cleanTibbles)

# Get the afinn Lexicon
afinn <- get_sentiments(lexicon = c("afinn"))

# Perform the join
polarizedReviewWords <- inner_join(oneTriplet, afinn, 
                                   by = c('term'='word'),
                                   relationship = 'many-to-many')

# Account for the count and polarized value
polarizedReviewWords$polarizedValue <- polarizedReviewWords$count*polarizedReviewWords$value

# Now let's aggregate pol value by doc and sum
polarizedDocs <- aggregate(polarizedValue~document, polarizedReviewWords, sum)

# Basic examination with summary and density plot
summary(polarizedDocs$polarizedValue)
plot(density(polarizedDocs$polarizedValue))

# Let's subset doc IDs that are positive, negative & neutral
posIdx <- subset(polarizedDocs$document,polarizedDocs$polarizedValue>0 )
negIdx <- subset(polarizedDocs$document,polarizedDocs$polarizedValue<0 )
neuIdx <- setdiff(1:nrow(reviews), c(posIdx, negIdx))

# Now let's make a quick comparison cloud
posReviews <- reviews[posIdx,]
negReviews <- reviews[negIdx,]
neuReviews <- reviews[neuIdx,]

# Collapse 
posReviews <- paste(posReviews, collapse = ' ')
negReviews <- paste(negReviews, collapse = ' ')
neuReviews <- paste(neuReviews, collapse = ' ')

# Organize and clean
allReviews <- c(posReviews, negReviews, neuReviews)
allReviews <- cleanCorpus(VCorpus(VectorSource(allReviews)), customStopwords)

# DTM
reviewDTM <- DocumentTermMatrix(allReviews)
dim(reviewDTM)

# Make it into a simple matrix
reviewDTMm <- as.matrix(reviewDTM)

# Make sure order is the same as the c(objA, objB) 
rownames(reviewDTMm) <- c('positive_reviews', 'negative_reviews', 'neutral_reviews')

# Make comparison cloud; requires TDM!
comparison.cloud(t(reviewDTMm),
                 max.words=75,
                 random.order=FALSE,
                 title.size=2,
                 colors=c('purple','#bada55', 'orange'))

# End