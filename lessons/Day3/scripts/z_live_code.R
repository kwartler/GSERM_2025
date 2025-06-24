#' Title: Live Code
#' Purpose: Lexicon Based Sentiment Analysis
#' Author: Ted Kwartler
#' Date: June 9, 2024
#' 

# Libs - tm, tidytext, dplyr


# Custom Functions
# tryTolower

#cleanCorpus


# Stops


# Read in the data
reviews <- read.csv('https://github.com/kwartler/teaching-datasets/raw/refs/heads/main/yelp_review_sample35K.csv')

# Document level tidy org
cleanTibbles <- list()
for(i in 1:nrow(reviews)){
  x <- _______(________(reviews[i,2])) #declare as a corpus
  x <- _______(x, ____) #clean each corpus
  x <- ______(x) #make a DTM
  x <- tidy(x) #change orientation
  x$document <- i
  cleanTibbles[[i]] <- x #put it into the list
  print(paste('complete with',i))
}

# Now to organize into a long triplet
oneTriplet <- do.call(______, cleanTibbles)

# Get the afinn Lexicon
afinn <- ________(lexicon = c("______"))

# Perform the join
polarizedReviewWords <- ________(oneTriplet, afinn, 
                                   by = c('____'='____'),
                                   relationship = 'many-to-many')

# Account for the count and polarized value
polarizedReviewWords$polarizedValue <- polarizedReviewWords$_____*polarizedReviewWords$_____

# Now let's aggregate pol value by doc and sum
polarizedDocs <- ______(____~___, polarizedReviewWords, ___)

# Basic examination with summary and density plot
summary(____$______)
plot(_______(_______$_______))

# Let's subset doc IDs that are positive, negative & neutral
posIdx <- subset(____$document,polarizedDocs$____>0 )
negIdx <- subset(____$document,polarizedDocs$____<0 )
neuIdx <- setdiff(1:nrow(reviews), c(posIdx, negIdx))

# Now let's make a quick comparison cloud
posReviews <- reviews[______,]
negReviews <- reviews[______,]
neuReviews <- reviews[______,]

# Collapse 
posReviews <- paste(posReviews, ________ = ' ')
negReviews <- paste(__________, ________ = ' ')
neuReviews <- paste(__________, ________ = ' ')

# Organize and clean
allReviews <- c(posReviews, negReviews, neuReviews)
allReviews <- ___________(VCorpus(________(allReviews)), customStopwords)

# DTM
reviewDTM <- _________(allReviews)
dim(reviewDTM)

# Make it into a simple matrix
reviewDTMm <- ________(________)

# Make sure order is the same as the c(objA, objB) 
rownames(reviewDTMm) <- c('positive_reviews', 'negative_reviews', 'neutral_reviews')

# Make comparison cloud; requires TDM!
_________________(t(reviewDTMm),
                 max.words=__,
                 random.order=FALSE,
                 title.size=0.5,
                 colors=c('______','______', '______'))

# End