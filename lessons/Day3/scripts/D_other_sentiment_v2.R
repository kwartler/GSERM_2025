#' Purpose: Other sentiment libraries
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 5, 2023
#'

# Libs
library(sentimentr)
library(lexicon) #lexicon::available_data()
library(dplyr)
library(SentimentAnalysis)
library(ggplot2)


# Read in data
txt <- read.csv("https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/yelp_review_sample35K.csv")
# another dataset to try "https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/twoNeighborhoods_BOS_airbnb_reviews.csv"


# this is a non-qdap wrapper for sentiment from sentimentr, with come minor differences around dictionary and negations
?sentiment
sentResults <- sentiment(text.var = get_sentences(txt$text),
                         polarity_dt = lexicon::hash_sentiment_jockers_rinker,
                         valence_shifters_dt = lexicon::hash_valence_shifters,
                         amplifier.weight = 0.8, n.before = 5, n.after = 2)
head(sentResults)

# Let's aggregate by word count
totalWords <- aggregate(word_count ~ element_id, sentResults, sum)

# Since sentiment is adjusted for doc & sentiment length simple sum should be fine
totalSent <- aggregate(sentiment ~ element_id, sentResults, mean) #could try sum too

results <- left_join(totalWords, totalSent, by = join_by(element_id))
head(results)

# sometimes author effort and conviction can be seen with longer documents and the result is a "barbell" i.e. super bad and super positive reviews are often longer w/determined authors
plot(results$word_count, results$sentiment)
cor(results$word_count, results$sentiment, use = 'complete.obs')

# What about by a grouping variable like author or meta information
# If you have two groups then you can add a by parameter, here is a fake example
set.seed(42)
txt$fakeGrp <- as.factor(sample(letters[1:6], size = nrow(txt), replace = TRUE))
sentGrp <- sentiment_by(text.var = get_sentences(txt$text),
                        by = txt$fakeGrp,
                        polarity_dt = lexicon::hash_sentiment_jockers_rinker,
                        valence_shifters_dt = lexicon::hash_valence_shifters,
                        amplifier.weight = 0.8, n.before = 5, n.after = 2)
head(sentGrp)

# Let's make a cleveland dot plot by groups
ggplot(sentGrp[1:15,], aes(x = word_count, y = reorder(fakeGrp, word_count))) +
  geom_segment(aes(yend = reorder(fakeGrp, ave_sentiment)),
               xend = 0, colour = "darkgrey") +
  geom_point(aes(x = word_count, y = reorder(fakeGrp, ave_sentiment), size = ave_sentiment)) +
  theme_bw()


#### Takes a long time; so save a copy of results
# Now let's try SentimentAnalysis, which can also accept a corpus directly, again some calc differences
# This function has an aggregate parameter but its unclear whether it does it by factor order thus
nReviews <- 100
multipleMethods <- analyzeSentiment(txt$text[1:nReviews],
                                    language = "english",
                                    aggregate = NULL,
                                    removeStopwords = TRUE,
                                    stemming = TRUE)
#saveRDS(multipleMethods,
#        '~/Desktop/ICPSR/personalFiles/multipleMethods.rds')
#multipleMethods <- readRDS('~/Desktop/ICPSR/personalFiles/multipleMethods.rds')
head(multipleMethods)

# Append the grp - we are only doing a few for expediency
results <- cbind(multipleMethods, fakeGrp = txt$fakeGrp[1:nReviews])
head(results)
# Depending on the method you may want to aggregate up now
neighborhoodResults <- results %>%
  group_by(fakeGrp) %>%
  summarise_at(vars(SentimentLM), list(name = mean)) %>% as.data.frame()
head(neighborhoodResults[order(neighborhoodResults$name, decreasing = T),],15)

# End
