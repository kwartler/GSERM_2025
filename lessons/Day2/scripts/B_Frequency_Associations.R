#' Purpose: Frequency and Associations
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: May 11, 2024
#'

# Declare the data path
filePathA  <- 'https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/mortgageLoan_2024.csv'
filePathB <- 'https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/studentLoan_2024.csv'

# Libs
library(tm)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyverse)

# Custom functions
# Robust to lower
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
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
stops <- c(stopwords('english'), 'mortgage', 'student','loan')

# Data
text <- read.csv(filePathA)

# Substitutions
text$Consumer.complaint.narrative <- complaintSubstitutions(text$Consumer.complaint.narrative)

# Read in Data, clean & organize
txtCorpus <- VCorpus(VectorSource(text$Consumer.complaint.narrative))
txtCorpus <- cleanCorpus(txtCorpus, stops)
txtDTMa    <- DocumentTermMatrix(txtCorpus)
txtDTMm   <- as.matrix(txtDTMa)

# Frequency Data Frame
txtSums <- colSums(txtDTMm)
txtFreq <- data.frame(word=names(txtSums),
                      frequency=txtSums,
                      type = 'mortgage',
                      row.names = NULL)

# Review a section
txtFreq[50:55,]

# Simple barplot; top_n will determine the number of columns
topWordsA <- txtFreq[order(txtFreq$frequency, decreasing=F),]
topWordsA <- tail(topWordsA, 50)

# Chg to factor for ggplot
topWordsA$word <- factor(topWordsA$word,
                        levels=unique(as.character(topWordsA$word)))

ggplot(topWordsA, aes(y=word, x=frequency)) +
  geom_bar(stat="identity", fill='darkred') +
  theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0) +
  ggtitle('Mortgage Complaints')

# library qdap version, slightly different results based on params but faster
# plot(freq_terms(text$text, top=35, at.least=2, stopwords = stops))

# Now let's make some comparisons & explore other bar charts
text <- read.csv(filePathB)
text$Consumer.complaint.narrative <- complaintSubstitutions(text$Consumer.complaint.narrative)
txtCorpus <- VCorpus(VectorSource(text$Consumer.complaint.narrative))
txtCorpus <- cleanCorpus(txtCorpus, stops)
txtDTM    <- DocumentTermMatrix(txtCorpus)
txtDTMm   <- as.matrix(txtDTM)
txtSums   <- colSums(txtDTMm)
txtFreq   <- data.frame(word=names(txtSums),
                        frequency=txtSums,
                        type = 'student',
                        row.names = NULL)
topWordsB <- txtFreq[order(txtFreq$frequency, decreasing=F),]
topWordsB <- tail(topWordsB, 50)
topWordsB$word <- factor(topWordsB$word,
                         levels=unique(as.character(topWordsB$word)))

# Find words in commong among the corpora
inCommonTerms <- inner_join(topWordsA, topWordsB, by = "word")
head(inCommonTerms)

# Reshape the data
plotDF <- rbind(topWordsA, topWordsB)
plotDF <-plotDF %>%
  group_by(word) %>%
  filter(n_distinct(type) > 1)

# Side by Side bar chart
ggplot(data = plotDF, aes(x = word, y = frequency, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x = 'Word', y = 'Frequency') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Stacked Barchart
ggplot(plotDF, aes(x = word, y = frequency, fill = type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Words", y = "Frequency", title = "Word Frequency by Type")

# Proportional; keep in mind document lengths impact this comparison.
# We will learn TFIDF later
# or one could do proportional within a document before comparison.
propDF <- plotDF %>%
  group_by(word) %>%
  mutate(prop = frequency / sum(frequency))
ggplot(propDF, aes(x = word, y = prop, fill = type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Words", y = "Proportion (Percent)", title = "Proportional Word Frequency by Type")

############ Back to PPT

# Inspect word associations; takes about a minute to run
associations <- findAssocs(txtDTMa, 'never', 0.30)
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations),
                      row.names = NULL)
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() +
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3)

# End
