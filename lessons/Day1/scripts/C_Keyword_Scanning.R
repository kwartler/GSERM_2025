#' Title: Intro: Keyword Scanning
#' Purpose: Learn some basic string manipulation
#' Author: Ted Kwartler
#' Date: June 11, 2025
#'

# Libs
library(stringi)

# Options & Functions
Sys.setlocale('LC_ALL','C') #some tweets are in different languages so you may get an error

# Get Data
text <- read.csv('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/coffeeVector.csv')
head(text$x)


# Logical T/F vector that a string appears at least ONCE
coffee    <- grepl("coffee", text$x, ignore.case=TRUE)
starbucks <- grepl("starbucks", text$x, ignore.case=TRUE)

# Find the row positions of a specific word appearing at least ONCE
#this shows the difference between grep and grepl
grep("mug", text$x, ignore.case=TRUE)

# Grep for indexing; better when you have more information like author and time
text[grep('mug', text$x),]

# Logical T/F for one word OR another appears at least ONCE
keywords    <-"mug|glass|cup"
mugGlassCup <-grepl(keywords, text$x,ignore.case=TRUE)

# Calculate the % of times among all tweets
sum(coffee) / nrow(text)
sum(starbucks) / nrow(text)
sum(mugGlassCup) / nrow(text)

# Count occurrences of words per tweet
theCoffee <- stri_count(text$x, fixed="the")
sum(theCoffee) / nrow(text)

# Example data organization
keywordScans <- data.frame(text = text$x, coffee, starbucks, mugGlassCup)
keywordScans

# Substitute the first pattern found in a document
text$x[4]
sub('mug', 'cup', text$x[4], ignore.case=TRUE)

# Substitute all the patterns found
text[grep('mug', text$x),]
gsub('mug', 'cup', text[grep('mug', text$x),], ignore.case=TRUE)

# Shown another way
fakeTweet <- 'This is a good coffee mug.  It is a mug of hot coffee.'
sub('mug', 'cup', fakeTweet, ignore.case=TRUE)
gsub('mug', 'cup', fakeTweet, ignore.case=TRUE)


# End
