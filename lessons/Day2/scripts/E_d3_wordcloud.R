#' Purpose: Make a d3 wordcloud webpage
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 14, 2025
#'

# Declare the data path
filePath  <- 'https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/chardonnay.csv'

# Libs
library(tm)
library(wordcloud2)
library(RColorBrewer)
library(echarts4r)
library(dplyr)
library(stringi)

# Custom functions
# Robust to lower
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
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


# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "),
         use.names = FALSE)}

# Create custom stop words
stops <- c(stopwords('english'), 'lol', 'amp', 'chardonnay')

# Data
text <- read.csv(filePath)

# Fix the encoding for some students
text$text <- stringi::stri_encode(text$text, "", "UTF-8")

# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(text$text))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, stops)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
txtDTM  <- DocumentTermMatrix(txtCorpus,
                               control=list(tokenize=bigramTokens))
txtDTMm <- as.matrix(txtDTM)

# Get Column Sums & organize
txtSums <- colSums(txtDTMm)
txtFreq <- data.frame(word=names(txtSums),
                      frequency=txtSums,
                      row.names = NULL)
txtFreq <- txtFreq[order(txtFreq$frequency, decreasing = T),]

# Regular dynamic WC, click the pop-out in the viewer
wordcloud2(data = txtFreq[1:10,])
?wordcloud2

# Choose a color & drop light ones
pal <- brewer.pal(8, "Dark2")
wordcloud2(txtFreq[1:50,],
           color = pal,
           backgroundColor = "lightgrey")

# Some built in shapes need to click "show in new window"
# 'circle', 'cardioid', 'diamond', 'triangle-forward', 'triangle', 'pentagon', & 'star'
wordcloud2(txtFreq[1:50,],
           shape = "diamond",
           color = "blue",
           backgroundColor = "pink")

# Now let's use a more up to date package echarts4r
txtFreq[1:50,] %>%
  e_color_range(frequency, color, colors = c("tomato", "goldenrod")) %>% # Append the column name color, with the colors hex codes
  e_charts() %>%
  e_cloud(
    word = word,
    freq = frequency,
    color = color,
    shape = "circle",
    rotationRange = c(0, 0),
    sizeRange = c(15, 100)
  ) %>%
  e_tooltip() %>%
  e_title("Chardonnay")


# End
