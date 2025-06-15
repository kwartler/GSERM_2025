#' Purpose: Comparative visualizations for text
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 14, 2025
#'

# Data Input, locally you can use list.files()
mortgageLoans  <- 'https://raw.githubusercontent.com/kwartler/GSERM_2024/main/lessons/Day2_Visualizations/data/mortgageLoan_2024.csv'
studentLoans   <- 'https://raw.githubusercontent.com/kwartler/GSERM_2024/main/lessons/Day2_Visualizations/data/studentLoan_2024.csv'
txtFiles <- c(mortgageLoans, studentLoans)

# Libs
library(tm)
library(plotrix)
library(ggplot2)
library(ggthemes)

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
stops <- c(stopwords('english'), 'mortgage', 'student','loans', 'loan')


# Read in Data, collapse, clean & organize.  For advanced programming students, this could be done with a custom function
textA <- read.csv(txtFiles[1])
textA <- complaintSubstitutions(textA$Consumer.complaint.narrative)
textA <- paste(textA, collapse = ' ')
textA <- VCorpus(VectorSource(textA))
textA <- DocumentTermMatrix(cleanCorpus(textA, stops))
textA <- as.matrix(textA)

# Repeat
textB <- read.csv(txtFiles[2])
textB <- complaintSubstitutions(textB$Consumer.complaint.narrative)
textB <- paste(textB, collapse = ' ')
textB <- VCorpus(VectorSource(textB))
textB <- DocumentTermMatrix(cleanCorpus(textB, stops))
textB <- as.matrix(textB)

# Find terms in common
commonCols <- intersect(colnames(textA), colnames(textB))

# Merge the matrices
mergedDF <- rbind(textA[,commonCols], textB[,commonCols])
rownames(mergedDF) <- c('Mortgage','Student')

# Examine
head(mergedDF[1:2,1:10])

# Transpose for the graphing later; could have made a TDM though
mergedDF <- t(mergedDF)
head(mergedDF)

# Calculate the absolute differences among in common terms
difference <- abs(mergedDF[,1] - mergedDF[,2])

# Create the plotting data frame
df <- data.frame(terms      = rownames(mergedDF),
                 mortgage   = mergedDF[,1],
                 student    = mergedDF[,2],
                 difference = difference,
                 row.names  = NULL)
head(df)


# Organize df for plotting
df<- df[order(df$difference, decreasing=TRUE), ]
topTerms <- df[1:10, ]

# Pyarmid Plot
pyramid.plot(lx         = topTerms$mortgage, #left
             rx         = topTerms$student,    #right
             labels     = topTerms$terms,  #terms
             top.labels = c('Mortgage', 'Terms', 'Student'), #corpora
             gap        = 1000, # space for terms to be read
             main       = 'Words in Common', # title
             unit       = 'wordFreq')

# ggplot interface
p <- ggplot(topTerms, aes(x = terms)) +
  geom_bar(aes(y = -student, fill = "Student"),
           stat = "identity", width = 0.8) +
  geom_bar(aes(y = mortgage, fill = "Mortgage"),
           stat = "identity", width = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("blue", "red"), name = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "")
p

# Change the aesthetics
p2 <- ggplot(topTerms, aes(x = terms)) +
  geom_bar(aes(y = mortgage, fill = "Mortgage"),
           stat = "identity", width = 0.4, alpha = 0.5) +
  geom_bar(aes(y = -student, fill = "Student"),
           stat = "identity", width = 0.4, alpha = 0.5) +
  geom_point(data = topTerms, aes(y = mortgage), color = 'blue', alpha = 0.75) +
  geom_point(data = topTerms, aes(y = -student), color = 'red', alpha = .75) +
  geom_text(aes(y = 0, label = terms), size = 3, nudge_y=0, color = 'black', fontface='bold') +
  coord_flip() +
  scale_fill_manual(values = c("blue", "red"), name = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "") +
  theme(axis.text.y=element_blank()) +
  ggtitle('Comparing Mortgage Complaints to Student Loan Complaints')
p2

# End
