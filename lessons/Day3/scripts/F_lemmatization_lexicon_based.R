#' Purpose: Apply syntactic parsing with UD pipe
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 24, 2025
#'

# Libs
library(lexicon)
library(tm)
library(mgsub)
library(qdapRegex)
library(pbapply)

# Inputs
datPth          <- 'https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/Guardian_text.csv'
nonBasicStops   <- c('am', 'gmt', 'pakistan', 'pakistani')
testing         <- T
folderPath      <- '~/Desktop/GSERM_2025/personalFiles/'

# Let's examine the base lemma lexicon
data(hash_lemmas)
hash_lemmas
class(hash_lemmas)

# Custom functions
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_bracket)) # new
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Bring in data & organize
textData <- read.csv(datPth)

if(testing == T){
  idx <- order(nchar(textData$body))
  textData <- textData[idx[1:2],] #find 2 shortest examples to show
  }

# Let's learn about gsub & regex; pattern "anchoring"
exampleTxt <- 'RT I love the Statue of Liberty RT'
gsub('rt','', exampleTxt)
gsub('rt','', exampleTxt, ignore.case = T)
gsub('^RT','' ,exampleTxt) #beginning-of-string anchor ^
gsub('\\bRT\\b','' ,exampleTxt) # escaped "\b" #word boundary anchor, we can also use ignore.case = TRUE
gsub('^RT I love the Statue of Liberty RT$', #^ beginning, $ ending with no variation
     'I love the Stature of Liberty' ,
     exampleTxt) # this works if the entire string is to be subsituted exactly


# Perform the dictionary based lemmatization
# Original:   <p>The Taliban has claimed responsibility 
# Lemmatized: <p>The Taliban ~have~ ~claim~ responsibility
mgsub::mgsub(textData$body[1], 
             paste0('\\b',hash_lemmas$token, '\\b'),
             paste0('~',hash_lemmas$lemma,'~'),  # replace w/spaces for real, just for learning
             ignore.case = T) 

# Now do it for real; TAKES AGES so we invoke a progress bar; ~30sec/short article
lemmaText <- pblapply(
  X = textData$body,
  FUN = mgsub, # Pass mgsub function directly
  pattern = paste0('\\b',hash_lemmas$token, '\\b'),
  replacement = paste0('~',hash_lemmas$lemma,'~'),
  ignore.case = TRUE)

# W/O the progress bar, will look like R is frozen
#lemmaText <- mgsub(textData$body, 
#                    paste0('\\b',hash_lemmas$token, '\\b'),
#                    paste0('~',hash_lemmas$lemma,'~'),
#                    ignore.case = T) 

# Apply the cleaning function, then get the plain text version
lemmaText <- unlist(lemmaText) #list to vector
textCorp <- VCorpus(VectorSource(lemmaText))
textCorp <- cleanCorpus(textCorp, c(stopwords('SMART'),nonBasicStops))
text     <- sapply(textCorp, NLP::content)

# Re-organize to keep track of doc_id
text <- data.frame(doc_id = 1:nrow(textData),
                   text   =text)

# Compare Original to Lemma & Processed
textData$body[1]
text$text[1]

# Now we can just save a copy for our analysis to begin
write.csv(text, paste0(folderPath,'/example_lexicon_lemmatization.csv'), row.names = F)

# End
