#' Purpose: Apply syntactic parsing with UD pipe
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: june 24, 2025
#'

# Libs
library(udpipe)
library(tm)
#library(qdap)
library(reshape2)

# Inputs
datPth          <- 'https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/tweet_jailbolsonaro_vector.csv'
testing         <- T
nonBasicStops   <- c('segura', 'seguro')
folderPath      <- '~/Desktop/GSERM_2025/personalFiles/'

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
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  return(corpus)
}

# Get a language model to the server
?udpipe_download_model
udModel <- udpipe_download_model(language  = "portuguese-gsd",
                                 model_dir = folderPath)

# Load into the space
udModel <- udpipe_load_model(udModel$file_model)
udModel <- udpipe_load_model('~/Desktop/GSERM_2025/personalFiles/portuguese-gsd-ud-2.5-191206.udpipe')

# Bring in data & organize
textData <- read.csv(datPth)
textData$text <- stringi::stri_encode(textData$text, "", "UTF-8")


# Convert so my computer can handle non English characters
#Encoding(textData$text)
#iconvlist()
#stringi::stri_enc_detect(textData$text)
txt <- iconv(textData$text, "latin1", "ISO-8859-1", sub="")

# Apply the cleaning function, then get the plain text version
textCorp <- VCorpus(VectorSource(txt))
textCorp <- cleanCorpus(textCorp, c(stopwords('portuguese'),nonBasicStops))
text     <- sapply(textCorp, NLP::content)

# Re-organize to keep track of doc_id
text <- data.frame(doc_id = 1:nrow(textData),
                   text   =text)

# UD Pipe require UTF8 so this may be needed to enforce it.
text$text <- enc2utf8(text$text)

# Compare; sometime R can't parse certain characters
text$text[577] # cleaned
textData$text[577] # raw

# The code may error out if there is a blank document ie only stopwords that were removed.
#text$text <- ifelse(nchar(text$text)==0,'blank document',text$text)

# Reduce for testing
nDocs            <- ifelse(testing ==T, 2, nrow(text))

# Get the dependencies
syntatcicParsing <- udpipe(text[1:nDocs,], object = udModel)
head(syntatcicParsing)
tail(syntatcicParsing)

# ID and replace any non-lemma terms
syntatcicParsing$cleanTxt <- ifelse(is.na(syntatcicParsing$lemma),
                                    syntatcicParsing$token,
                                    syntatcicParsing$lemma)

# Aggregate back to the document level
lemmaText        <- aggregate(syntatcicParsing$cleanTxt,
                              list(syntatcicParsing$doc_id), paste, collapse=" ")

names(lemmaText) <- c('doc_id', 'text')

# Compare lemmatization "mulheres" to "mulher"; "demorando" to "demorar"
# From here you can reapply to get a lemmatized version of a DTM, using tokenization etc
text[1,2]
lemmaText[1,2]

# You can also get dense data about the document
# Codes can be found here: https://universaldependencies.org/u/dep/
originalLemmaTxt <- reshape2::dcast(syntatcicParsing, doc_id ~  xpos)
originalLemmaTxt <- originalLemmaTxt[order(as.numeric(originalLemmaTxt$doc_id)),]
head(originalLemmaTxt)

# End
