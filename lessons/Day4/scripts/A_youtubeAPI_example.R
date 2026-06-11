#' Title: Grab Youtube JSON
#' Purpose: Demonstrate f12 in Chrome for API
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: June 10, 2026
#'

# Libraries
library(jsonlite)
library(stringr)
library(plyr)

# Youtube URL
# https://www.youtube.com/watch?v=K5Rly83zfuI&ab_channel=TheDailyShowwithTrevorNoah
# https://www.youtube.com/watch?v=sal78ACtGTc&ab_channel=SequoiaCapital
youtubeCaption <- 'https://www.youtube.com/api/timedtext?v=K5Rly83zfuI&ei=D20qav3TCsncmLAPo9qcsQo&caps=asr&opi=112496729&exp=xpe&xoaf=4&xowf=1&hl=en&ip=0.0.0.0&ipbits=0&expire=1781190527&sparams=ip%2Cipbits%2Cexpire%2Cv%2Cei%2Ccaps%2Copi%2Cexp%2Cxoaf&signature=4FABCAE41F840F264D44BF9D84B168D2A8A73CF2.67B7B1E609FA3404511D29A4805DE8DC560EEDD6&key=yt8&lang=en-US&potc=1&pot=MlM3qvwfsr-lDTeW5EJrNCq7Z1BTZdbfqjy_EUJQBGEOpEILHv-ZtY0Ly4F-56ZQzTqjfINbDzIhKVQ6f5rPoOLLcN3Cw7MYcyHvIn2nfYwtfS7vmQ%3D%3D&fmt=json3&xorb=2&xobt=3&xovt=3&cbrand=apple&cbr=Chrome&cbrver=148.0.0.0&c=WEB&cver=2.20260606.02.00&cplayer=UNIPLAYER&cos=Macintosh&cosver=10_15_7&cplatform=DESKTOP'

# Go get the data
dat <- fromJSON(youtubeCaption) # you can even pass in a URL to go to a webpage

# closed captioning data
dat$events$tStartMs
dat$events$dDurationMs
dat$events$segs[1:10]

# Get each first column called utf8
rawTxt <- lapply(dat$events$segs, "[", 'utf8') 

# organize just the single column
rawTxt <- do.call(rbind, rawTxt)

# Drop line returns "\n"
rawTxt <- gsub('[\r\n]',' ',rawTxt[,1])

# Sometimes there are entries that are empty so they need to be dropped
head(rawTxt,10)
rawTxt <- rawTxt[nchar(rawTxt) != "0"]

# Sometimes, there is extra spacing from the gsub
rawTxt <- str_squish(rawTxt)

# If you want it as a single chunk
oneChunk <- paste(rawTxt, collapse = ' ')

# If you want to retain the meta data
tmpText <- lapply(dat$events$segs, "[", 'utf8')
tmpTextList <- list()
for(i in 1:length(tmpText)){
  if(is.null(tmpText[[i]])){
    tmp <- 'NULL'
  } else {
    tmp <- apply( tmpText[[i]], 2, paste, collapse = ' ')
    tmp <- trimws(tmp)
  }
  tmpTextList[[i]] <- tmp
  
}


textDF <- data.frame(startTime = dat$events$tStartMs/1000,
                     duration  = dat$events$dDurationMs/1000,
                     text = unlist(tmpTextList))

# Examine to make sure format is ok
head(textDF, 10)

# End
