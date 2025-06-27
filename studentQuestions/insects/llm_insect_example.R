#' Purpose: Using a local model let's identify specific elements from lengthy texts
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 27, 2025
#'

# Inputs
allFiles      <- list.files(path = '~/Desktop/GSERM_2025/studentQuestions/insects',
                                pattern = '.txt',
                                full.names = T)
sysPromptExample  <- 'You are a PhD expert in the field of plants and insects.  You extract insect names from text.  You have to review text and ONLY respond with the name, either common or Latin for insects and bugs that you have identified within the text.  Do NOT respond with any other information.  Only respond with the names of insects in Latin or if mentioned with a common name you would note that too. For example after reviewing text you would respond with:\n\nApis mellifera mellifera\nFormica polyctena\nGiant House Spide\n\nBelow is text to examine:\n\n'
savePath          <- '~/Desktop/GSERM_2025/studentQuestions/insects/LLMresults/' #remember the last /
llmModel          <- 'qwen2.5-7b-instruct'
temp              <-  0.7
maxTokens         <- 512*2
nCharChunk        <- 10000 # characters overlap
chunkOverlap      <- 0.1 # between 0 [distinct] - 1 [all chunks the same]

insectAbstract <- function(filePath, 
                           llmModel =  'llama-3.2-1b-instruct', 
                           temp = 0.7, 
                           maxTokens = 512,
                           sysPrompt, 
                           savePath,
                           nCharChunk = 10000,
                           chunkOverlap = 0.1,
                           testing = T){
  require(httr)
  require(jsonlite)
  
  print(paste('working on',filePath ))
  x <- readLines(filePath)
  x <- paste(x, collapse ='\n')
  
  # We must chunk up the data using this function
  chunkDocument <- function(textVector, chunkSize = 3500, overlap = 0.1) {
    n <- nchar(textVector)
    stepSize <- chunkSize * (1 - overlap)
    startPositions <- seq(1, n, stepSize)
    endPositions <- pmin(startPositions + chunkSize - 1, n)
    chunks <- substring(textVector, startPositions, endPositions)
    return(chunks)
  }
  chunks <- chunkDocument(x,
                          chunkSize = nCharChunk, 
                          overlap = chunkOverlap)
  if(testing == T){
    warning('Only using 3 chunks for testing purposes.  If you want a complete run, set testing to F')
    chunks <- chunks[1:3]
  }
  
  chunkResults <- list()
  for (j in 1:length(chunks)) {
    print(paste('chunk:',j,'of',length(chunks)))
    dataLLM <- list(
      model = llmModel,
      messages = list(
        list(role = "system", content = sysPrompt),
        list(role = "user", content = chunks[j])
      ),
      temperature = 0.7,
      max_tokens = 512,
      stream = FALSE
    )
    
    headers <- c(`Content-Type` = "application/json")
    res <- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                      httr::add_headers(.headers = headers),
                      body = toJSON(dataLLM, auto_unbox = TRUE))
    
    llmResponse <- httr::content(res)$choices[[1]]$message$content
    chunkResults[[j]] <- data.frame(
      urlFile = filePath,
      chunk_id = j,
      llmClassification = llmResponse
    )
  }
  chunkResultsDF <- do.call(rbind, chunkResults)
  
 
  
  # Construct the results file name
  nam <- tail(unlist(strsplit(filePath, '/')[[1]]),1)
  nam <- paste0(nam,'_LLM_results_', Sys.time(),'.csv')
  nam <- paste0(savePath, nam)
  write.csv(chunkResultsDF, nam, row.names = F)
  return(chunkResultsDF)
}

# Test it on one file
insectAbstract(filePath = allFiles[1], 
               llmModel =  llmModel, 
               temp = temp, 
               maxTokens = maxTokens,
               sysPrompt = sysPromptExample, 
               savePath = savePath,
               nCharChunk = nCharChunk,
               chunkOverlap = chunkOverlap,
               testing = T)

# Then delete the saved single file for testing and rerun for all
# with testing = F
# I am using a smaller, less accurate model for this complete run but 
# you would likely not want to do this
# This code would work with more than one document in the folder but 
# since it is just 1, length(allFiles) is 1 and the loop only 
# runs once
for(i in 1:length(allFiles)){
  print(paste('working on file:'),allFiles[i])
  oneAbstract <- allFiles[i]
  insectAbstract(filePath = oneAbstract[i], 
                 llmModel =  'llama-3.2-1b-instruct', 
                 temp = temp, 
                 maxTokens = maxTokens,
                 sysPrompt = sysPromptExample, 
                 savePath = savePath,
                 nCharChunk = nCharChunk,
                 chunkOverlap = chunkOverlap,
                 testing = F)
  }

# End