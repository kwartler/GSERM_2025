#' Purpose: using a local LLM extract information from journal abstracts
#' TK
#'June 27, 2025
#'
#' Notes: If you're local LLM isn't good enough for the task you can adjust this function so that you use a commercial LLM.  You would need an API and $.  In Day4, "Z_openAI_batch_processing.R" has an example of how to do this or you can adjust this script to include an API key, different model name like 4o-mini and POST url (which would point to openAI for example).


# Inputs
allAbstracts      <- list.files(path = '~/Desktop/GSERM_2025/studentQuestions/abstractArticles',
                                pattern = 'article',
                                full.names = T)
sysPromptExample  <- readLines('~/Desktop/GSERM_2025/studentQuestions/abstractArticles/exampleSysPrompt.txt')
sysPromptExample  <- paste(sysPromptExample, collapse = '\n')
savePath          <- '~/Desktop/GSERM_2025/studentQuestions/abstractArticles/LLMresults/' #remember the last /
llmModel          <- 'qwen2.5-7b-instruct'
temp              <-  0.7
maxTokens         <- 512

# We could load them all but that is computationally intensive so now I will work in series
# Custom function
methodAbstract <- function(filePath, 
                           llmModel =  'llama-3.2-1b-instruct', 
                           temp = 0.7, 
                           maxTokens = 512,
                           sysPrompt, 
                           savePath){
  require(httr)
  require(jsonlite)
  
  print(paste('working on',filePath ))
  x <- readLines(filePath)
  x <- paste(x, collapse ='\n')
  
  # Organize Request
  dataLLM <- list(model = llmModel,
                  messages = list(
                    list(role = "system", content = sysPrompt),
                    list(role = "user", content = x)),
                  temperature = temp,
                  max_tokens = maxTokens,
                  stream = FALSE)
  # Request header
  headers <- c(`Content-Type` = "application/json")
  # Make the POST request
  res <- httr::POST(url = "http://localhost:1234/v1/chat/completions",
                  httr::add_headers(.headers = headers),
                  body = toJSON(dataLLM, auto_unbox = TRUE))
  # Extract the response
  llmResponse <- httr::content(res)$choices[[1]]$message$content
  response <- data.frame(file = filePath,
                         llmOutput = llmResponse)
  
  # Construct the results file name
  nam <- tail(unlist(strsplit(filePath, '/')[[1]]),1)
  nam <- paste0(nam,'_LLM_results_', Sys.time(),'.csv')
  nam <- paste0(savePath, nam)
  write.csv(response, nam, row.names = F)
  
  return(response)
}

# Test it on one file
methodAbstract(filePath = allAbstracts[4], 
               llmModel =  llmModel, 
               temp = temp, 
               maxTokens = maxTokens,
               sysPrompt = sysPromptExample, 
               savePath = savePath)

# Then delete the saved single file for testing and rerun for all
for(i in 1:length(allAbstracts)){
  oneAbstract <- allAbstracts[i]
  methodAbstract(filePath = oneAbstract, 
                 llmModel =  llmModel, 
                 temp = temp, 
                 maxTokens = maxTokens,
                 sysPrompt = sysPromptExample, 
                 savePath = savePath)
  
}


# End