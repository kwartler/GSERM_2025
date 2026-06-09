#' TK
#' Date June 9 2026
#' Use LLM to explore branding in LLMS 

# Use our function to get N LLM responses about a topic
source("~/Desktop/GSERM_2025/openRouter_function.R")

# clean corpus and try to lower functions

# libraries


# Inputs
topicSubject <- #'what is the best laptop to buy for a student?'
  'What is the best electric car to buy in Switzerland right now?'
modelLLM     <- 'openai/gpt-5-mini'#'deepseek/deepseek-v4-flash' #
temperature  <- 0.7
sysPrompt    <- 'You are a helpful AI assistant.  Be honest about brand recommendations.'
nResponses   <- 60

# Test 
tmp <- query_openrouter(prompt = topicSubject,
                        system_prompt = sysPrompt,
                        model = modelLLM, 
                        temperature = temperature)

# Now get the corpus
allResponses <- list()
for(i in 1:nResponses){
  print(paste('obtaining response',i,'from', modelLLM, 'out of', nResponses))
  tmp <- query_openrouter(prompt = topicSubject,
                          system_prompt = sysPrompt,
                          model = modelLLM, 
                          temperature = temperature)
  allResponses[[i]] <- tmp
}
# Organize
allResponsesDF <- do.call(rbind, allResponses)

# Drop line breaks
allResponsesDF <- gsub('\n','', allResponsesDF)

allResponsesDF <- data.frame(doc_id = 1:nResponses,
                             text   = allResponsesDF,
                             model  = rep(modelLLM,nResponses),
                             date   = rep(Sys.Date(), nResponses))

# Save a copy just for class
nam <- make.names(paste0(Sys.Date(),'_', modelLLM,'.csv'))
write.csv(allResponsesDF, nam, row.names = F)
allResponsesDF <- read.csv('https://raw.githubusercontent.com/kwartler/GSERM_2025/refs/heads/main/lessons/Day2/X2026.06.09_openai.gpt.5.mini.csv')

# Prepare corpus

# frequency bar chart

# association

# dendrogram

# static wordcloud

# d3 wordcloud

 # End