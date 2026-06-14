#' Purpose: Basic agentic ROUTING using OpenRouter cloud models.
#' A small "product manager" model decides whether a request is a coding task.
#' If so, it routes the work to a "smarter" coding model, which writes, saves,
#' and runs the R script. Otherwise the PM model just answers directly.
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 12, 2026
#'

# Libraries
library(httr2)
library(jsonlite)
library(stringr)

# Bring in the query_openrouter() helper (handles the OpenRouter API + key)
source('~/Desktop/GSERM_2025/openRouter_function.R')

# Inputs
#prompt <- "What is the capital of Brazil?"
#prompt <- 'Write ggplot2 code to make a scatter plot of 100 random numbers.  The plot should have a title that says 100 random values.'
prompt <- "Using the following text, write code to make a word cloud.  The text is: I love this course, it has been so hard but fun  I will bring this love of NLP back to my role for more fun."
savePath <- '~/Desktop/GSERM_2025/personalFiles/'

# LLM Names (OpenRouter model ids)
llmModel  <- 'google/gemini-3.1-flash-lite' # small, fast, cheap general purpose "PM"
codingLLM <- 'anthropic/claude-opus-4.8'      # "slower" but "smarter" specialist
# Tip: you can swap codingLLM for a dedicated coder, e.g. 'qwen/qwen-2.5-coder-32b-instruct'

###
# Now we have a simple agentic workflow where one agent is able to answer questions but
# it can decide to ask a "specialized" LLM for a task.

# Starting system prompt - sometimes this model will still write R code :( but its ignored at this stage
initialSystemPrompt <- "Do not write any code.  Only describe what needs to be done for a developer. You are a helpful, smart, kind, and efficient product manager AI assistant.  You do not know the R programming language and cannot write it.  You can describe what is needed functionally but not write code.  If the user's request is about writing code or programming in R, respond with 'CODE_TASK: ' followed by a precise description of the R coding task.  Do not write the code.  Instead respond with 'CODE_TASK:' followed by a functional specification. For example, if the user asks 'Write an R function to calculate factorial', you should respond\n\n 'CODE_TASK: Write an R function to create a scatter plot of random numbers'. \n\nIf the request is not about coding, answer the user's request directly and concisely."

initialLLMResponse <- query_openrouter(prompt = prompt,
                                       system_prompt = initialSystemPrompt,
                                       model = llmModel,
                                       max_tokens = 512)

# Now we classify the task
if(grepl('CODE_TASK:', initialLLMResponse)==T){
  cat('This looks like a coding task. I will ask the other LLM\n')
  codingSysPrompt <- "You are an expert R programmer. Your task is to write a complete, runnable R script based on the user's request. Enclose the R code within a markdown code block (``` ... ```). Do not include any explanations or conversational text outside of the code block. Ensure the script is self-contained and complete.  The code will be run with source().  DO NOT USE install.packages(), ONLY call library() when needed. If there is a final output of the code it must be printed, or called to display the output."

  # Route to the coding LLM. IMPORTANT: the PM only writes a high-level spec and
  # tends to drop literal details (like the exact text to use). So we hand the
  # coder BOTH the original user request AND the PM's spec, otherwise the code
  # gets built from the spec alone and ignores the user's actual data/text.
  # Increase max_tokens as code can be lengthy.
  codingRequest <- paste0("ORIGINAL USER REQUEST (use any literal data/text exactly as given here):\n",
                          prompt,
                          "\n\nPRODUCT MANAGER SPECIFICATION:\n",
                          initialLLMResponse)

  # Show students the exact handoff: the literal text is now included
  cat('\n--- HANDOFF TO CODING AGENT ---\n')
  cat(codingRequest)
  cat('\n-------------------------------\n')

  codingLLMResponse <- query_openrouter(prompt = codingRequest,
                                        system_prompt = codingSysPrompt,
                                        model = codingLLM,
                                        max_tokens = 2048)

  # Now we need to extract the code which should start ```r and end ``` delimiters
  # So I am searching for them and keeping the text between
  tmpCode <- capture.output(cat(codingLLMResponse))
  idx <- grep('```', tmpCode)
  if(length(idx)!=2){stop(paste('code syntax error from the coding LLM:',codingLLMResponse))}
  st <- idx[1]+1
  en <- idx[2]-1
  onlyCode <- paste(tmpCode[st:en], collapse = '\n')

  # Now let's save a copy of the automated script
  nam <- paste0('CODE_TASK_', Sys.time(),'.R') # Define the name of the file to save the script
  writeLines(onlyCode, paste0(savePath,nam))
  cat(paste('R script create called:', nam,'\n'))

  print('now running the saved R script\n')
  source(paste0(savePath,nam))
} else {
  print(initialLLMResponse)
}

# End
