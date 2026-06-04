#' Author: Ted Kwartler
#' June 12, 2025
#' An example for students using JAN AI locally.

# Libraries
library(httr)
library(jsonlite)

# Inputs
prompt <- "What is the capital of Brazil?" # Put your prompt here
apiKey <- "1"
llmModel <- "lmstudio-community:Qwen2.5-7B-Instruct-GGUF:Qwen2.5-7B-Instruct-Q4_K_M.gguf"#"qwen2.5:7b" #"llama3.2-1b-instruct"

urlAPI <- "http://127.0.0.1:1337/v1/chat/completions"

# Construct the body of your message
body <- list(
  messages = list(
    list(role = "system", content = "You are a helpful and concise assistant."),
    list(role = "user", content = prompt)
  ),
  model = llmModel,
  max_tokens = 512, # Controls the maximum number of tokens in the response
  temperature = 0.7  # Controls the creativity/randomness of the response
)
response <- POST(
  urlAPI,
  body = jsonlite::toJSON(body, auto_unbox = TRUE),
  content_type_json(),
  accept_json(),
  add_headers(Authorization = paste("Bearer", apiKey)) 
)
content(response)
httr::status_code(response)
stop_for_status(response)

# End