#' Query OpenRouter LLMs
#'
#' @param prompt Character string. The prompt to send to the model.
#' @param model Character string. The OpenRouter model string. Defaults to a fast, cost-effective model.
#' @param temperature Numeric. Controls randomness (0.0 to 2.0). Default is 0.7.
#'
#' @return A character string containing the model's response.
#' @export
query_openrouter <- function(prompt, 
                             model = "google/gemini-2.5-flash", 
                             temperature = 0.7) {
  # 1. Ensure required packages are loaded
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("The 'httr2' package is required. Please run: install.packages('httr2')")
  }
  
  # 2. Safely retrieve the environment variable
  api_key <- Sys.getenv("OPENROUTER_API_KEY")
  if (api_key == "") {
    stop("API Key not found! Please ensure you have set OPENROUTER_API_KEY in your .Renviron file and restarted R.")
  }
  
  # 3. Construct the payload matching OpenRouter's required schema
  body_payload <- list(
    model = model,
    temperature = temperature,
    messages = list(
      list(role = "user", content = prompt)
    )
  )
  
  # 4. Build and execute the request
  req <- httr2::request("https://openrouter.ai/api/v1/chat/completions") %>%
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `HTTP-Referer` = "https://harvard.edu",      # Identifies your institution to OpenRouter
      `X-Title` = "Generative AI Class Practice"   # Names your app/course in the dashboard
    ) %>%
    httr2::req_body_json(body_payload) %>%
    httr2::req_retry(max_tries = 3) # Resilient against minor network hiccups
  
  # 5. Perform request and safely parse the nested JSON response
  tryCatch({
    resp <- httr2::req_perform(req)
    resp_data <- httr2::resp_body_json(resp)
    
    # Extract the message content string from the OpenRouter response object
    completed_text <- resp_data$choices[[1]]$message$content
    return(completed_text)
    
  }, error = function(e) {
    message("An error occurred during the API call:")
    print(e)
    return(NULL)
  })
}

# Trying a different model with a lower temperature for structured output
creative_response <- query_openrouter(
  prompt = "List 3 common data cleaning steps for text data.",
  model = "meta-llama/llama-3-8b-instruct:free",
  temperature = 0.2
)
cat(creative_response)