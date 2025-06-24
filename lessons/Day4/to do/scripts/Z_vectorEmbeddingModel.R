LLMdata <- list(input = 'vectorize this text')

jsonData <- jsonlite::toJSON(LLMdata, auto_unbox = TRUE)

res     <- httr::POST(url = 'http://localhost:1234/v1/embeddings',
                      
                      httr::add_headers(.headers = headers),
                      
                      body = jsonData)



llmResponse <- unlist(content(res)$data[[1]]$embedding)

t(llmResponse)