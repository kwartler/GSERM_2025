#' Title: LLM Batch Processing: Brand NER & Text Extraction
#' Purpose: Concurrent LLM inference over a corpus of documents. For each
#'          document, identify brands and extract the text related to each
#'          brand. Results are unnested to long format (one row per
#'          doc_id x brand) and written to CSV. Requests are executed in
#'          batches with automatic backoff retry using httr2.
#' Author: Ted Kwartler

# Libraries
library(httr2)
library(jsonlite)

# ------------------------------------------------------------------------------
# 1. Inputs & Configuration
# ------------------------------------------------------------------------------
docURL <- "https://raw.githubusercontent.com/kwartler/GSERM_2025/refs/heads/main/lessons/Day2/X2026.06.09_openai.gpt.5.mini.csv"
docs   <- read.csv(docURL, stringsAsFactors = FALSE)

# Expecting columns: doc_id, text, model, date
# For class, optionally subset: docs <- docs[1:60, ]

llmModel  <- "google/gemini-3.1-flash-lite" # a smaller model will hallucinate sometimes we can try llama-3.2-1b-instruct
batchSize <- 10    # concurrent requests per batch
batchWait <- 1     # seconds to pause between batches
maxTokens <- 2000  # documents mention many brands; leave headroom

openrouterKey <- Sys.getenv("OPENROUTER_API_KEY")
if (openrouterKey == "") {
  stop("API Key missing! Please set OPENROUTER_API_KEY in your .Renviron file.")
}

# The NER instruction sent with every document
systemPrompt <- paste(
  "You are a precise named entity recognition system.",
  "Identify every brand or company mentioned in the user's document.",
  "For each brand, extract the verbatim text from the document that relates",
  "to that brand (the sentences or bullet fragments discussing it).",
  "Treat sub-brands and models under their parent brand",
  "(e.g. 'Model 3' belongs to Tesla).",
  "Respond with ONLY a valid JSON array, no markdown fences, no commentary.",
  "Schema: [{\"brand\": \"<brand name>\", \"extracted_text\": \"<verbatim related text>\"}]",
  "If no brands are present respond with []."
)

# ------------------------------------------------------------------------------
# 2. Build the Base Request Template
# ------------------------------------------------------------------------------
base_req <- request("https://openrouter.ai/api/v1/chat/completions") %>%
  req_headers(
    `Content-Type`  = "application/json",
    `Authorization` = paste("Bearer", openrouterKey),
    `HTTP-Referer`  = "http://localhost",
    `X-Title`       = "R Batch Brand NER"
  ) %>%
  # RETRY LOGIC: up to 3 tries on 429 / 5xx with exponential backoff
  req_retry(max_tries = 3, backoff = ~ 2^.x)

# ------------------------------------------------------------------------------
# 3. Generate a List of Custom Requests (one per document)
# ------------------------------------------------------------------------------
req_list <- lapply(seq_len(nrow(docs)), function(i) {
  
  body_payload <- list(
    model = llmModel,
    messages = list(
      list(role = "system", content = systemPrompt),
      list(role = "user",   content = docs$text[i])
    ),
    temperature = 0,      # deterministic-ish extraction, not creative writing
    max_tokens  = maxTokens,
    stream      = FALSE
  )
  
  base_req %>% req_body_json(body_payload)
})

# ------------------------------------------------------------------------------
# 4. Execute in Batches (scales to thousands of documents)
# ------------------------------------------------------------------------------
# Split request indices into chunks of batchSize
batchIdx  <- split(1:length(req_list), ceiling(1:length(req_list)/ batchSize))
responses <- vector("list", length(req_list))

cat("Processing", length(req_list), "documents in",
    length(batchIdx), "batches of up to", batchSize, "...\n")

for (b in 1:length(batchIdx)) {
  idx <- batchIdx[[b]]
  cat("  Batch", b, "of", length(batchIdx),
      "(docs", min(idx), "-", max(idx), ")\n")
  
  responses[idx] <- req_perform_parallel(req_list[idx], on_error = "continue")
  
  if (b < length(batchIdx)) Sys.sleep(batchWait)
}

# ------------------------------------------------------------------------------
# 5. Parse Responses & Unnest to Long Format (doc_id x brand)
# ------------------------------------------------------------------------------
parse_one <- function(res, doc_id) {
  
  # Request-level failure
  if (is.null(res) || inherits(res, "error") || resp_status(res) != 200) {
    return(data.frame(doc_id         = doc_id,
                      brand          = "REQUEST_ERROR",
                      extracted_text = "Request failed or timed out.",
                      stringsAsFactors = FALSE))
  }
  
  res_body   <- resp_body_json(res)
  clean_text <- trimws(res_body$choices[[1]]$message$content)
  
  # Models sometimes wrap JSON in markdown fences; strip them
  clean_text <- gsub("^```(json)?", "", clean_text)
  clean_text <- gsub("```$", "", clean_text)
  clean_text <- trimws(clean_text)
  
  parsed <- tryCatch(fromJSON(clean_text, simplifyDataFrame = TRUE),
                     error = function(e) NULL)
  
  # Parse-level failure: keep the raw text so it can be inspected or re-run
  if(is.null(parsed) || length(parsed) == 0) {
    return(data.frame(doc_id         = doc_id,
                      brand          = ifelse(is.null(parsed),"PARSE_ERROR", "NO_BRANDS"),
                      extracted_text = ifelse(is.null(parsed),
                                              clean_text, ""),
                      stringsAsFactors = FALSE))
  } else {
  
  # Successful parse: one row per brand found in the document
  data.frame(doc_id         = doc_id,
             brand          = parsed$brand,
             extracted_text = parsed$extracted_text,
             stringsAsFactors = FALSE)}
}

final_results <- lapply(1:length(responses), function(i) {
  parse_one(responses[[i]], docs$doc_id[i])
})

results_df <- do.call(rbind, final_results)

# ------------------------------------------------------------------------------
# 6. Verbatim Check: is the extracted text an exact substring of the source?
# ------------------------------------------------------------------------------
# LLM "verbatim" extraction can silently paraphrase. This flags any row where
# the extracted text is NOT found character-for-character in the original doc.
# Error and empty rows are marked NA since there is nothing to verify.
# **This is a STRICT check.  Small differences will be FALSE like normalizing a curly apostrophe to a straight one**
# vapply is a loop in disguise, this will apply the custom function to each row.
results_df$verbatim_match <- vapply(1:nrow(results_df), function(i) {
  
  if (results_df$brand[i] %in% c("REQUEST_ERROR", "PARSE_ERROR", "NO_BRANDS")) {
    return(NA)
  }
  
  srcText <- docs$text[docs$doc_id == results_df$doc_id[i]][1]
  grepl(results_df$extracted_text[i], srcText, fixed = TRUE) #fixed means treat it as a character string NOT regex
  
}, logical(1)) #enforce that each evaluation returns a single T/F just in case

# ------------------------------------------------------------------------------
# 7. Review & Save
# ------------------------------------------------------------------------------
cat("\nDocuments processed :", length(unique(results_df$doc_id)), "\n")
cat("Brand rows extracted:", sum(!results_df$brand %in%
                                   c("REQUEST_ERROR", "PARSE_ERROR")), "\n")
cat("Errors              :", sum(results_df$brand %in%
                                   c("REQUEST_ERROR", "PARSE_ERROR")), "\n")
cat("Verbatim match rate :",
    round(mean(results_df$verbatim_match, na.rm = TRUE) * 100, 1), "%\n\n")

print(head(results_df, 15))

outFile <- paste0("brand_NER_results_", Sys.Date(), ".csv")
write.csv(results_df, outFile, row.names = FALSE)
cat("\nSaved:", outFile, "\n")

# To re-run only the failures:
# failedIds <- unique(results_df$doc_id[results_df$brand %in%
#                       c("REQUEST_ERROR", "PARSE_ERROR")])
# docs <- docs[docs$doc_id %in% failedIds, ]  then re-run from section 3
# End