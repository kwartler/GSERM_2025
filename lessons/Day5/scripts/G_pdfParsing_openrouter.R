#' Purpose: Parse a PDF two ways - fast local text extraction OR a multimodal
#' (vision) model via OpenRouter that reads text, images and tables from page images.
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 12, 2026
#'

# Libraries
library(httr)
library(jsonlite)
library(pdftools)
library(base64enc)

# Inputs
pdfPath  <- '~/Desktop/GSERM_2025/personalFiles/example.pdf'
savePath <- '~/Desktop/GSERM_2025/personalFiles/'
tmpDir   <- '~/Desktop/GSERM_2025/personalFiles/pdfPages/' # temp page images land here; auto-deleted on success
visionModel <- 'google/gemini-3.1-flash-lite' # multimodal, fast + cheap
imgDPI <- 150 # 150-200 is the sweet spot for small text vs token cost

# THE ROUTING SWITCH:
# simplePDF <- TRUE  -> use pdftools (fast, free, deterministic).
#   Use when the PDF is "born digital" with a real text layer and simple layout.
# simplePDF <- FALSE -> render each page to an image and send to a vision model.
#   Use for scanned/image-only PDFs, complex layouts, or when you need tables and
#   figures interpreted (slower + costs API tokens, but far more capable).
simplePDF <- FALSE

# OUTPUT FORMAT for Route B (the vision model only):
# jsonOutput <- FALSE -> human-readable markdown (tables as | pipe | tables |). Saved as .md
# jsonOutput <- TRUE  -> structured JSON per page with keys: text, tables, figures.
#   Use when you want to PROGRAM against the result, e.g. turn tables into data frames. Saved as .json
jsonOutput <- FALSE

# Retrieve API Key securely from the environment
openrouterKey <- Sys.getenv("OPENROUTER_API_KEY")
if (openrouterKey == "") {
  stop("API Key missing! Please set OPENROUTER_API_KEY in your .Renviron file.")
}

# Request header
headers <- c(
  `Content-Type`  = "application/json",
  `Authorization` = paste("Bearer", openrouterKey),
  `HTTP-Referer`  = "http://localhost",
  `X-Title`       = "R Student Script"
)

if(simplePDF == TRUE){

  # ROUTE A: local text extraction, one string per page
  parsedPages <- pdf_text(pdfPath)

} else {

  # ROUTE B: multimodal vision parsing, one API call per page

  # Pick the parser instructions based on the desired output format
  if(jsonOutput == TRUE){
    visionSysPrompt <- "You are a document parser. Return ONLY a valid JSON object for this page with keys: 'text' (a string of all readable text), 'tables' (an array of tables, where each table is an array of row arrays), and 'figures' (an array of short string descriptions). Use empty arrays where an element is absent."
  } else {
    visionSysPrompt <- "You are a document parser. Extract ALL readable text exactly. Convert any tables to clean markdown tables. Briefly describe any images or figures. Return only the parsed content."
  }

  # Make the temp image folder if needed
  dir.create(tmpDir, showWarnings = FALSE, recursive = TRUE)

  nPages <- pdf_info(pdfPath)$pages
  parsedPages <- character(nPages)

  for(i in 1:nPages){
    cat('Parsing page', i, 'of', nPages, '\n')

    # Render the page to a PNG in tmpDir, then base64 encode it as a data URI
    pngFile <- pdf_convert(pdfPath, format = 'png', pages = i, dpi = imgDPI,
                           filenames = paste0(tmpDir, 'page_', i, '.png'),
                           verbose = FALSE)
    dataURI <- paste0('data:image/png;base64,', base64encode(pngFile))

    # NOTE (different from our text-only calls!): a multimodal request puts the
    # user 'content' as a LIST of typed blocks - a 'text' block plus an
    # 'image_url' block holding the base64 data URI. That mixed content is what
    # makes this a vision call rather than a plain chat call.
    dataLLM <- list(model = visionModel,
                    messages = list(
                      list(role = "system", content = visionSysPrompt),
                      list(role = "user", content = list(
                        list(type = "text", text = "Parse this page."),
                        list(type = "image_url", image_url = list(url = dataURI))))),
                    max_tokens = 2048)

    # Ask the API to guarantee valid JSON back when we want structured output
    if(jsonOutput == TRUE){ dataLLM$response_format <- list(type = "json_object") }

    res <- httr::POST(url = "https://openrouter.ai/api/v1/chat/completions",
                      httr::add_headers(.headers = headers),
                      body = toJSON(dataLLM, auto_unbox = TRUE))

    parsed <- httr::content(res)$choices[[1]]$message$content

    # Clean up: delete the temp image on success, but KEEP it if the page failed
    # so you can open it and see what tripped up the model.
    if(is.null(parsed) || parsed == ""){
      warning(paste('Page', i, 'failed to parse - keeping image:', pngFile))
      parsedPages[i] <- paste0('[PAGE ', i, ' PARSE FAILED]')
    } else {
      parsedPages[i] <- parsed
      unlink(pngFile)
    }
  }

  # Remove the temp folder only if it is now empty (i.e. every page succeeded)
  if(length(list.files(tmpDir)) == 0){ unlink(tmpDir, recursive = TRUE) }
}

# Stitch the pages together and save. JSON output (Route B only) becomes one
# JSON array of page objects (.json); everything else is markdown (.md).
if(jsonOutput == TRUE && simplePDF == FALSE){
  parsedDoc <- paste0('[\n', paste(parsedPages, collapse = ',\n'), '\n]')
  outFile   <- paste0(savePath, 'parsedPDF.json')
} else {
  parsedDoc <- paste(parsedPages, collapse = '\n\n--- PAGE BREAK ---\n\n')
  outFile   <- paste0(savePath, 'parsedPDF.md')
}

cat(parsedDoc)
writeLines(parsedDoc, outFile)

# ---- Reading it back in: MARKDOWN (.md) ----
# Plain text, so just read the lines and look at them
# mdDoc <- readLines(paste0(savePath, 'parsedPDF.md'))
# cat(mdDoc, sep = '\n')

# ---- Reading it back in: JSON (.json) ----
# Structured, so we can program against it - e.g. pull a table into a data frame
# doc <- fromJSON(paste0(savePath, 'parsedPDF.json'), simplifyVector = FALSE)
# firstTable <- doc[[1]]$tables[[1]]                 # page 1, first table (list of rows)
# tableDF <- do.call(rbind, lapply(firstTable, unlist)) |> as.data.frame()
# tableDF

# End
