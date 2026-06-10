#' TK
#' Date June 9 2026
#' Use LLM to explore branding in LLMs
#' Compare how two brands are described across N LLM responses
#' Visualization: comparison cloud, pyramid plot, commonality cloud

# Load our OpenRouter function
source("~/Desktop/GSERM_2025/openRouter_function.R")

# Libraries
library(tm)
library(wordcloud)
library(wordcloud2)

# Inputs
modelLLM    <- 'openai/gpt-5-mini'  # 'deepseek/deepseek-v4-flash'
temperature <- 0.7
sysPrompt   <- 'You are a helpful AI assistant. Be honest about brand descriptions.'
nResponses  <- 50

topicA <- 'Describe Tesla cars.'
topicB <- 'Describe BYD cars.'

# Collect Brand A responses
responsesA <- list()
for(i in 1:nResponses){
  print(paste('Tesla response', i, 'of', nResponses))
  tmp <- query_openrouter(prompt      = topicA,
                          system_prompt = sysPrompt,
                          model       = modelLLM,
                          temperature = temperature)
  responsesA[[i]] <- tmp
}

# ── Collect Brand B responses (BYD) ───────────────────────────────────────────
responsesB <- list()
for(i in 1:nResponses){
  print(paste('BYD response', i, 'of', nResponses))
  tmp <- query_openrouter(prompt      = topicB,
                          system_prompt = sysPrompt,
                          model       = modelLLM,
                          temperature = temperature)
  responsesB[[i]] <- tmp
}

# ── Clean and organize ────────────────────────────────────────────────────────
# Collapse list to character vector and strip line breaks
vecA <- gsub('\n', ' ', unlist(responsesA))
vecB <- gsub('\n', ' ', unlist(responsesB))

# Build labeled data frames
dfA <- data.frame(doc_id = 1:nResponses,
                  text   = vecA,
                  brand  = 'Tesla',
                  model  = modelLLM,
                  date   = Sys.Date())

dfB <- data.frame(doc_id = 1:nResponses,
                  text   = vecB,
                  brand  = 'BYD',
                  model  = modelLLM,
                  date   = Sys.Date())

# Save copies for class
write.csv(dfA, paste0('~/Desktop/GSERM_2025/lessons/Day2/', Sys.Date(), '_Tesla_', make.names(modelLLM), '.csv'), row.names = FALSE)
write.csv(dfB, paste0('~/Desktop/GSERM_2025/lessons/Day2/', Sys.Date(), '_BYD_',   make.names(modelLLM), '.csv'), row.names = FALSE)

# ── Build corpora ─────────────────────────────────────────────────────────────
# Create a Corpus for each brand
corpA <- Corpus(VectorSource(vecA))
corpB <- Corpus(VectorSource(vecB))

# Apply standard cleaning to both corpora
cleanCorpus <- function(corp) {
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, removeWords, c(stopwords('en'), 'tesla', 'byd', 'car', 'cars', 'electric'))
  corp <- tm_map(corp, stripWhitespace)
  return(corp)
}

corpA <- cleanCorpus(corpA)
corpB <- cleanCorpus(corpB)

# ── Term Document Matrices ────────────────────────────────────────────────────
tdmA <- TermDocumentMatrix(corpA)
tdmB <- TermDocumentMatrix(corpB)

# ── Frequency bar chart ───────────────────────────────────────────────────────
# Hint: use rowSums() on as.matrix(tdmA) to get term frequencies
# Then sort descending and barplot() the top N terms
# Do the same for tdmB and compare side by side

# ── Association ───────────────────────────────────────────────────────────────
# Hint: use findAssocs(tdmA, terms = 'innovative', corlimit = 0.3)
# Try a brand-relevant seed word for each corpus

# ── Dendrogram ────────────────────────────────────────────────────────────────
# Hint: remove sparse terms first with removeSparseTerms(tdmA, 0.97)
# Then dist() -> hclust() -> plot() to see term clusters for each brand

# ── Static wordcloud ──────────────────────────────────────────────────────────
# Hint: use wordcloud() on the frequency vectors from each TDM
# Compare which words dominate Tesla vs BYD responses visually

# ── Comparison cloud ──────────────────────────────────────────────────────────
# A comparison cloud shows words that are DISTINCTIVE to each brand.
# Words appearing more in Tesla responses appear on one side;
# words more common in BYD responses appear on the other.
#
# Step 1: collapse each corpus into a single document (one per brand)
# Hint: paste(vecA, collapse = ' ') and paste(vecB, collapse = ' ')
#
# Step 2: combine into a named character vector of length 2
# Hint: all_text <- c(Tesla = ..., BYD = ...)
#
# Step 3: build a single TermDocumentMatrix from the combined VectorSource
# and set the column names to c('Tesla', 'BYD')
#
# Step 4: convert to matrix and call comparison.cloud()
# Hint: comparison.cloud(mat, max.words = 100, title.size = 1.5)

# ── Pyramid plot ──────────────────────────────────────────────────────────────
# A pyramid plot shows the most frequent terms for EACH brand side by side,
# making it easy to see what language each brand owns.
#
# Step 1: get top N term frequencies from each TDM using rowSums()
# Step 2: find terms that appear in BOTH frequency tables (intersect)
# Step 3: align the two frequency vectors on the shared terms
# Hint: use pyramid.plot() from the plotrix package
# install.packages('plotrix') if needed
# Hint: pyramid.plot(freqA[common], freqB[common],
#                    labels = common,
#                    top.labels = c('Tesla','Terms','BYD'),
#                    main = 'Brand Language Comparison')

# ── Commonality cloud ─────────────────────────────────────────────────────────
# A commonality cloud shows words that BOTH brands share — the common language
# the LLM uses regardless of which brand it is describing.
#
# Step 1: reuse the combined TermDocumentMatrix from the comparison cloud step
#
# Step 2: call commonality.cloud() on the matrix
# Hint: commonality.cloud(mat, max.words = 80, color = 'steelblue')
#
# Question for students: what does shared language tell us about GEO?
# If both brands are described using the same words, what does that mean
# for a brand trying to differentiate itself in LLM responses?

# ── d3 wordcloud (interactive) ────────────────────────────────────────────────
# Hint: build a frequency data frame from one TDM and pass to wordcloud2()
# wordcloud2(data.frame(word = names(freqA), freq = freqA))

# End