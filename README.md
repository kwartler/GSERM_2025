# GSERM_2025
GSERM St Gallen 2025 June

## Software requirements

This course requires [R](https://cran.r-project.org/), [R-studio](https://posit.co/download/rstudio-desktop/) both freely available.

Additionally, at times we will explore programmatic requests to "local LLMs". This requires [LM-Studio](https://lmstudio.ai/) or if you have an intel Mac [Jan AI](https://jan.ai/) . 
*Students with older computers, old GPUs or a small amount of RAM, may not be able to execute this portion of the lesson*

### Packages for R

R is customized for specific functions using libraries or packages.  In this class we will use the following packages.  Once you have R and R studio installed run the following command in your console.  Don't worry if you struggle, on day 1 we will set aside time to help though we aren't performing technical support if you don't have administrative access to your computer etc.

```
# Install library pacman
install.packages('pacman')

# Use pacman to install other libraries)
pacman::p_load(ggplot2, ggthemes, stringi, stringr, tm, qdapRegex, dplyr, tidyverse, igraph, networkD3, wordcloud, RColorBrewer, ggwordcloud, wordcloud2, echarts4r, plotrix, gplots, tidytext, radarchart, textdata, tidyr, lubridate, tm # displays the emojis correctly, emoji # Get the emoji lexicon or load one manually, emojifont # another emoji  to explore, textclean #another one!, mgsub #used for substitutions, qdap #emoticons functions, sentimentr, lexicon, SentimentAnalysis, httr, jsonlite, vtreat, ModelMetrics, MLmetrics, pROC, text2vec, caret, glmnet, lsa, yardstick, cluster, factoextra, clue, kmed, skmeans, irlba, lexicon, plyr, sylcount)

```

- Day 1: R Basics & Introduction to NLP #
- Day 2: Visualizations in text mining #
- Day 3: Sentiment Analysis & Machine Learning: Document Classification & Clustering
- Day 4: Introduction to Large Language Models
- Day 5: Effective Prompt Engineering, vector databases and RAG models; FINAL EXAM


### Ethics Paper

Please watch this Youtube [video](https://www.youtube.com/watch?v=zKCynxiV_8I&ab_channel=TaylorLorenz).  Its about 40min long but speaks to the growing role AI is playing in shaping our perspectives.  Please write a ~500-750 word essay summarizing the video and reflecting on what can be done to address these issues. **Revised due date July 7, 2025, midnight EST**  You may turn it in at <edwardkwartler@fas.harvard.edu>.