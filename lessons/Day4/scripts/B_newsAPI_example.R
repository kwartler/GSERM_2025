#' Title: https://newsdata.io/pricing
#' Purpose: Get data from JSON
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: June 10, 2026
#' 

# Library
library(jsonlite)

# www.newsapi.org Key
NEWSORG_API_KEY <- Sys.getenv('NEWSORG_API_KEY')

# Search term
apiQuery <- URLencode('Harvard University')

# One example API endpoint
pg <- paste0('https://newsapi.org/v2/everything?q=',apiQuery,'&sortBy=publishedAt&apiKey=',NEWSORG_API_KEY)
pg 

# Recent News Data 
apiResponse <- fromJSON(pg)

# 200 or "success" is what you're looking for with APIs
apiResponse$status

# How many articles were received? Articles are paginated so you have to refer to the next page in the query
apiResponse$totalResults

# Article responses returned in a data frame
str(apiResponse)

# One article
apiResponse$articles[2,]


# End