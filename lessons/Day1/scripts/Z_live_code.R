#' Live Code in class
#' TK
#' Apr 28, 2024
#' 

# libraries

# Custom Functions

# Create custom stop words

# data
text <- read.csv('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/coffeeVector.csv')
head(text)

# Find the tweets that mention "mug"

# Any substitutions? Starbuck's to Starbucks

# How many tweets contain the term starbucks?

# Make a Corpus from a Vector Source

# Clean the Corpus

# Extract a cleaned copy of the text and save it to the personal files folder

# Create a DTM and change it to a simple matrix

# Find the column names that have "mug" anchored within grep within the DTM
mugColumns

# Review the first 6 rows and the index of "mug" previously created
DTMmatrix[_:_,mugColumns]

# Compare the 4th tweet in the original text to the content() of the 4th cleaned corpus doc


# End