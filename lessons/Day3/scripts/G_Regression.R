#' Author: Ted Kwartler
#' Date: June 12, 2025
#' Purpose: Build a regression model
#' 

# libs
library(ggplot2)
library(ggthemes)
library(dplyr)

# Data
diamonds <- read.csv('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/diamonds2023.csv')

# EDA
head(diamonds)
summary(diamonds)

# Let's see if there are outliers
ggplot(data = diamonds, aes(y=priceClean)) + geom_boxplot() + theme_gdocs()

# Let's drop the outliers in the top demi decile
quantile(diamonds$priceClean, probs = seq(.1,.95, by = .05))
dropAmt <- tail(quantile(diamonds$priceClean, probs = seq(.1,.95, by = .05)), 1)
diamonds <- subset(diamonds, diamonds$priceClean<dropAmt)

# Build a scatter plot to show relationship 
p <- ggplot(diamonds, aes(Carat, priceClean)) + geom_point(alpha=0.05) + theme_gdocs()
p

# Since we see a relationship let's make a linear model to predict prices
fit <- lm(priceClean ~ Carat, diamonds)
fit

# Add out model predictions
p <- p + geom_abline(intercept =  coefficients(fit)[1], 
                     slope = coefficients(fit)[2], 
                     color='red') +
  theme_gdocs()
p

# End
