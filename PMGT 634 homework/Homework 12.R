#---------------------------------------------------------------------------
# PMGT 634 Homework 12
# February 2011
# Andrew Heiss
#---------------------------------------------------------------------------

# Load required libraries
library(car)
library(ggplot2)
library(forecast)
library(timeSeries)

# Load data
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster23.RData")

#--------
# 23.37
#--------
Chains <- goldChains

# Part A
scatterplotMatrix(~ price + length + width, data=Chains, main="Scatterplot matrix for gold chains dataset", pch=20)

price.length <- lm(price ~ length, data=Chains)
price.width <- lm(price ~ width, data=Chains)
length.width <- lm(length ~ width, data=Chains)

summary(price.length)
summary(price.width)
summary(length.width)

# Part B
round(cor(Chains), 3)

# Part C
chains.model <- lm(price ~ length + width, data=Chains)
(chains.model.summary <- summary(chains.model))

summary.values <- c(chains.model.summary$r.squared, chains.model.summary$adj.r.squared, chains.model.summary$sigma, length(Chains$price))
summary.names <- c("R²", "Adjusted R²", "se", "n")
data.frame("Statistics"=summary.names, "Values"=round(summary.values, 3))


#--------
# 22.28
#--------
Shopping <- convenience[,c("sales", "volume", "washes")]

# Part A
scatterplotMatrix(~ sales + volume + washes, data=Shopping, main="Scatterplot matrix for convenience store dataset", pch=20)

sales.volume <- lm(sales ~ volume, data=Shopping)
sales.washes <- lm(sales ~ washes, data=Shopping)
volume.washes <- lm(volume ~ washes, data=Shopping)

summary(sales.volume)
summary(sales.washes)
summary(volume.washes)

# Part B
round(cor(Shopping), 3)

# Part C
shopping.model <- lm(sales ~ volume + washes, data=Shopping)
(shopping.model.summary <- summary(shopping.model))

summary.values <- c(shopping.model.summary$r.squared, shopping.model.summary$adj.r.squared, shopping.model.summary$sigma, length(Shopping$sales))
summary.names <- c("R²", "Adjusted R²", "se", "n")
data.frame("Statistics"=summary.names, "Values"=round(summary.values, 3))
