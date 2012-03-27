# Load required libraries
library(car)
library(ggplot2)
library(timeSeries)

# Load data
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster24.RData")

# Initial model
sony.model <- lm(SonyPercentChange ~ MarketPercentChange, data = capm)
summary(sony.model)

sony.mrm <- lm(SonyPercentChange ~ MarketPercentChange + DowPercentChange + smallBig + highLow, data = capm)
sony.summary <- summary(sony.mrm)
vif(sony.mrm)

# Collinearity
sony.summary <- as.data.frame(sony.summary$coefficients)
sony.summary <- round(data.frame(sony.summary[-1,], vif(sony.mrm)), 2)
colnames(sony.summary) <- c('Estimate', 'Std Error', 't-Statistics', 'p-Value', 'VIF')
sony.summary

