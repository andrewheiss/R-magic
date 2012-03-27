#---------------------------------------------------------------------------
# PMGT 634 Homework 14
# March 2011
# Andrew Heiss
#---------------------------------------------------------------------------

# Load required libraries
library(car)

# Load data
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster24.RData")

fancy.summary <- function(model) {
  model.summary <- summary(model)
  n <- sum(model.summary$df[c(1,2)])
  r2 <- model.summary$r.squared
  adj.r2 <- model.summary$adj.r.squared
  f <- model.summary$fstatistic[1]
  # f.p <- pf(model.summary$fstatistic[1],model.summary$fstatistic[2],model.summary$fstatistic[3],lower.tail=FALSE)
  se <- model.summary$sigma
  summary.values <- c(r2, adj.r2, se, f, n)
  summary.names <- c("R²", "Adjusted R²", "se", "F-statistic", "n")
  data.frame("Statistics"=summary.names, "Values"=round(summary.values, 3))
}

vif.summary <- function(model) {
  require(car)
  model.summary <- summary(model)
  better.table <- as.data.frame(model.summary$coefficients)
  better.table <- round(data.frame(better.table[-1,], vif(model)), 2)
  colnames(better.table) <- c('Estimate', 'Std Error', 't-Statistic', 'p-Value', 'VIF')
  better.table
}

#--------
# 24.32
#--------

Stores <- convenience

# Part A
stores.model <- lm(sales ~ volume + washes, data=Stores)
summary(stores.model)

# Part C
fancy.summary(stores.model)
vif.summary(stores.model)

marginalModelPlots(stores.model)
sales.washes <- lm(sales ~ washes, data=Stores)
sales.volume <- lm(sales ~ volume, data=Stores)
summary(sales.washes)
summary(sales.volume)
compareCoefs(stores.model, sales.washes)

#--------
# 24.35
#--------

HomePrices <- homePrices
homes.model <- lm(price ~ sqFt + Bathrooms, data=HomePrices)
summary(homes.model)

# Part A
cor(HomePrices$sqFt, HomePrices$Bathrooms)

# Part B
vif(homes.model)

# Part C
price.sqft <- lm(price ~ sqFt, data=HomePrices)
summary(price.sqft)
price.bathrooms <- lm(price ~ Bathrooms, data=HomePrices)
summary(price.bathrooms)

leveragePlots(homes.model, pch=20)
marginalModelPlots(homes.model, pch=20)

price.sqft.residuals <- residuals(price.sqft)
price.bathrooms.residuals <- residuals(price.bathrooms)

compareCoefs(homes.model, price.sqft, price.bathrooms)