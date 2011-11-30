# Chapter 21
load("~/Documents/BYU 2011-2012/Fall 2011/MBA 609/R magic/MBA 609 datasets/stineFoster21.RData")
library(byumpa)
library(car)
library(moments)

x <- berkshire$percentageChangeMarket
y <- berkshire$percentageChangeBH

# Initial scatterplot
grey.plot(x, y, xlab="% change market", ylab="% change Berkshire Hathaway", main="Simple Regression Model")
srm <- lm(y~x)
abline(srm, col="red")

summary(srm)

# Check conditions for simple regression
# 1. Is it linear? 
# Yep. Original plot shows that, as does the residual plot alone and in a visual test
grey.plot(x, residuals(srm), xlab="% change market", ylab="Residuals", main="Residuals")
abline(0,0,col="red")
visual.test(x, residuals(srm))

# 2. Lurking variables? 
# The book says there aren't any

# 3. Independent residuals
grey.plot(berkshire$date, residuals(srm), xlab="Date", ylab="Residuals % change in Berk Hath", main="Residuals over time", type="o")
abline(0, 0, col="red")
# No drifting over time. They're independent.

# 4. Similar variances?
# Look for heteroscedasticity in the residuals
grey.plot(x, residuals(srm), xlab="% change market", ylab="Residuals", main="Residuals")
# Spread of the residuals is pretty constant

# 5. Normally distributed?
hist(residuals(srm))
qqPlot(residuals(srm), pch=20)
# The residuals look mostly normal, except when they shoot out at the end like that. That's probably because the book removed outliers or something
# It's not quite normal, but that's okay; inferences will still work well
# Because it's not normal, we can use the Central Limit Theorem
# Sample size should be greater than 10 times the larger of the skewness or the absolute value of the kurtosis of the residuals
skewness(residuals(srm))
kurtosis(residuals(srm))
# So the sample should have at least 56 observations.
length(residuals(srm))
# It is. Phew.