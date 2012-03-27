# Stine and Foster Chapter 20 Problem 37 
# Author: Ray Nelson
###############################################################################
# Data and R libraries
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster20.RData")
# load the Chapter 20 data frames from the class web site
library(car)
library(forecast)

# scale subscribers by converting to one million subscribers
cellularUS <- transform(cellularUS, subscribers = subscribers / 1000000, period.reciprocal = 1 / period)

# b) Trend in the data
plot(subscribers ~ date, data = cellularUS, type = "l",
     xlab = "", ylab = "Million Subscribers")

# c) linear model
cellularUS.lm <- lm(subscribers ~ date, data = cellularUS)
scatterplot(residuals(cellularUS.lm) ~ cellularUS$date, id.n = 5)
residualPlot(cellularUS.lm)
rm(cellularUS.lm)

# d) Logarithmic transformation
log.lm <- lm(log(subscribers) ~ date, data = cellularUS)
summary(log.lm)
plot(log(subscribers) ~ date, data = cellularUS, type = "l")
abline(log.lm,  col = "red")
residualPlot(log.lm)

# Forecast with logarithmic transformation model
futureDate <- seq(2007, 2009, .5) # explanatory values to generate forecasts
predictions.log <- predict(log.lm, data.frame(date = futureDate),
                           interval = "prediction")
predictions.log <- exp(predictions.log[,1]) # reverse the logarithm with exp

# values for comparison forecast plot
(time <- c(cellularUS$date, futureDate))
subscribers.log <- c(cellularUS$subscribers, predictions.log)
plot(subscribers.log ~ time, type = "l")
rm(log.lm, predictions.log)

# e) and f) percentage change in subscribers
plot(pctGrowth ~ period, data = cellularUS, type = "l")
recip.lm <- lm(pctGrowth ~ period.reciprocal, data = cellularUS)
summary(recip.lm)
plot(pctGrowth ~ period.reciprocal, data = cellularUS, type = "l",
     xlab = "Reciprocal of Time Since 1984", ylab = "Percentage Change",
     main = "Down the Ladder of Powers for X")
abline(recip.lm, col = 'red')
rm(recip.lm)

# Optimal lambda for Box-Cox transformation
BoxCox.lambda(cellularUS$subscribers) # optimal lambda
lambda <- .31
response <- BoxCox(cellularUS$subscribers, lambda)
explanatory <- cellularUS$date
transformed.lm <- lm(response ~ explanatory)
plot(response ~ explanatory, type = "l")
abline(transformed.lm, col = "red")

# Box-Cox forecasts
predictions.boxcox <- predict(transformed.lm,
                              data.frame(explanatory = futureDate), interval = "prediction")
predictions.boxcox <- InvBoxCox(predictions.boxcox, lambda) # reverse Box-Cox
subscribers.boxcox <- c(cellularUS$subscribers, predictions.boxcox[,1])
plot(subscribers.boxcox ~ time, type = "l")
rm(lambda, response, explanatory, transformed.lm, predictions.boxcox)

# Comparison plot of logarithm and Box-Cox Transformations
plot(subscribers.log ~ time, type = "l", ylab = "Millions of Subscribers",
     xlab = "", main = "Comparison of Log and Box-Cox Transformations")
lines(subscribers.boxcox ~ time, type = "l", col = 'red')

# cleanup
rm(time, futureDate, subscribers.log, subscribers.boxcox)