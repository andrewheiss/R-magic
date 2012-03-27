# Stine and Foster Chapter 22 Regession Diagnostics
# Author: Ray Nelson
# February 14, 2012
################################################################################
# load data and libraries
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster22.RData")

library(car)
library(ggplot2)
library(forecast)
library(timeSeries)

# Problem 1: Changing Variation ################################################
scatterplot(price ~ sqFt, data = homePrices)
home.lm <- lm(price ~ sqFt, data = homePrices)
summary(home.lm)

# Constant Variance
residualPlots(home.lm)
sizeCategory <- cut(homePrices$sqFt, breaks = c(0, 1500, 2500, 10000),
                    labels = c("Less than 1500", "1500 to 2500", "More than 2500"))
Boxplot(home.lm$residuals ~ sizeCategory)
plotData <- data.frame(homePrices, sizeCategory,
                       residuals = home.lm$residuals) # data.frame for qplot
qplot(data = plotData, x = sizeCategory, y = price, geom = "boxplot") +
  labs(x = "", y = "\nThousands of Dollars") +
  coord_flip() +
  opts(title = "Heteroscedastic Prices\n")
qplot(data = plotData, x = sizeCategory, y = residuals, geom = "boxplot") +
  labs(x = "", y = "\nThousands of Dollars") +
  coord_flip() +
  opts(title = "Heteroscedastic Residuals\n")

# Are residuals normally distributed
hist(home.lm$residuals, breaks = "FD", freq = FALSE)
lines(density(home.lm$residuals))
rug(home.lm$residuals)
qqPlot(home.lm$residuals)
skewness(home.lm$residuals)
kurtosis(home.lm$residuals)
jarque.bera.test(home.lm$residuals)
shapiro.test(home.lm$residuals)

# cleanup
rm(home.lm, sizeCategory, plotData)

# Reciprocal Analysis
scatterplot(pricePerSqFt ~ reciprocalSqFt, data = homePrices)
home.lm <- lm(pricePerSqFt ~ reciprocalSqFt, data = homePrices)
summary(home.lm)

# Constant Variance
residualPlots(home.lm)
sizeCategory <- cut(homePrices$sqFt, breaks = c(0, 1500, 2500, 10000),
                    labels = c("Less than 1500", "1500 to 2500", "More than 2500"))
Boxplot(home.lm$residuals ~ sizeCategory, xlab = "", ylab = "Residuals")
plotData <- data.frame(homePrices, sizeCategory,
                       residuals = home.lm$residuals) # data.frame for qplot
qplot(data = plotData, x = sizeCategory, y = price, geom = "boxplot") +
  labs(x = "", y = "\nThousands of Dollars") +
  coord_flip() +
  opts(title = "Heteroscedastic Prices\n")
qplot(data = plotData, x = sizeCategory, y = residuals, geom = "boxplot") +
  labs(x = "", y = "\nResiduals") +
  coord_flip() +
  opts(title = "Homoscedastic Residuals\n")

# Are residuals normally distributed
hist(home.lm$residuals, breaks = "FD", freq = FALSE)
lines(density(home.lm$residuals))
rug(home.lm$residuals)
qqPlot(home.lm$residuals)
skewness(home.lm$residuals)
kurtosis(home.lm$residuals)
jarque.bera.test(home.lm$residuals)
shapiro.test(home.lm$residuals)

# cleanup
rm(home.lm, sizeCategory, plotData)

# Problem 2: Leveraged Outliers ################################################
# all observations
scatterplot(cost ~ size, data = construction, smooth = FALSE,
            id.method = "identify")
construction.lm <- lm(cost ~ size, data = construction)
summary(construction.lm)

# Analysis of residuals
residualPlots(construction.lm)
outlierTest(construction.lm)
influencePlot(construction.lm, id.n = 3)
influenceIndexPlot(construction.lm, id.n = 3)

# exclude observation # 1
construction2 <- construction[-1,]
scatterplot(cost ~ size, data = construction2, smooth = FALSE)
construction2.lm <- lm(cost ~ size, data = construction2)
summary(construction2.lm)

# Analysis of residuals
residualPlots(construction2.lm)
outlierTest(construction2.lm)
influencePlot(construction2.lm)
influenceIndexPlot(construction2.lm, id.n = 3)

# Comparison of models
# Plain
compareCoefs(construction.lm, construction2.lm)
plot(cost ~ size, data = construction)
abline(construction.lm, col = "blue")
abline(construction2.lm, col = "red")

# Lowess graph for lecture page
ggplot(data = construction, aes(x = size, y = cost / 1000)) +
  geom_point(shape = 20) +
  geom_smooth(method = 'lm', fill = alpha("lightblue", 0.4), colour = "blue",
              size = 1.0) +
                geom_smooth(data = construction2, aes(x = size, y = cost / 1000),
                            method = 'lm', fill = alpha("lightsalmon3", 0.4), colour = 'red',
                            size = 1.0) +
                              labs(x = '\nSize in (Square Feet)', y = 'Cost in Thousands of Dollars\n') +
                              opts(title = 'Contractor Cost Versus Size of Project\n')	
ggsave("construction.png", width = 7, height = 5, dpi = 100)

# Comparison of prediction intervals
responseValues <- seq(0, 1000, 10)
prediction1 <- predict(construction.lm, data.frame(size = responseValues),
                       interval = "prediction")
prediction2 <- predict(construction2.lm, data.frame(size = responseValues),
                       interval = "prediction")
prediction <- data.frame(responseValues, prediction1, prediction2)
names(prediction) <- c("size", "all", "allLow", "allHigh", "omit", "omitLow",
                       "omitHigh")

# All the data
ggplot(data = prediction, aes(x = size)) +
  geom_ribbon(aes(ymin = allLow, ymax = allHigh),
              fill = alpha("red", 0.2)) +
                geom_point(data = construction, aes(x = size, y = cost)) + 
                geom_line(aes(y = all), colour = "red", size = 1.0)

# Omitted observation
ggplot(data = prediction, aes(x = size)) +
  geom_ribbon(aes(ymin = omitLow, ymax = omitHigh),
              fill = alpha("blue", 0.2)) +
                geom_point(data = construction, aes(x = size, y = cost)) + 
                geom_line(aes(y = omit), colour = "blue", size = 1.0)

# All together
ggplot(data = prediction, aes(x = size)) +
  geom_ribbon(aes(ymin = allLow, ymax = allHigh),
              fill = alpha("red", 0.2)) +
                geom_line(aes(y = all), colour = "red", size = 1.0) +
                geom_ribbon(aes(ymin = omitLow, ymax = omitHigh),
                            fill = alpha("blue", 0.2), colour = NA) +
                              geom_line(aes(y = omit), colour = "blue", size = 1.0) +
                              geom_point(data = construction, aes(x = size, y = cost))

rm(construction.lm, construction2, construction2.lm, prediction,
   prediction1, prediction2, responseValues)

# Problem 3: Dependent Errors and Time Series ##################################
scatterplot(millionSubscribers ~ nDate, data = cellular, smooth = FALSE)
cellular.lm <- lm(millionSubscribers ~ nDate, data = cellular)
summary(cellular.lm)
residualPlots(cellular.lm)

# Durbin Watson and autocorrelation function (ACF)
dwt(cellular.lm, max.lag = 15)
Acf(cellular.lm$residuals)

# Forecasts
future <- seq(2007, 2009, 0.5)
forecasts <- predict(cellular.lm, data.frame(nDate = future))

# Plot of forecasts
plotData <- data.frame(cellular[, c("nDate", "millionSubscribers")], 
                       fitted = cellular.lm$fitted)
plotData2 <- data.frame(future, forecasts)
ggplot(data = plotData, aes(x = nDate)) +
  geom_point(aes(y = millionSubscribers)) +
  geom_line(aes(y = fitted), colour = "blue", size = 1.0) +
  geom_line(data = plotData2, aes(x = future, y = forecasts), colour = "red",
            size = 1.0) +
              labs(x = '', y = "Million Subscribers")

# Cleanup
rm(cellular.lm, forecasts, plotData, plotData2)

# Fourth root transformation
scatterplot(millionSubscribers4Root ~ nDate, data = cellular, smooth = FALSE)
cellular.lm <- lm(millionSubscribers4Root ~ nDate, data = cellular)
summary(cellular.lm)
residualPlots(cellular.lm)

# Durbin Watson and autocorrelation function (ACF)
dwt(cellular.lm, max.lag = 10)
Acf(cellular.lm$residuals)

# Forecasts
forecasts <- (predict(cellular.lm, data.frame(nDate = future)))^4

# Plot of forecasts
plotData <- data.frame(cellular[, c("nDate", "millionSubscribers")], 
                       fitted = cellular.lm$fitted^4)
plotData2 <- data.frame(future, forecasts)
ggplot(data = plotData, aes(x = nDate)) +
  geom_point(aes(y = millionSubscribers)) +
  geom_line(aes(y = fitted), colour = "blue", size = 1.0) +
  geom_line(data = plotData2, aes(x = future, y = forecasts), colour = "red",
            size = 1.0) +
              labs(x = '', y = "Million Subscribers")

# Cleanup
rm(cellular.lm, forecasts, plotData, plotData2)

# Box-Cox Transformation
(lambda <- BoxCox.lambda(cellular$millionSubscribers))
cellular <- transform(cellular, boxCox = BoxCox(millionSubscribers, lambda))
scatterplot(boxCox ~ nDate, data = cellular, smooth = FALSE)
cellular.lm <- lm(boxCox ~ nDate, data = cellular)
summary(cellular.lm)
residualPlots(cellular.lm)

# Durbin Watson and autocorrelation function (ACF)
dwt(cellular.lm, max.lag = 10)
Acf(cellular.lm$residuals)

# Forecasts
forecasts <- InvBoxCox(predict(cellular.lm, data.frame(nDate = future)), lambda)

# Plot of forecasts
plotData <- data.frame(cellular[, c("nDate", "millionSubscribers")], 
                       fitted = InvBoxCox(cellular.lm$fitted, lambda))
plotData2 <- data.frame(future, forecasts)
ggplot(data = plotData, aes(x = nDate)) +
  geom_point(aes(y = millionSubscribers)) +
  geom_line(aes(y = fitted), colour = "blue", size = 1.0) +
  geom_line(data = plotData2, aes(x = future, y = forecasts),
            colour = "red", size = 1.0) +
              labs(x = '', y = "Million Subscribers")

# Cleanup
rm(future, lambda, cellular.lm, forecasts, plotData, plotData2)

# Lowess Plot
qplot(data = cellular, x = nDate, y = millionSubscribers,
      geom = c("point", "smooth"), xlab = '', ylab = "Millions\n",
      main = "Subscribers\n")