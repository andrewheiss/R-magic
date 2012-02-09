# Stine and Foster Chapter 19 Linear Models
# Author: Ray Nelson
###############################################################################
# load data and libraries
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster19.RData")
library(car)
library(ggplot2)
library(moments)
library(timeDate)

## Diamond Rings

# Scatterplot of diamond rings
scatterplot(price ~ weight, data = emeraldDiamonds)
qplot(data = emeraldDiamonds, x = weight, y = price)
qplot(data = emeraldDiamonds, x = weight, y = price,
      geom = c("point", "smooth"))

# linear model and interpretation of the intercept and slope
diamonds.lm <- lm(price ~ weight, data = emeraldDiamonds)
summary(diamonds.lm)

# linear model and summary linear model attributes
attributes(diamonds.lm)
coefficients(diamonds.lm)
attributes(summary(diamonds.lm))
summary(diamonds.lm)["r.squared"]

diamonds <- data.frame(emeraldDiamonds,fitted = fitted(diamonds.lm),
                       residuals = residuals(diamonds.lm))
head(diamonds)

# forecasting and prediction
predict(diamonds.lm, data.frame(weight = 0.4), interval = "confidence")
predict(diamonds.lm, data.frame(weight = 0.4), interval = "prediction")
predict(diamonds.lm, data.frame(weight = c(0.3, 0.5)),
        interval = "confidence")
predict(diamonds.lm, data.frame(weight = seq(0.1, 0.9, 0.1)),
        interval = "confidence")

# Residuals plot and conditions for linear regression
# Conditions for simple regression
# 1) Linear
# 2) Random residual variation
# 3) No obvious lurking variables
# 4) Normally distributed

qplot(data = diamonds, x = weight, y = price,
      geom = c("point", "smooth"), method = "lm")
qplot(data = diamonds, x = weight, y = price,
      geom = c("point", "smooth"), se = FALSE) +
        geom_smooth(method = "lm", se = FALSE, colour = "red")
qplot(data = diamonds, x = weight, y = residuals) +
  geom_hline(yintercept = 0)
with(diamonds, {
  hist(residuals, breaks = "FD", freq = FALSE)
  lines(density(residuals))
  print(skewness(residuals))
  print(kurtosis(residuals))
  qqPlot(residuals)
  print(shapiro.test(residuals))
  jarque.test(residuals)
})

rm(diamonds.lm, diamonds, )

## Estimated Consumption

# Scatterplot of gas consumption by degree days
scatterplot(gas ~ degreesBelow65, data = gasConsumption)

# linear model and interpretation of the intercept and slope
gas.lm <- lm(gas ~ degreesBelow65, data = gasConsumption)
summary(gas.lm)

# Prediction for 25 degree days

predict(gas.lm, data.frame(degreesBelow65 = 25), interval = "prediction",
        se.fit = TRUE)

# Residuals plot and conditions for linear regression
# Conditions for simple regression
# 1) Linear
# 2) Random residual variation
# 3) No obvious lurking variables
# 4) Normally distributed

scatterplot(gas ~ degreesBelow65, data = gasConsumption)
residualPlots(gas.lm)
Residuals <- residuals(gas.lm)
hist(Residuals, breaks = "FD", freq = FALSE)
lines(density(Residuals))
skewness(Residuals)
kurtosis(Residuals)
qqPlot(Residuals)
shapiro.test(Residuals)
jarque.test(Residuals)

rm(gas.lm, Residuals)

## BMW Lease Cost
scatterplot(price ~ age, data = bmwLease)
scatterplot(price ~ mileage, data = bmwLease)

# Regression model

# Conditions for linear model

## Seattle Homes
scatterplot(price ~ squareFeet, data = seattleHomes, id.method = "identify")

# Regression model

# Conditions for linear model

## How to identify points in OECD data for problem #45

with(oecd, {
  plot(x = tradeBalance, y = gdp)
  identify(x = tradeBalance, y = gdp, labels = nation)
})
oecd.lm <- lm(gdp ~ tradeBalance, data = oecd)
oecd.expanded <- data.frame(oecd, Residuals = residuals(oecd.lm))
with(oecd.expanded, {
  plot(x = tradeBalance, y = Residuals)
  identify(x = tradeBalance, y = Residuals, labels = nation)
})