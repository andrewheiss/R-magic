# Stine and Foster Chapter 21 Simple Linear Regression
# Author: Ray Nelson
################################################################################
# load data and libraries
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster21.RData")
library(car)
library(forecast)

## Berkshire-Hathaway ##########################################################
# linearity and lurking variables
scatterplot(returnBH ~ VWReturn, data = berkshire) # linear
berkshire.lm <- lm(returnBH ~ VWReturn, data = berkshire)
summary(berkshire.lm)
residualPlots(berkshire.lm)

# independence
scatterplot(berkshire.lm$residuals ~ berkshire$VWReturn)
Acf(berkshire.lm$residuals)
Pacf(berkshire.lm$residuals)

# normality
hist(berkshire.lm$residuals, breaks = "FD", freq = FALSE)
lines(density(berkshire.lm$residuals))
rug(berkshire.lm$residuals)
qqPlot(berkshire.lm$residuals)
skewness(berkshire.lm$residuals)
kurtosis(berkshire.lm$residuals)

# Equal variance
ncvTest(berkshire.lm)

# inference
summary(berkshire.lm)

# prediction
predict(berkshire.lm, data.frame(VWReturn = seq(-.2, .2, 0.05)),
        interval = "confidence")
predict(berkshire.lm, data.frame(VWReturn = seq(-.2, .2, 0.05)),
        interval = "prediction")
rm(berkshire.lm)

## CEO Compensation ############################################################
# linearity and lurking variables
scatterplot(totalCompensation ~ netSales, data = financeCEO) # linear?
scatterplot(log10TotalCompensation ~ log10NetSales, data = financeCEO)
ceo.lm <- lm(log10TotalCompensation ~ log10NetSales, data = financeCEO)
summary(ceo.lm)
residualPlots(ceo.lm)

# normality
hist(ceo.lm$residuals, breaks = "FD", freq = FALSE)
lines(density(ceo.lm$residuals))
rug(ceo.lm$residuals)
qqPlot(ceo.lm$residuals)
skewness(ceo.lm$residuals)
kurtosis(ceo.lm$residuals)

# Equal variance
ncvTest(ceo.lm)

# inference
summary(ceo.lm)

# prediction
10^(predict(ceo.lm, data.frame(log10NetSales = seq(8, 11, 0.5)),
            interval = "confidence"))
10^(predict(ceo.lm, data.frame(log10NetSales = seq(8, 11, 0.5)),
            interval = "prediction"))
rm(ceo.lm)

## Locating a franchise outlet #################################################
# linearity and lurking variables
scatterplot(sales ~ traffic, data = traffic) # linear?
traffic.lm <- lm(sales ~ traffic, data = traffic)
summary(traffic.lm)
residualPlots(traffic.lm)

# normality
hist(traffic.lm$residuals, breaks = "FD", freq = FALSE)
lines(density(traffic.lm$residuals))
rug(traffic.lm$residuals)
qqPlot(traffic.lm$residuals)
skewness(traffic.lm$residuals)
kurtosis(traffic.lm$residuals)

# Equal variance
ncvTest(traffic.lm)

# inference
summary(traffic.lm)

# prediction
predict(traffic.lm, data.frame(traffic = seq(25, 50, 1.0)),
        interval = "confidence")
predict(traffic.lm, data.frame(traffic = seq(25, 50, 1.0)),
        interval = "prediction")
rm(traffic.lm)

## diamonds ####################################################################
# linearity and lurking variables
scatterplot(price ~ weight, data = diamonds) # linear?
diamonds.lm <- lm(price ~ weight, data = diamonds)
summary(diamonds.lm)
residualPlots(diamonds.lm)

# normality
hist(diamonds.lm$residuals, breaks = "FD", freq = FALSE)
lines(density(diamonds.lm$residuals))
rug(diamonds.lm$residuals)
qqPlot(diamonds.lm$residuals)
skewness(diamonds.lm$residuals)
kurtosis(diamonds.lm$residuals)

# Equal variance
ncvTest(diamonds.lm)

# inference
summary(diamonds.lm)

# prediction
predict(diamonds.lm, data.frame(weight = seq(0, 1.5, 0.1)),
        interval = "confidence")
predict(diamonds.lm, data.frame(weight = seq(0, 1.5, 0.1)),
        interval = "prediction")
rm(diamonds.lm)

## crabs  ######################################################################
# linearity and lurking variables
scatterplot(weight ~ daysEffort, data = crabs) # linear?
crabs.lm <- lm(weight ~ daysEffort, data = crabs)
summary(crabs.lm)
residualPlots(crabs.lm)

# independence
scatterplot(crabs.lm$residuals ~ crabs$daysEffort)
Acf(crabs.lm$residuals)
Pacf(crabs.lm$residuals)

# normality
hist(crabs.lm$residuals, breaks = "FD", freq = FALSE)
lines(density(crabs.lm$residuals))
rug(crabs.lm$residuals)
qqPlot(crabs.lm$residuals)
skewness(crabs.lm$residuals)
kurtosis(crabs.lm$residuals)

# Equal variance
ncvTest(crabs.lm)

# inference
summary(crabs.lm)

# prediction
predict(crabs.lm, data.frame(daysEffort = seq(3000, 9000, 500)),
        interval = "confidence")
predict(crabs.lm, data.frame(daysEffort = seq(3000, 9000, 500)),
        interval = "prediction")
rm(crabs.lm)
