#---------------------------------------------------------------------------
# PMGT 634 Homework 10
# February 2011
# Andrew Heiss
#---------------------------------------------------------------------------

# Load required libraries
library(car)
library(ggplot2)
library(forecast)
library(timeSeries)

# Load data
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster21.RData")

#--------
# 21.41
#--------

Seattle <- seattleHomes
x <- Seattle$reciprocalSqFt
y <- Seattle$priceSqFt
homeprice.model <- lm(y ~ x)
summary(homeprice.model)
plot(y~x)

# Parts A and B
confint(homeprice.model)

# Part C
(prediction <- predict(homeprice.model, data.frame(x=(1/3000)), interval=c("prediction")))

# Part D
prediction * 3000


#-------------------------
# 4M High Frequency Data
#-------------------------
Stocks.daily <- appleDaily
Stocks.monthly <- appleMonthly

# Part C
Stocks.monthly.model <- lm(Stocks.monthly$appleReturn ~ Stocks.monthly$marketReturn)
Stocks.monthly$residuals <- residuals(Stocks.monthly.model)

Stocks.daily.model <- lm(Stocks.daily$dailyApple ~ Stocks.daily$dailyMarket)
Stocks.daily$residuals <- residuals(Stocks.daily.model)

# Verify conditions for SRM
# Check prerequisites for simple regression model
# 1. Is it linear?
Combined.plot <- data.frame(market=Stocks.monthly$marketReturn, apple=Stocks.monthly$appleReturn, residuals=Stocks.monthly$residuals)

p <- ggplot(data=Combined.plot, aes(market, apple))
p + geom_point(size=3, colour="black") + 
  xlab("\nMarket returns") + ylab("Apple returns\n") + 
  geom_smooth() + 
  geom_smooth(method="lm", colour="red", se=FALSE) + 
  opts(title = "Apple returns as a function of market returns (monthly)\n")

r <- ggplot(data=Combined.plot, aes(market, residuals))
r + geom_point(size=3, colour="black") + 
  xlab("\nMarket returns") + ylab("Residuals\n") + 
  geom_smooth() + 
  geom_smooth(method="lm", colour="red", se=FALSE) + 
  opts(title = "Apple residuals (monthly)\n")

library(byumpa)
visual.test(Stocks.monthly$marketReturn, Stocks.monthly$residuals)

# 2. Lurking variables?
# There might be, but for simplicity's sake, I won't worry about them

# 3. Independent?
Acf(Stocks.monthly$residuals)

# 4. Similar variance?
ncvTest(Stocks.monthly.model)

# 5. Normally distributed?
hist(Stocks.monthly$residuals, breaks = "FD", freq = FALSE)
lines(density(Stocks.monthly$residuals))
rug(Stocks.monthly$residuals)
qqPlot(Stocks.monthly$residuals)
skewness(Stocks.monthly$residuals)
kurtosis(Stocks.monthly$residuals)


# Part E
summary(Stocks.monthly.model)
summary(Stocks.daily.model)

# Parts F and G
confint(Stocks.monthly.model)
confint(Stocks.daily.model)

# Annualized ranges
confint(Stocks.monthly.model) * 12
confint(Stocks.daily.model) * 250