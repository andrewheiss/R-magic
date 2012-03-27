#---------------------------------------------------------------------------
# PMGT 634 Homework 9
# February 2011
# Andrew Heiss
#---------------------------------------------------------------------------

# Load required libraries
library(car)
library(ggplot2)
library(forecast)
library(quantmod)

# Load data
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster20.RData")

# Fix main variables
CellUS <- cellularUS
CellUS <- transform(CellUS, subscribers = subscribers / 1000000, period.reciprocal = 1 / period)

# Part B
p <- ggplot(data=CellUS, aes(date, subscribers))
p + geom_point(size=3, colour="black") + 
  xlab("\nYear") + ylab("Subscribers\n") + 
  geom_smooth() + 
  geom_smooth(method="lm", colour="red", se=FALSE) + 
  opts(title = "US Cell Phone Subscribers (1985-2006)\n")

# Part C
CellUS.lm.raw <- lm(subscribers ~ date, data=CellUS)
summary(CellUS.lm.raw)
residualPlot(CellUS.lm.raw, pch=20, main="Residuals for raw model")

# Part D
CellUS.lm.log <- lm(log(subscribers) ~ date, data=CellUS)
summary(CellUS.lm.log)
p <- ggplot(data=CellUS, aes(date, log(subscribers)))
p + geom_point(size=3, colour="black") + 
  xlab("\nYear") + ylab("Subscribers (log)\n") + 
  geom_smooth() + 
  geom_smooth(method="lm", colour="red", se=FALSE) + 
  opts(title = "US Cell Phone Subscribers (1985-2006)\n")

(lambda.car <- powerTransform(CellUS.lm.raw))
(lambda.forecast <- BoxCox.lambda(CellUS$subscribers))

p <- ggplot(data=CellUS, aes(date, BoxCox(subscribers, lambda.forecast)))
p + geom_point(size=3, colour="black") + 
  xlab("\nYear") + ylab("Subscribers (cube root-ish)\n") + 
  geom_smooth() + 
  geom_smooth(method="lm", colour="red", se=FALSE) + 
  opts(title = "US Cell Phone Subscribers (1985-2006)\n")

# Part E
CellUS <- transform(CellUS, pctChange = (100 * as.vector(Delt(subscribers))), weirdYear = date - 1984)
p <- ggplot(data=CellUS, aes(weirdYear, pctChange))
p + geom_point(size=3, colour="black") + 
  xlab("\nYears since 1984") + ylab("Percent change\n") + 
  geom_smooth() + 
  geom_smooth(method="lm", colour="red", se=FALSE) + 
  opts(title = "US Cell Phone Subscribers (1985-2006)\n")

# Part F and G
CellUS <- transform(CellUS, reciprocalWeirdYear = 1 / weirdYear)
CellUS.lm.weird <- lm(pctChange ~ reciprocalWeirdYear, data=CellUS)
summary(CellUS.lm.weird)

p <- ggplot(data=CellUS, aes(reciprocalWeirdYear, pctChange))
p + geom_point(size=3, colour="black") + 
  xlab("\n1 / Years since 1984") + ylab("Percent change\n") + 
  geom_smooth() + 
  geom_smooth(method="lm", colour="red", se=FALSE) + 
  opts(title = "US Cell Phone Subscribers (1985-2006)\n")

# Part H
# Predict the number of subscribers in the next period - 2007.0
# Estimated pct growth = 7.6% - applying this to the last observation predicts 236 million subscribers

next.period <- tail(CellUS$weirdYear, n=1) + .5
(next.reciprocal <- 1/next.period)

(prediction.pct <- predict(CellUS.lm.weird, data.frame(reciprocalWeirdYear=next.reciprocal)) / 100)

subscribers.last <- tail(CellUS$subscribers, n=1)
(total <- subscribers.last + (subscribers.last * prediction.pct))

# Better prediction with Box-Cox transformation
(lambda <- BoxCox.lambda(CellUS$subscribers))
CellUS.lm.boxcox <- lm(BoxCox(subscribers, lambda) ~ date, data=CellUS)
next.year <- tail(CellUS$date, n=1) + 0.5
prediction.boxcox <- predict(CellUS.lm.boxcox, data.frame(date=next.year))
(prediction.boxcox <- InvBoxCox(prediction.boxcox, lambda))