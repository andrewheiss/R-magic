load("~/Documents/BYU 2011-2012/Fall 2011/MBA 609/R magic/MBA 609 datasets/stineFoster19.RData")
library(byumpa)
library(car)
library(moments)

#---------------
# 19.48: Apple
#---------------

x <- apple$marketReturn
y <- apple$appleReturn
model <- lm(y ~ x)
grey.plot(x,y, main="Apple returns as a function of market returns", xlab="Market returns", ylab="Apple returns")
abline(model, col="red")
# It looks mostly linear, but we can check, just to make sure

# Plot the residuals
grey.plot(x, residuals(model), main="Residuals for market vs. Apple returns", xlab="Market return", ylab="Residuals")
abline(0, 0, col="red")
# identify(x, residuals(model))
# Biggest outliers = rows 205, 237, and 241
outliers <- c(205, 237, 241)
apple$date[outliers]
apple$appleReturn[outliers]

# There's no curvature. Do a visual test just to make sure.
visual.test(x, residuals(model))
# I have no idea which one is which. It's linear.

# Since it's linear, we can summarize the model
summary(model)

# Compare difference between Apple returns when market goes up and down by 2%
predicted <- data.frame(market=c(0.02, -0.02))
predicted$apple <- predict(model, data.frame(x=predicted$market))
predicted

# Check the relationship with Apple's excess returns instead
# Subtract treasury rate from regular returns to determine excess returns
y.excess <- apple$appleReturn - apple$TreasuryReturn
x.excess <- apple$marketReturn - apple$TreasuryReturn
model.excess <- lm(y.excess ~ x.excess)
grey.plot(x.excess, y.excess, main="Excess Apple returns as a function of market returns", xlab="Excess market returns", ylab="Apple returns")
abline(model.excess, col="red")
abline(model, col="blue")
legend("bottomright", inset=c(0.1,0.05), lty=c(1,1), lwd=c(2.5, 2.5), legend=c("Excess returns", "All returns"), col=c("red", "blue"), cex=1)

summary(model.excess)
summary(model)
# Not much changed at all. The two models are basically identical.


#-----------------------
# 21.37: Diamond rings
#-----------------------

x1 <- diamondRings$weight
y1 <- diamondRings$price
model1 <- lm(y1 ~ x1)
grey.plot(x1, y1, main="Diamond price as a function of weight", xlab="Diamond weight (carats)", ylab="Price (Singapore dollars)")
abline(model1, col="red")
summary(model1)

# Check prerequisites for simple regression model
# 1. Is it linear?
grey.plot(x1, residuals(model1), main="Residuals of diamond price", xlab="Diamond weight (carats)", ylab="Residuals")
abline(0, 0, col="red")
visual.test(x1, residuals(model1))
# No apparent pattern in the residual plot, and it's really hard to spot. It's linear.

# 2. Lurking variables?
# Nah. It seems like a valid, simple relationship

# 3. Independent?
# Not as important... these aren't time series data

# 4. Similar variance?
# Look for heteroscedasticity in the residuals...
grey.plot(x1, residuals(model1), main="Residuals of diamond price", xlab="Diamond weight (carats)", ylab="Residuals")
abline(0, 0, col="red")
# It's good. Everything is clustered about equally.

# 5. Normally distributed?
hist(residuals(model1))
skewness(residuals(model1))
qqPlot(residuals(model1), pch=20, main="Normal quantile plot for model", xlab="Normal quantiles", ylab="Residuals")
# There's very little skewness, the histogram looks great, and the qqplot shows good normality. It's a good model. Yay for contrived data! :)

confint(model1)

# Check if $800 is too much for a .25 carat diamond
predict(model1, data.frame(x1=c(.25)), interval="predict")
# Yep