load("~/Documents/BYU 2011-2012/Fall 2011/MBA 609/R magic/MBA 609 datasets/stineFoster19.RData")
library(byumpa)
library(car)
library(moments)

x <- cars$horsepower
y <- cars$basePrice
model <- lm(y ~ x)

grey.plot(x, y, main="Car price as a function of horsepower", ylab="Car price ($)", xlab="Horsepower")
abline(model, col="red")

# Check prerequisites for simple regression model
# 1. Is it linear?
grey.plot(x, residuals(model), main="Residuals", ylab="Residuals", xlab="Horsepower")
abline(0, 0, col="red")
# Heteroscedasticity baby!

visual.test(x, residuals(model))
# So easy to pick out. This needs to be transformed. A lot.

lambda <- powerTransform(model); lambda
# Reciprocal 4th root! Crazy!
# I'm not going to transform it though. I'm too lazy.


# 2. Lurking variables?
# There might be, but for simplicity's sake, I won't worry about them

# 3. Independent?
# Yep. No curvature in the residuals (just heteroscedasticity)

# 4. Similar variance?
# Look for heteroscedasticity in the residuals...
grey.plot(x, residuals(model), main="Residuals", xlab="Horespower", ylab="Residuals")
abline(0, 0, col="red")
# There's a definite lack of similar variance

# 5. Normally distributed?
qqPlot(residuals(model), pch=20, main="Normal quantile plot for model", xlab="Normal quantiles", ylab="Residuals")
hist(residuals(model), breaks=20, main="Distribution of residuals", xlab="Model residuals")
# Pretty normal. Phew
# But the central limit theorem might hold. What's the largest of 10x the skewness and kurtosis?
skewness(residuals(model))
kurtosis(residuals(model))
# Sample size should be at least 70. Is it?
length(x)
# Yep. We're good.

summary(model)
confint(model)

model$coefficients[2] * 20
(model$coefficients[2]+10.18) * 20
(model$coefficients[2]-10.18) * 20

predict180 <- predict(model, data.frame(x=c(180)), interval="confidence"); predict180
predict200 <- predict(model, data.frame(x=c(200)), interval="confidence"); predict200
predict200 - predict180

predict(model, data.frame(x=c(200)), interval="confidence")