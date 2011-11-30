# Homework 21
load("~/Documents/BYU 2011-2012/Fall 2011/MBA 609/R magic/MBA 609 datasets/stineFoster20.RData")
library(byumpa)
library(car)

#-----------------
# 20.32 - Target
#-----------------

x <- target$date
y <- target$operatingIncome

grey.plot(x, y, xlab="", ylab="Operating income", main="Target's operating income, 1990-2005")
model <- lm(y ~ x)
abline(model, col="red")
lines(loess.smooth(x,y), col="blue")

summary(model)
model$coefficients

# It appears curved. Run some tests...
# Regular residual plot
grey.plot(x, residuals(model), xlab="", ylab="Residuals", main="Residuals")
abline(0, 0, col="red")

# There's a definite pattern. Run a visual test for fun
visual.test(x, residuals(model))
# Yep. Definitely not linear.

# Check Box-Cox lambda for best transformation
lambda <- powerTransform(model); lambda
# Lambda is basically zero, indicating a logarithmic transformation. The plot also bulges down and to the left, which according to Tukey means y should be transformed with a log.

y.transformed <- log(y)
model.transformed <- lm(log(y) ~ x)

grey.plot(x, y.transformed, xlab="", ylab="Operating income (Log $)", main="Target's operating income, 1990-2005")
abline(model.transformed, col="red")

# Yay! Much more linear, except for that weird dip in 1995

# Check the residuals...
grey.plot(x, residuals(model.transformed), xlab="", ylab="Residuals", main="Residuals for transformed model")
abline(0,0, col="red")

# There's still a pattern. Run a visual test for fun
visual.test(x, residuals(model.transformed))

# 4th quarter is still easy to spot, but the rest of the data is pretty random, which is good. There's probably some transformation that removes seasonality, but I don't know what it is...

# Reverse the transformation and see how the new model fits with the original scatterplot
# Replot the original data
grey.plot(x, y, xlab="", ylab="Operating income", main="Target's operating income, 1990-2005")
abline(model, col="red")

# Build a data frame of predicted values, reversing the transformation
predicted <- data.frame(x.new=seq(1985,2010,.25))
predicted$y.new <- exp(predict(model.transformed, data.frame(x=predicted$x.new)))
lines(predicted$y.new ~ predicted$x.new, lwd=1, col="blue")

# It looks a lot better. Cool. We have a good model.
summary(model.transformed)

#----------------------
# 20.36 - Used Camrys
#----------------------

x1 <- usedCamry$age
y1 <- usedCamry$askingPrice
model1 <- lm(y1 ~ x1)

grey.plot(x1, y1, xlab="Car age (years)", ylab="Resale value ($1000)", main="Resale value of used Camrys")
abline(model1, col="red")

summary(model1)

# The pattern isn't exactly linear - the price levels out after 15 years and drops steeplyish during the first 5 years. 
# Check the residuals
grey.plot(x1, residuals(model1), xlab="Car age (years)", ylab="Residuals", main="Residuals for basic linear model")
abline(0, 0, col="red")

# There's a curved pattern there. Visual test for fun...
visual.test(x1, residuals(model1))

# Definitely spottable. One of the variables should be transformed. Let's see what Box and Cox say...
powerTransform(model1)

# 0.35, which is halfway between a log and a square root. The data is bulging at the bottom left, so Tukey says the same thing. The book says to use a log, so I will :)
x1.transformed <- log(x1)
# Note - the actual transformation should generally go in the linear model itself, not in a variable
model1.transformed <- lm(y1 ~ log(x1))

grey.plot(x1.transformed, y1, xlab="Log car age", ylab="Resale value ($1000)", main="Resale value of used Camrys (transformed x)")
abline(model1.transformed, col="red")

# Much more linear. Check residuals
grey.plot(x1.transformed, residuals(model1.transformed), xlab="Log car age", ylab="Residuals", main="Residuals for transformed model")
abline(0, 0, col="red")

# No pattern. Visual test:
visual.test(x1.transformed, residuals(model1.transformed))
# Cool. Hard to pick out.

# Put the transformed model on the original plot
grey.plot(x1, y1, xlab="Car age (years)", ylab="Resale value ($1000)", main="Resale value of used Camrys")
abline(model1, col="red")
lines(loess.smooth(x1,y1), col="green")

# Build a data frame of predicted values, reversing the transformation
predicted1 <- data.frame(x.new=seq(0,20,.25))
predicted1$y.new <- predict(model1.transformed, data.frame(x1=predicted1$x.new))
lines(predicted1$y.new ~ predicted1$x.new, lwd=1, col="blue")
# Much better fit.

summary(model1.transformed)

# Predict cars that are 1, 2, 11, and 12 years old
prediction <- data.frame(age=c(1, 2, 11, 12))
prediction$askingPrice <- predict(model1.transformed, data.frame(x1=prediction$age))
View(prediction)

prediction.bad <- data.frame(age=c(1, 2, 11, 12))
prediction.bad$askingPrice <- predict(model1, data.frame(x1=prediction.bad$age))
View(prediction.bad)

# The transformed model reflects the steep drop and eventual levelling out of the price. The normal model doesn't capture that; it underestimates value at the extremes of the car's age and overestimates the value in the middle.