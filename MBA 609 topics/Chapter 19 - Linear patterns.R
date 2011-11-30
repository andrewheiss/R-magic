load("~/Documents/BYU 2011-2012/Fall 2011/MBA 609/R magic/MBA 609 datasets/stineFoster19.RData")

x <- emeraldDiamonds$weight
y <- emeraldDiamonds$price

plot(x, y, pch=20, xlab="Weight (carats)", ylab="Price ($)")
cor(x, y)

model <- lm(y ~ x)
summary(model)
# Estimated price = 43 + 2670 Weight

# Add the fitted line to the plot
abline(model, col="red")

# Estimate any arbitrary value, like 0.4 carats
predict(model, data.frame(x=c(0.4))) # = $1,111

# Example of extrapolation when interpreting the intercept. This is bad:
plot(x, y, pch=20, xlab="Weight (carats)", ylab="Price ($)", xlim=c(0, 0.5), ylim=c(0, 2000))
abline(model, col="red")

# There's no such thing as a weightless diamond. Interpret the intercept as fixed costs instead. Diamonds inherently cost $43 for labor, storage, etc.


#******************
#------------------
# 4M Example 19.1
#------------------
#******************

#*************
# Motivation
#*************

# Meter readers inside houses... how do you estiamte their gas usage?

#*********
# Method
#*********

# 1. Identify x and y
a <- gasConsumption$degreesBelow65
b <- gasConsumption$gas

# 2. Link slope and intercept to the problem
# The intercept estimates amount of gas usage unrelated to heating (like cooking)
# Slope estimates the average amount of gas used per 1˚ decrease in temperature

# 3. Describe the data
# Data collected over 48 months
# Do other summary statistics, like histograms and stuff

# 4. Check for linear association
plot(a,b,pch=20, xlab="Degrees below 65", ylab="Gas (CCF)")
abline(lm(b ~ a), col="red")
# It's linear!

#************
# Mechanics
#************
model1 <- lm(b ~ a)
summary(model1)

# Estimated gas (CCF) = 26.7274 + 5.69 Degrees below 65
# Average gas usage for heating increases by 5.7 CCF for every drop in 1˚

#**********
# Message
#**********
# So during a billing period with a temperature of 55˚ we can expect a home to use...
predict(model1,data.frame(a=10)) # 55˚ is 10˚ below 65˚...
# ...83.66 CCF of gas


#************
#------------
# Residuals
#------------
#************

# Plot the model's residuals
plot(x, model$residuals, pch=20)
abline(0,0, col="red")

# Distribution is relatively normal, with some skewness to the right
hist(model$residuals)
library(moments)
skewness(model$residuals)

# Standard deviation of the residuals = $170.2


#******************
#------------------
# 4M Example 19.1
#------------------
#******************

#*************
# Motivation
#*************

# How does the price of cars decrease over time?

#*********
# Method
#*********

# 1. Identify x and y
a <- bmwLease$age
b <- bmwLease$price

# 2. Link slope and intercept to the problem
# The intercept estimates the value of a just-sold car, when age = 0
# Slope estimates how the average resale price changes over time

# 3. Describe the data
# Data = prices and ages of 218 used BMWs
# Do other summary statistics, like histograms and stuff

# 4. Check for linear association
plot(a,b,pch=20, xlab="Age (years)", ylab="Price ($)")
abline(lm(b ~ a), col="red")
# It's linear!
# But there might be a lurking variable, like mileage

#************
# Mechanics
#************
model1 <- lm(b ~ a)
summary(model1)

# Estimated price = 39,851 - 2,905 Age
# Average used car resale value decreases by $2,905 each year

# Is it a good fit? Check residuals...
plot(a, model1$residuals)
abline(0,0,col="red")
# Random enough. It's good.

#**********
# Message
#**********
# So a car that is 1.5 years old should have an average resale value of...
predict(model1,data.frame(a=1.5))
# ...$35,493

# Limitations:
# The model only explains 44.83% of the variation, leaving 55%ish to other factors.
# Also, the estimate could be off by $3,367 either way, which is pretty big
