#---------------------------------------------------------------------------
# Curved Patterns
#
# Chapter 20 of Robert Stine and Dean Foster, *Statistics for Business: Decision Making and Analysis* 
#
# Andrew Heiss
# MBA 609 - November 2011
#
# Special instructions:
#   * This contains all the R stuff necessary to do all the analysis the chapter talks about
#   * Run each line or each chunk of code individually
#
#--------------------------------------------------------------------------

load("~/Documents/BYU 2011-2012/Fall 2011/MBA 609/R magic/MBA 609 datasets/stineFoster20.RData")

# The functions visual.test() and elasticity() are part of the custom package I've made named "byumpa"
# Download it at https://github.com/andrewheiss/byumpa/downloads and install it using the R package manager
library(byumpa)

#****************************
#----------------------------
# Reciprocal transformation
#----------------------------
#****************************

# Remove outliers, because the book did
cars$mpgFixed <- cars$mpgCity
cars$mpgFixed[cars$mpgFixed > 45] <- NA
cars1 <- na.omit(cars)

x <- cars1$weight
y <- cars1$mpgCity

# Build initial model
model <- lm(y ~ x)
summary(model)
plot(x,y, pch=20)
abline(model, col="red", lwd=2)

# There might be some curvature... check the residual plot
plot(x, model$residuals, main="Residuals", pch=20)
abline(0, 0, col="red", lwd=2)

# Plot looks curved... use a visual test for association to see if there's really a pattern in the residual
visual.test(x, model$residuals)

# Yep. There's a pattern. That's bad. Transform it!

# Look at the original scatterplot again...
plot(x,y, pch=20)
abline(model, col="red", lwd=2)

# There's a bulge in the bottom right, so try either a reciprocal or log transformation
model.adjusted <- lm(100*(1/y) ~ x)
summary(model.adjusted)
plot(x, 100*(1/y))

# Check the residual plots visually
visual.test(x, model.adjusted$residuals)
# Harder to pick out the actual plot, meaning that we have a good model
# Final estimate: Estimated Gallons/Hundred Miles = 1.11 + 1.21 Weight (r^2 = 0.41)

# Compare both linear models
# Create a data frame for predicted values
d <- data.frame(x.new=seq(2,6,.1))
d$y.new <- predict(model.adjusted, data.frame(x=d$x.new))
# Undo the transformation
d$y.new <- 100/d$y.new

# Graph original line and transformed line on the same plot
plot(x,y, pch=20)
abline(model, col="red", lwd=2)
lines(y.new ~ x.new, data=d, lwd=2, col="blue")

# Estimate of a car that weighs 6,400 lbs (Hummer)
# Regular model:
predict(model, data.frame(x=c(6.4)))

# Transformed model = more accurate
100/predict(model.adjusted, data.frame(x=c(6.4)))


#*******************************************
#-------------------------------------------
# "What do you think?" section (p. 496-97)
# More on reciprocal transformation
#-------------------------------------------
#*******************************************

a <- costs$avgTemperature
b <- costs$cost

model.cost <- lm(b ~ a)
plot(a,b, xlab="Average temperature (˚)", ylab="Cost ($)", pch=20)
abline(model.cost, col="red")
summary(model.cost)

visual.test(a, model.cost$residuals)

# Obvious pattern in residuals. There's a bulge in the bottom left of the normal scatterplot, so use a log or reciprocal
model.cost.adj <- lm(1/b ~ a)
plot(a, (1/b), xlab="Average temperature (˚)", ylab="Something...", pch=20)

# Reciprocating works, so create sample dataset with predicted values from new model (while undoing the transformation)
p <- data.frame(a.new=seq(20,70,.1))
p$b.new <- 1/(predict(model.cost.adj, data.frame(a=p$a.new)))

# Graph everything
plot(a,b, xlab="Average temperature (˚)", ylab="Cost ($)", pch=20)
abline(model.cost, col="red")
lines(b.new ~ a.new, data=p, lwd=2, col="blue")


#*****************************
#-----------------------------
# Logarithmic transformation
#-----------------------------
#*****************************

x <- catFood$avgPrice
y <- catFood$salesVolume

# There seems to be a bulge in the data in the lower left
model <- lm(y ~ x)
plot(x, y, pch=20)
abline(model, col="red")

# The residuals verify that there's a nonlinear relationship
plot(x, model$residuals, pch=20)
abline(0,0,col="red")
visual.test(x, model$residuals)

# Because the bulge is in the lower left, a log is appropriate. Transform both variables with a log
model.adj <- lm(log(y) ~ log(x))
plot(log(x), log(y), pch=20)
abline(model.adj, col="red")
summary(model.adj)

# It's linear! Check residuals just for fun
plot(log(x), model.adj$residuals, pch=20)
abline(0,0, col="red")
# And a visual test, also just for fun
visual.test(log(x), model.adj$residuals)

# Untransform everything and plot the estimated line on the original scatterplot
d <- data.frame(x.new=seq(0.6,1.4,.01))
d$y.new <- exp(predict(model.adj, data.frame(x=d$x.new)))

# Graph everything
plot(x,y, pch=20)
abline(model, col="red")
lines(y.new ~ x.new, data=d, lwd=1, col="blue")
# Cool. It matches better.

#*******************
# Elasticity stuff
#*******************

# Slope = elasticity. A 1% increase in price = 2.44% decrease in sales
summary(model.adj)

# Optimal price with elasticity:
# Cost of can is 60 cents...
elasticity(.6, model.adj$coefficients[2])
# $1.016 is the optimal price

#******************
#------------------
# 4M Example 20.1
#------------------
#******************

#*************
# Motivation
#*************
# Store charges $3 for a half-gallon of orange juice
# It costs them $1 to purchase and stock the juice
# Stores typically sell 18 cartons a day


#*********
# Method
#*********
x <- juice$price
y <- juice$sales

model <- lm(y~x)
plot(x,y,pch=20)
abline(model, col="red")

# It looks like there is a bulge. Check the residuals first
plot(x, model$residuals, pch=20)
# Yep. It's curved. Do a visual test just for the heck of it.
visual.test(x, model$residuals)
# There's an obvious residual pattern, so a transformation is needed
# Because there's a bulge in the bottom left, a log is probaly good

model.adj <- lm(log(y) ~ log(x))
plot(log(x), log(y), pch=20)
abline(model.adj, col="red")
visual.test(log(x), model.adj$residuals)
# That fixed it! It's linear!

# Just for fun, try a Box-Cox transformation
library(car)
lambda <- powerTransform(model); lambda
x2 <- bcPower(x, lambda$lambda)
y2 <- bcPower(y, lambda$lambda)
model.boxcox <- lm(y2 ~ x2)
summary(model.boxcox)
plot(x2,y2, pch=20)
abline(model.boxcox, col="red")

# Compare the two estimates
# Replot everything
plot(x,y, pch=20)
abline(model, col="red")

# Build a data frame of predicted values, reversing the transformation
predicted <- data.frame(x.new=seq(.5,5,.1))
predicted$y.new <- exp(predict(model.adj, data.frame(x=predicted$x.new)))
lines(predicted$y.new ~ predicted$x.new, lwd=1, col="blue")

# Syntactically simpler way - make two independent vector
x1 <- seq(.5,5,.1)
y1 <- exp(predict(model.adj, data.frame(x=x1)))
lines(x1, y1, lwd=1, col="blue")


#************
# Mechanics
#************
# Now interpret it
summary(model.adj)
# Equation: Estimated log Sales = 4.812 - 1.752 * log Price


#**********
# Message
#**********

# Calculate the optimal price using the cost of juice and elasticity
optimal_price <- unname(elasticity(1, model.adj$coefficients[2])); optimal_price
# Best price = $2.329 / can

# Estimated sales per day when priced at $3/carton
current_price <- c(3)
current_quantity = unname(exp(predict(model.adj, data.frame(x=c(current_price))))); current_quantity

# Estimated profits at current price
current_profits = current_quantity * (current_price-1); current_profits

# How many would the store sell at $2.33 a carton?
predicted_quantity = unname(exp(predict(model.adj, data.frame(x=c(optimal_price))))); predicted_quantity

# Total daily profits at optimal price and predicted quantity
optimal_profits = predicted_quantity * (optimal_price-1); optimal_profits

# Compare the results of the two prices:
compare_prices <- round(data.frame(price=c(current_price, optimal_price), quantity=c(current_quantity, predicted_quantity), profit=c(current_profits, optimal_profits)), 2); compare_prices

profit_difference <- round(optimal_profits - current_profits, 2); profit_difference
percent_change <- round((profit_difference / optimal_profits * 100),2); percent_change

# Percent change in sales with each 1% increase in price: 
round(unname(model.adj$coefficients[2]), 2)

# The end!