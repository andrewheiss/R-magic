# Chapter 21
load("~/Documents/BYU 2011-2012/Fall 2011/MBA 609/R magic/MBA 609 datasets/stineFoster21.RData")
library(byumpa)
library(car)
library(moments)

x <- berkshire$percentageChangeMarket
y <- berkshire$percentageChangeBH

# Initial scatterplot
grey.plot(x, y, xlab="% change market", ylab="% change Berkshire Hathaway", main="Simple Regression Model")
srm <- lm(y~x)
abline(srm, col="red")

summary(srm)

# Check conditions for simple regression
# 1. Is it linear? 
# Yep. Original plot shows that, as does the residual plot alone and in a visual test
grey.plot(x, residuals(srm), xlab="% change market", ylab="Residuals", main="Residuals")
abline(0,0,col="red")
visual.test(x, residuals(srm))

# 2. Lurking variables? 
# The book says there aren't any

# 3. Independent residuals
grey.plot(berkshire$date, residuals(srm), xlab="Date", ylab="Residuals % change in Berk Hath", main="Residuals over time", type="o")
abline(0, 0, col="red")
# No drifting over time. They're independent.

# 4. Similar variances?
# Look for heteroscedasticity in the residuals
grey.plot(x, residuals(srm), xlab="% change market", ylab="Residuals", main="Residuals")
# Spread of the residuals is pretty constant

# 5. Normally distributed?
hist(residuals(srm))
qqPlot(residuals(srm), pch=20)
# The residuals look mostly normal, except when they shoot out at the end like that. That's probably because the book removed outliers or something
# It's not quite normal, but that's okay; inferences will still work well
# Because it's not normal, we can use the Central Limit Theorem
# Sample size should be greater than 10 times the larger of the skewness or the absolute value of the kurtosis of the residuals
skewness(residuals(srm))
kurtosis(residuals(srm))
# So the sample should have at least 56 observations.
length(residuals(srm))
# It is. Phew.

# Since everything checks out, the model can probably be used to estimate the entire population
summary(srm)
confint(srm)


#-------------------------------
# What Do You Think? on page 526
#-------------------------------

financeCEO <- na.omit(financeCEO)
x <- na.exclude(financeCEO$log10NetSales)
y <- na.exclude(financeCEO$log10TotalCompensation)
model <- lm(y ~ x)

# Part a: Check conditions for regression
# 1. Linearity
# Plot initial values
grey.plot(x, y, main="", ylab=expression(paste("Log"["10"], " Total Compensation")), xlab=expression(paste("Log"["10"], " Net Sales")))
abline(model, col="red")
# Looks good

# 2. Lurking variables
# Probably not

# 3. Independent
# Not time series, so it's probably okay

# 4. Similar variances
# Check for heteroscedasticity in the residuals
grey.plot(x, residuals(model), main="", ylab="Residuals", xlab=expression(paste("Log"["10"], " Net Sales")))
abline(0, 0, col="red")
# Looks good

# 5. Residuals normally distributed?
hist(residuals(model))
qqPlot(residuals(model), pch=20)
# Looks mostly normal, but comes out at the ends. See if it meets the CLT
skewness(residuals(model))
kurtosis(residuals(model))
# The model should have 10*6.837 or 68 observations. 
length(residuals(model))
# It does. 

# Check the model parameters
summary(model)

# Part b: t-value for b1 is greater than 10 (11.61) = b1 is 11 standard intervals away from 0, which is highly significant

# Part c: Find 95% confidence interval for b1
confint(model)
# 0.42 - 0.59

# Part d: CEO claims elasticity is 1/2. Does the model agree?
# Yep. 0.5 lies in the confidence interval

# Part e: Outlier in bottom right is Warren Buffett - he earns a small salary


#------------------
# 4M Example 21.1
#------------------

# Motivation
# Does traffic volume affect gas station sales? How much more gas can the company sell at the busier location?


# Method
# Y = average sales of gas per day (in thousands)
# X = average daily traffic volume (in thousands)
# β[0] = baseline of gas sales regardless of traffic
# β[1] = sales per passing car
# How much more gas expected to be sold at busier location = 40,000-32,000 * confint for β[1]
x <- traffic$traffic
y <- traffic$sales

# Check linearity
model <- lm(y~x)
grey.plot(x, y, main="", xlab="Daily traffic volume (in thousands)", ylab="Sales of gas per day ($, in thousands)")
abline(model, col="red")
# Linear enough. I'm not going to do the whole mega check

# Lurking variables? Probably not there


# Mechanics
# Independent? Yep

# Similar variances?
grey.plot(x, residuals(model), main="", xlab="Daily traffic volume (in thousands)", ylab="Residuals")
abline(0, 0, col="red")
# No heteroscedasticity or patterns. It's good.

# Normal?
hist(residuals(model))
qqPlot(residuals(model), pch=20)
# Normal enough

# Since everything checks, we can make inferences about the population
summary(model)

# 95% confidence interval for the slope:
confint(model)

# Estimated range of additional gallons per day in busier location:
8000 * confint(model)
# 1,507-2,281 more gallons


#------------------
# 4M Example 21.2
#------------------

# Motivation: Overfishing - how to predict the size of a catch in a season with 7,500 days of effort

# Method
# Y = catch weight
# X = days of effort
# Slope = average catch per additional day a boat operates
# Intercept = extrapolation

x <- crabs$daysEffort
y <- crabs$weight
model <- lm(y~x)

# Check linearity
grey.plot(x, y, main="", xlab="Days of effort", ylab="Crab weight (1000 pounds)")
abline(model, col="red")

# Lurking variables? Maybe time, but move on...


# Mechanics
# Check residuals of time
grey.plot(crabs$year, residuals(model), main="", xlab="Year", ylab="Residuals")
abline(0, 0, col="red")
# Seems independent

# Check variance
grey.plot(x, residuals(model), main="", xlab="Days of effort", ylab="Residuals")
abline(0, 0, col="red")

# Check normality
hist(residuals(model))
qqPlot(residuals(model), pch=20)
# Normal enough

summary(model)
# The model checks out, so move on to inference
confint(model)
# 0.133 - 0.189 thousand pounts per boat-day
# The interval for β[0] includes 0, so it's not significant (seen in t and p values)
# But β[1] does not include 0 and it is significant (seen again in t and p)

# Predict catch with 7,500 boat-days
predict(model, data.frame(x=c(7500)), interval="predict")
# 1,173,274 pounds, with a 95% confidence interval of 908,439-1,438,110 pounds
# Or, rounded = 0.9-1.4 million pounds
