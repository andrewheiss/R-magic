# Stine and Foster Chapter 23 MRM steps with corresponding R Commands
# Author: Ray Nelson
# February 28, 2012
################################################################################
# load data and libraries
library(car)
library(forecast) # needed for Box Cox Transformations
library(timeSeries) # needed for skewness and kurtosis

## Step #1 What is the problem to be solved?  ##################################

## Scatterplots and initial model estimation
# Step #2 Scatterplots of response and explanatory variables
summary(dataframe)
numSummary() # from R Commander
scatterplot(y ~ x, data = ) # scatterplot
sp(y ~ x, data = ) # scatterplot
scatterplotMatrix( ~ y + x1 + x2 + x3, data = ) # scatterplot matrix
spm( ~ y + x1 + x2 + x3, data = ) # scatterplot matrix
# Step #3 Identify possible outliers from the scatterplots
# Step #4 Linear relationships? If not then used transformations
BoxCox.lambda(x) # optimal lambda
dataFrame.new <- transform(dataFrame, BoxCox(x, lambda)
# Step #5 Fit a multiple regression model
model.lm <- lm(y ~ x1 + x2 + x3, data = )

## Check Residuals #############################################################
# Step #6 Calculate residuals and fitted values
model.lm$residuals # residuals
residuals(model.lm) # residuals using the extractor function
model.lm$fitted # fitted values
fitted(model.lm) # fitted Values from the extractor function
# Step #7 Calibration plot and look for similar variance and potential outliers
residualPlots(model.lm) # calibration and partial slopes
ncvTest(model.lm) # non constant variance test
spreadLevelPlot(model.lm) # Spread-level Plot (abbreviation: slp)
outlierTest(model.lm)
leveragePlots(model.lm)
influencePlot(model.lm, id.n = 3)
influenceIndexPlot(model.lm, id.n = 3)
# Step #8 Residual versus explanatory variable plots
residualPlots(model.lm)
# Step #9 Check whether residuals are nearly normal
hist(model.lm$residuals, breaks = "FD", freq = FALSE) # histogram 
lines(density(model.lm$residuals)) # density plot
rug(model.lm$residuals) # rug plot on the x axis
qqPlot(model.lm$residuals) # quantile-quantile plot comparison with normal
skewness(model.lm$residuals) # skewness coefficient
kurtosis(model.lm$residuals) # coefficient of excess from time 
jarque.bera.test(model.lm$residuals) # Jarque Bera test for normality
shapiro.test(model.lm$residuals) # Shapiro Wilks test for normality
# Step #10 Check for independence of residuals
durbinWatsonTest(model.lm) # Durbin Watson test (abbreviation: dwt)
acf(model.lm$residuals) # Autocorrelation function
pacf(model.lm$residuals) # Partial autocorrelation function

## Regression Inference ########################################################
# Step #10 Evaluate R-Squared, adjusted R-Squared, and standard error
summary(model.lm) # model summary extractor function
# Step #11 ANOVA analysis
anova(model.lm) # analysis of variance extractor function
# Step #12 Analysis of partial slopes
avPlots(model.lm) # added variable plots gives graphs for the partial slopes
# Step #13 Check for collinearity
cor(explanatoryVariables)
vif(model.lm) # variance inflation factors

## Application of Estimated Model ##############################################
# Step 14 Analyze, evaluate, and interpret the estimated coefficients
confint(model.lm) # confidence intervals for estimated coefficients
avPlots(model.lm) # added variable plots (abbreviation: avp)
marginalModelPlots(model.lm) # marginal model plot (abbreviation: mmps)
# Step 15 Add and drop explanatory variables and make additional transformations
step(model.lm) # stepwise regression
# Step 16 Make predictions and evaluate the confidence intervals for predictions
predict(model.lm, newdata = data.frame(x1 =, x2 =), interval = "prediction")
# Step 17 Incorporate the regression model into a decision framework