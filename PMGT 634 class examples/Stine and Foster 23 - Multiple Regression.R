# Stine and Foster Chapter 23 Multiple Regression Model
# Author: Ray Nelson
# February 27, 2012
################################################################################
# load data and libraries
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster23.RData")
library(car)
library(ggplot2)

## Interpreting Multiple Regression -- Mall Sales  #############################
# Rotating plot using car package
scatter3d(sales ~ competitors + income, data = mallSales, model.summary = TRUE,
          revolutions = 2, surface.col = "cadetblue")
# Scatterplot Matrix using car package
scatterplotMatrix(~ sales + income + competitors + mallVisitors, span = 0.7,
                  data = mallSales, pch = 20)
cov(mallSales) # variance-covariance matrix
cor(mallSales) # correlation matrix

# Multiple regression model
mallSales.lm <- lm(sales ~ income + competitors, data = mallSales)
summary(mallSales.lm)

# partial slope for income
response.lm <- lm(sales ~ competitors, data = mallSales)
explanatory.lm <- lm(income ~ competitors, data = mallSales)
sales <- residuals(response.lm)
income <- residuals(explanatory.lm)
income.lm <- lm(sales ~ income)
summary(income.lm)
plot(sales ~ income)
abline(income.lm, col = 'red')

# cleanup
rm(response.lm, explanatory.lm, sales, income)

# partial slope for competitors
response.lm <- lm(sales ~ income, data = mallSales)
explanatory.lm <- lm(competitors ~ income, data = mallSales)
sales <- residuals(response.lm)
competitors <- residuals(explanatory.lm)
competitors.lm <- lm(sales ~ competitors)
summary(competitors.lm)
plot(sales ~ competitors)
abline(competitors.lm, col = 'red')

# cleanup
rm(response.lm, explanatory.lm, sales, competitors)

# add variables plot
avPlots(mallSales.lm)

# Compare coefficients
compareCoefs(mallSales.lm, income.lm, competitors.lm)

# Marginal slope for income using simple regression
marginalIncome.lm <- lm(sales ~ income, data = mallSales)
summary(marginalIncome.lm)
scatterplot(sales ~ income, data = mallSales)

# Marginal slope for competitors using simple regression
marginalCompetitors.lm <- lm(sales ~ competitors, data = mallSales)
summary(marginalCompetitors.lm)
scatterplot(sales ~ competitors, data = mallSales)

# Marginal model plots
marginalModelPlots(mallSales.lm)

compareCoefs(mallSales.lm, marginalIncome.lm, marginalCompetitors.lm)

## Analysis of  residuals
residualPlots(mallSales.lm)

# Check for constancy of variance
ncvTest(mallSales.lm)
spreadLevelPlot(mallSales.lm)

# check for influencial observations
outlierTest(mallSales.lm)
leveragePlots(mallSales.lm)
influencePlot(mallSales.lm, id.n = 3)
influenceIndexPlot(mallSales.lm, id.n = 3)

# Check whether residuals are nearly normal
hist(mallSales.lm$residuals, breaks = "FD", freq = FALSE)
lines(density(mallSales.lm$residuals))
rug(mallSales.lm$residuals) 
qqPlot(mallSales.lm$residuals)
skewness(mallSales.lm$residuals)
kurtosis(mallSales.lm$residuals)
jarque.bera.test(mallSales.lm$residuals)
shapiro.test(mallSales.lm$residuals)

# Calibration plot
Actual <- mallSales$sales; Fitted <- fitted(mallSales.lm)
plotData <- data.frame(Actual, Fitted)
qplot(data = plotData, x = Fitted, y = Actual, geom = "point") +
  geom_smooth(method = "lm", size = 1.0, colour = "darkred") +
  opts(title = "Calibration Plot\n") +
  labs(x = '\nFitted Sales', y = 'Actual Sales\n')
ggsave("calibration.png", width = 7, height = 5, dpi = 100)

# cleanup
rm(Actual, Fitted, plotData)

# Multiple R-Squared
cor(Actual, Fitted)^2
summary(lm(Actual ~ Fitted))

# Analysis of Variance Table
anova(mallSales.lm)

# Confidence intervals for the estimated parameters
confint(mallSales.lm)

# Prediction and confidence intervals
explanatory <- data.frame(income = 100, competitors = 1)
predict(mallSales.lm, newdata = explanatory, interval = "confidence")
predict(mallSales.lm, newdata = explanatory, interval = "prediction")

# cleanup
rm(mallSales.lm, income.lm, competitors.lm, marginalIncome.lm,
   marginalCompetitors.lm, explanatory)