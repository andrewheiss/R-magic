# Stine and Foster Chapter 24
# Author: Ray Nelson
###############################################################################
# Load Libraries
load("statistics\\stineFoster\\Chapter24\\stineFoster24.RData")
library(car)
library(timeSeries)
#library(ggplot2)
#library(scales)

## CAPM with Sony ##############################################################

# Identifying Explanatory Variables
scatterplot(SonyPercentChange ~ MarketPercentChange, data = capm)
plot(SonyPercentChange ~ Date, data = capm, type = 'l')
# qplot(x = MarketPercentChange, y = SonyPercentChange, data = capm) + 
#  geom_smooth(method = 'lm', colour = 'cadetblue', size = 1.0) +
#	geom_point(colour = 'indianred') +
#	opts(title = "Capital Asset Price Model\n") +
#	labs(x = "Percentage Change in Market", y = "Percentage Change in Sony")
#ggsave("sonyCAPM.png", width = 7, height = 5, dpi = 100)
# qplot(x = Date, y = SonyPercentChange, data = capm) +
#	geom_hline(yintercept = 0, colour = 'grey65') + 
#	geom_path(colour = 'lightskyblue') +
#	geom_point(colour = 'indianred') +
#	labs(x = '', y = 'Sony Percentage Change')

# Residual analysis
# Linear model
sony.lm <- lm(SonyPercentChange ~ MarketPercentChange, data = capm)
summary(sony.lm)
residualPlots(sony.lm)

# residuals
Boxplot(residuals(sony.lm))
hist(residuals(sony.lm), freq = FALSE)
lines(density(residuals(sony.lm)))
residualPlots(sony.lm)
qqPlot(sony.lm) # t distribution
qqPlot(residuals(sony.lm)) # normal Distribution
skewness(residuals(sony.lm)) # skewness coefficient
kurtosis(residuals(sony.lm)) # coefficient of excess
shapiro.test(residuals(sony.lm)) # test residuals for normality

# Identifying other variables
Sony.df <- capm[, c('SonyPercentChange', 'MarketPercentChange',
                    'DowPercentChange', 'smallBig', 'highLow')]
scatterplotMatrix(Sony.df)
cor(Sony.df)

# Multiple regression with all possible explanatory variables

sony.lm <- lm(SonyPercentChange ~ MarketPercentChange + DowPercentChange +
  smallBig + highLow, data = Sony.df)
(sony.summary <- summary(sony.lm))
anova(sony.lm)
step(sony.lm, direction = "both") # Model selection using AIC

# residuals of multiple regression
Boxplot(residuals(sony.lm))
hist(residuals(sony.lm), freq = FALSE)
lines(density(residuals(sony.lm)))
residualPlots(sony.lm)
qqPlot(sony.lm) # t distribution
qqPlot(residuals(sony.lm)) # normal Distribution
skewness(residuals(sony.lm)) # skewness coefficient
kurtosis(residuals(sony.lm)) # coefficient of excess
shapiro.test(residuals(sony.lm)) # test residuals for normality

# Partial and marginal slopes
avPlots(sony.lm)
marginalModelPlots(sony.lm)

# Collinearity
vif(sony.lm)
sony.summary <- as.data.frame(sony.summary$coefficients)
sony.summary <- round(data.frame(sony.summary[-1,], vif(sony.lm)), 2)
colnames(sony.summary) <- c('Estimate', 'Std Error', 't-Statistics', 
                            'p-Value', 'VIF')
sony.summary

rm(sony.lm, Sony.df, sony.summary)

## Market Segmentation ########################################################

# MRM assumptions
scatterplotMatrix(~ rating + age + income, data = segmentation)
cor(segmentation[, c('rating', 'age', 'income')])

# Model
segmentation.lm <- lm(rating ~ age + income, data = segmentation)
summary(segmentation.lm)
step(segmentation.lm)

# Marginal and partial slope coefficients
marginalModelPlots(segmentation.lm)
avPlots(segmentation.lm)

# MRM Assumptions
qqPlot(segmentation.lm)
skewness(residuals(segmentation.lm))
kurtosis(residuals(segmentation.lm))
shapiro.test(residuals(segmentation.lm))
residualPlots(segmentation.lm)
spreadLevelPlot(segmentation.lm)
ncvTest(segmentation.lm)

# Prediction
NewData <- data.frame(age = 50, income = 80)
predict(segmentation.lm, newdata = NewData, interval = "prediction")

# Different income groups
segmentation.sub <- subset(segmentation, incomeGroup != 'na ')
scatterplot(rating ~ age|incomeGroup, data = segmentation.sub, 
            smooth = FALSE)
#ggplot(data = segmentation.sub, aes(x = age, y = rating, colour = incomeGroup)) +
#	geom_point() +
#	geom_smooth(method = 'lm', se = FALSE, size = 1.0) +
#	labs(x = 'Age', y = 'Rating') +
#	opts(title = "Cellphone Rating\n") +
#	guides(colour = guide_legend(title = 'Income Group'))

rm(segmentation.lm, segmentation.sub)

## Retail Profits ##############################################################

# MRM assumptions
retailProfits <- subset(retailProfits, !is.na(profit))
scatterplotMatrix(retailProfits[ , -1])
round(cor(retailProfits[ , -1]), 2)

# Model
retailProfits.lm <- lm(profit ~ income + disposableIncome + births +
  socialSecurity + CVDeath + older, data = retailProfits)
summary(retailProfits.lm)

# MRM Assumptions
qqPlot(retailProfits.lm)
skewness(residuals(retailProfits.lm))
kurtosis(residuals(retailProfits.lm))
shapiro.test(residuals(retailProfits.lm))
residualPlots(retailProfits.lm)
spreadLevelPlot(retailProfits.lm)
ncvTest(retailProfits.lm)

# Marginal and partial slope coefficients
marginalModelPlots(retailProfits.lm)
avPlots(retailProfits.lm)
step(retailProfits.lm)
vif(retailProfits.lm)

# Parsimonious model
retailProfits.lm <- lm(profit ~ disposableIncome + births + older,
                       data = retailProfits)
summary(retailProfits.lm)

# MRM Assumptions
qqPlot(retailProfits.lm)
skewness(residuals(retailProfits.lm))
kurtosis(residuals(retailProfits.lm))
shapiro.test(residuals(retailProfits.lm))
residualPlots(retailProfits.lm)
spreadLevelPlot(retailProfits.lm)
ncvTest(retailProfits.lm)

# Prediction
NewData <- data.frame(disposableIncome = 22642, births = 14.4, older = 11.4)
round(predict(retailProfits.lm, newdata = NewData, interval = "prediction"), -2)

# cleanup
rm(retailProfits.lm)