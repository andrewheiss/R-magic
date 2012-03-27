# Stine and Foster Chapter 24
# Author: Ray Nelson
###############################################################################
# Load Libraries
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster24.RData")
library(car)
library(timeSeries)
# library(ggplot2)

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
sony.summary <- as.data.frame(sony.summary$coefficients)
sony.summary <- round(data.frame(sony.summary[-1,], vif(sony.lm)), 2)
colnames(sony.summary) <- c('Estimate', 'Std Error', 't-Statistics', 
                            'p-Value', 'VIF')
sony.summary

rm(sony.lm, Sony.df, sony.summary)