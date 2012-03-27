# Stine and Foster Chapter 25
# Author: Ray Nelson
###############################################################################
# Load Libraries
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster25.RData")
library(car)
library(timeSeries)
library(ggplot2)
library(scales)
library(reshape2)
library(psych)

## Manager Wage Discrimination ################################################
# Preliminary investigation
describe.by(Managers$salary, Managers$gender)
ggplot(data = Managers, aes(x = gender, y = salary, fill = gender)) +
  geom_boxplot(notch = TRUE) +
  coord_flip() +
  opts(legend.position = 'none')
ggplot(data = Managers, aes(x = salary)) + 
  geom_histogram(aes(y = ..density.., fill = gender)) +
  geom_density(size = 0.50) +
  facet_grid(gender ~ .)
ggplot(data = Managers, aes(x = gender, y = salary, fill = gender)) +
  geom_violin() +
  coord_flip() +
  opts(legend.position = 'none')

# Two sample test
t.test(salary ~ gender,  data = Managers, alternative = 'two.sided',
       conf.level = 0.95, var.equal = TRUE)

## Confounding or lurking variables
scatterplot(salary ~ yrsExperience, data = Managers)

## Analysis for just those with 5 years of experience
Managers5 <- subset(Managers, !is.na(years5))
t.test(salary ~ gender,  data = Managers5, alternative = 'two.sided',
       conf.level = 0.95, var.equal = FALSE)
rm(Managers5)

## Separate regressions for females and males
ManagersFemale <- subset(Managers, gender == "female")
ManagersFemale.lm <- lm(salary ~ yrsExperience, data = ManagersFemale)
summary(ManagersFemale.lm)
ManagersMale <- subset(Managers, gender == "male")
ManagersMale.lm <- lm(salary ~ yrsExperience, data = ManagersMale)
summary(ManagersMale.lm)
compareCoefs(ManagersFemale.lm, ManagersMale.lm)

# Combined regression with only differences in intercepts
ManagersIntercepts.lm <- lm(salary ~ gender + yrsExperience, data = Managers)
summary(ManagersIntercepts.lm)

# Combined regression with differences in intercepts and slopes
scatterplot(salary ~ yrsExperience | gender, data = Managers, smooth = FALSE)
ggplot(data = Managers, aes(x = yrsExperience, y = salary, colour = gender)) +
  geom_point() +
  geom_smooth(method = 'lm', size = 0.75) +
  labs(x = 'Years of Experience', y = 'Thousands of Dollars') +
  opts(title = 'Gender Comparison of Walmart Manager Salaries\n')
#ggsave("walmaryManagers.png", width = 7, height = 5, dpi = 100)
ManagersInterceptSlope.lm <- lm(salary ~ gender + yrsExperience +
  gender*yrsExperience, data = Managers)
summary(ManagersInterceptSlope.lm)

# Comparison of models
compareCoefs(ManagersFemale.lm, ManagersMale.lm, ManagersInterceptSlope.lm)
rm(ManagersFemale.lm, ManagersMale.lm, ManagersIntercepts.lm,
   ManagersInterceptSlope.lm)

# Analysis of MRM conditions
Managers.lm <- lm(salary ~ gender + yrsExperience + gender*yrsExperience,
                  data = Managers)
Boxplot(residuals(Managers.lm))
hist(residuals(Managers.lm), freq = FALSE)
lines(density(residuals(Managers.lm)))
residualPlots(Managers.lm)
qqPlot(Managers.lm) # t distribution
qqPlot(residuals(Managers.lm)) # normal Distribution
skewness(residuals(Managers.lm)) # skewness coefficient
kurtosis(residuals(Managers.lm)) # coefficient of excess
shapiro.test(residuals(Managers.lm)) # test residuals for normality

# Check residuals by gender
plotData <- data.frame(Managers, Residuals = residuals(Managers.lm),
                       Fitted = fitted(Managers.lm))
ggplot(data = plotData, aes(x = gender, y = Residuals, fill = gender)) +
  geom_boxplot(notch = TRUE) +
  coord_flip() +
  opts(legend.position = 'none')
ggplot(data = plotData, aes(x = Fitted, y = Residuals, colour = gender)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = 'grey65')

rm(Managers.lm, plotData)