#---------------------------------------------------------------------------
# PMGT 634 Homework 13
# March 2011
# Andrew Heiss
#---------------------------------------------------------------------------

# Load required libraries
library(car)
library(ggplot2)
library(forecast)
library(timeSeries)

# Load data
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster23.RData")

#--------
# 23.39
#--------
Download <- download

# Part A
scatterplotMatrix(~ transferTime + fileSize + hoursPast8, data=Download, main="Scatterplot matrix for download dataset", pch=20)

transferTime.fileSize <- lm(transferTime ~ fileSize, data=Download)
transferTime.hoursPast8 <- lm(transferTime ~ hoursPast8, data=Download)
fileSize.hoursPast8 <- lm(fileSize ~ hoursPast8, data=Download)

summary(transferTime.fileSize)
summary(transferTime.hoursPast8)
summary(fileSize.hoursPast8)

round(cor(Download[, c("transferTime", "fileSize", "hoursPast8")]), 3)

# Part C
download.model <- lm(transferTime ~ fileSize + hoursPast8, data=Download)
(download.model.summary <- summary(download.model))

summary.values <- c(download.model.summary$r.squared, download.model.summary$adj.r.squared, download.model.summary$sigma, nrow(Download))
summary.names <- c("R²", "Adjusted R²", "se", "n")
data.frame("Statistics"=summary.names, "Values"=round(summary.values, 3))

fancy.summary <- as.data.frame(download.model.summary$coefficients)
fancy.summary <- round(data.frame(fancy.summary[-1,], vif(download.model)), 2)
colnames(fancy.summary) <- c('Estimate', 'Std Error', 't-Statistic', 'p-Value', 'VIF')
fancy.summary

# Part D
# Independent?
Acf(residuals(download.model), main="Autocorrelation function estimate for download model")
dwt(download.model, max.lag = 15)

# Equal variation?
residualPlots(download.model, pch=20, main="Calibration and residual plots for download model")

# Normal?
qqPlot(residuals(download.model), pch=20, main="Normal quantile plot of download model residuals")
jarque.bera.test(residuals(download.model))
shapiro.test(residuals(download.model))

# Part F
confint(download.model)  # Partial slopes
confint(transferTime.fileSize)  # Maringal slope

#--------
# 23.44
#--------

Cars <- cars
cars.model <- lm(basePrice ~ horsepower + weight, data=Cars)

# Part A
residualPlots(cars.model, pch=20, main="Calibration and residual plots for cars model")

# Part B
cars.model.log <- lm(log10(basePrice) ~ log10(horsepower) + log10(weight), data=Cars)
residualPlots(cars.model.log, pch=20, main="Calibration and residual plots for transformed cars model")

# Part C
summary(cars.model.log)

# Part D
price.weight <- lm(log10(basePrice) ~ log10(weight), data=Cars)
summary(price.weight)

confint(cars.model.log)
confint(price.weight)
compareCoefs(cars.model.log, price.weight)