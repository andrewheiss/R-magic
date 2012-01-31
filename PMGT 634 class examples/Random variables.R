# PMGT 634 Lecture #6 Random Variables
# 
# Author: Ray Nelson
###############################################################################
# load Libraries
library(ggplot2)

## Normal Distribution

# Distribution function
newMean <- 2
newSD <- 1
X <- seq(-15, 15, 0.1)
benchmarkProbability <- dnorm(X, mean = 0, sd = 1)
Probability <- dnorm(X, mean = newMean, sd = newSD)
plotData <- data.frame(X, benchmarkProbability, Probability)

ggplot(data = plotData, aes(x = X)) +
  	geom_area(aes(y = Probability), fill = "blue") +
		geom_area(aes(y = benchmarkProbability), fill = "red", alpha = 0.3)
		opts(title = "Probability Density Function\n") +
		labs(x = "\nRandom Variable", y = "Probability\n")

rm(newMean, newSD, X, benchmarkProbability, Probability, plotData)

# Random Number Generation
Mean <- 0
SD <- 1
sampleSize <- 50
randomNormal <- data.frame(Normal = rnorm(sampleSize, mean = Mean, sd = SD))

ggplot(data = randomNormal, aes(x = Normal)) +
		geom_histogram(aes(y = ..density..))+
		geom_density(color = "blue", size = 1)

rm(Mean, SD, sampleSize, randomNormal)

# Quantiles
nQuantiles <- 40
probability <- seq(0, 1, 1 / nQuantiles)
round(qnorm(probability), 2)
rm(nQuantiles, probability)

# Probabilities
Quantiles <- seq(-3, 3, 0.25)
cbind(Quantiles, Prob = round(pnorm(Quantiles), 2))
rm(Quantiles)

# Critical Values
# 95% Confidence Interval
qnorm(0.975)
qnorm(0.025)

# 90% Confidence Interval
qnorm(0.95)
qnorm(0.05)

# other probability distributions
help(distributions)

# Random sample of 50 observations from a chi-squared distribution with 10 df

# Distribution function
df <- 10
X <- seq(0, 40, 0.25)
Probability <- dchisq(X, df = df)
plotData <- data.frame(X, Probability)

ggplot(data = plotData, aes(x = X)) +
		geom_area(aes(y = Probability), fill = "blue") +
opts(title = "Probability Density Function\n") +
		labs(x = "\nRandom Variable", y = "Probability\n")

rm(X,  Probability, plotData)

# Summary statistics for a random sample
require(timeDate) # timeDate has the skewness and kurtosis functions
set.seed(123456) # helps everyone get the same answer
chiSquared <- rchisq(50, df) # random sample of 50 observations
mean(chiSquared)
median(chiSquared)
sd(chiSquared)
IQR(chiSquared)
skewness(chiSquared)
kurtosis(chiSquared) # coefficient of excess

rm(df, chiSquared)