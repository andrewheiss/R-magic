# PMGT 634 Lecture #7
# Author: Ray Nelson
###############################################################################
# load libraries
library(ggplot2)

# function definition got summary of random variable
summaryStats <- function(RV) {
  require(timeSeries)
	meanRV <- mean(RV)
	medianRV <- median(RV)
	sdRV <- sd(RV)
	iqrRV <- IQR(RV)
	skewnessRV <- skewness(RV)
	kurtosisRV <- kurtosis(RV)
	stats <- cbind(meanRV, medianRV, sdRV, iqrRV, skewnessRV, kurtosisRV)
	colnames(stats) <- c("Mean", "Median", "Standard Deviation",
		"InterQuartile Range", "Skewness", "Kurtosis")
	stats <- round(stats, 2)
	return(stats)
}

# Normal pdf
meanNormal <- 16 # mean for normal distribution
sdNormal <- 8 # standard deviation for the normal distribution
X <- seq(meanNormal - 4 * sdNormal, meanNormal + 4 * sdNormal, 0.1) # quantiles
density <- dnorm(X, mean = meanNormal, sd = sdNormal) # normal pdf
plot(X, density, type = "l") # simple plot of density
qplot(x = X, y = density, geom = "line") # qplot of density
rm(meanNormal, sdNormal, X, density) # cleanup

# Equally weighted equity portfolio
sampleSize <- 100
r1 <- rnorm(n = sampleSize, mean = 16, sd = 8)
r2 <- rnorm(n = sampleSize, mean = 10, sd = 5)
r3 <- rnorm(n = sampleSize, mean = 5, sd = 2)
portfolio <- 1/3 * r1 + 1/3 * r2 + 1/3 * r3 # equally weighted portfolio

summaryStats(r1)
summaryStats(r2)
summaryStats(r3)
summaryStats(portfolio)

portfolioCompare <- rbind(summaryStats(r1), summaryStats(r2), summaryStats(r3),
	summaryStats(portfolio))
rownames(portfolioCompare) <- c("r1", "r2", "r3", "portfolio")
portfolioCompare
rm(sampleSize, r1, r2, r3, portfolio, portfolioCompare)

## Chi-Square distribution
# Density Function
degreesFreedom <- 10
X <- seq(0, 40, 0.1) # sequence of quantiles
density <- dchisq(X, df = degreesFreedom) # Chi-Squared density
plot(X, density, type = "l") # simple plot of density
qplot(x = X, y = density, geom = "line") # qplot of density

# Random sample of 50 observations
rChisquare <- rchisq(50, df = degreesFreedom)
summaryStats(rChisquare)
rm(degreesFreedom, rChisquare)