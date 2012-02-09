#---------------------------------------------------------------------------
# PMGT 634 Homework 6
# January 2011
# Andrew Heiss
#---------------------------------------------------------------------------

# Load required libraries
library(timeSeries)
library(quantmod)

# Cool summary function
summaryStats <- function(RV) {
  meanRV <- mean(RV)
  medianRV <- median(RV)
  sdRV <- sd(RV)
  iqrRV <- IQR(RV)
  skewnessRV <- skewness(RV)
  kurtosisRV <- kurtosis(RV)
  stats <- cbind(meanRV, medianRV, sdRV, iqrRV, skewnessRV, kurtosisRV)
  colnames(stats) <- c("Mean", "Median", "SD", "IQR", "Skewness", "Kurtosis")
  return(stats)
}

# Perfectly normally distributed fake stocks
stock1 <- rnorm(n = 100, mean = 16, sd = 8)
stock2 <- rnorm(n = 100, mean = 10, sd = 5)
stock3 <- rnorm(n = 100, mean = 5, sd = 2)
portfolio <- 1/3 * stock1 + 1/3 * stock2 + 1/3 * stock3

summaryStats(stock1)
summaryStats(stock2)
summaryStats(stock3)
summaryStats(portfolio)


# Build a real sample portfolio with just the deltas
setDefaults(getSymbols, src='google', auto.assign=FALSE)
microsoft <- getSymbols('MSFT')
google <- getSymbols('GOOG')
apple <- getSymbols('AAPL')
costco <- getSymbols('COST')
amazon <- getSymbols('AMZN')

microsoft.delta <- na.omit(as.vector(Delt(microsoft$MSFT.Close)))
google.delta <- na.omit(as.vector(Delt(google$GOOG.Close)))
apple.delta <- na.omit(as.vector(Delt(apple$AAPL.Close)))
costco.delta <- na.omit(as.vector(Delt(costco$COST.Close)))
amazon.delta <- na.omit(as.vector(Delt(amazon$AMZN.Close)))

rm(microsoft, google, apple, costco, amazon)

portfolio <- 1/5 * microsoft.delta + 1/5 * google.delta + 1/5 * apple.delta + 1/5 * costco.delta + 1/5 * amazon.delta


# Show all the summaries
100 * round(summaryStats(microsoft.delta), 5)
100 * round(summaryStats(google.delta), 5)
100 * round(summaryStats(apple.delta), 5)
100 * round(summaryStats(costco.delta), 5)
100 * round(summaryStats(amazon.delta), 5)

100 * round(summaryStats(portfolio), 5)