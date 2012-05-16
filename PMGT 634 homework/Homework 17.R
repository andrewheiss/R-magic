# Time Series Components
# March 21, 2012
# Author: Ray Nelson
###############################################################################
library(quantmod)
library(forecast)
library(timeSeries)

## FRED time series ID
# FRED time series ID: PAYNSA,CURRNS,U36AVS, HOUSTNSA, CEU2000000001
seriesID <- 'USSLIND'

# Use quantmod to get data from FRED
series <- getSymbols(seriesID, src = "FRED", auto.assign = FALSE, return.class = "timeSeries")
plot(series, main="Leading Index for the United States (USSLIND)")
monthplot(series, main="Leading Index for the United States (USSLIND), plotted by month")
seasonplot(as.ts(series), s = 12, pch = 20)

# Autocorrelations
lag.plot(series, lags = 12, main="Scatterplot matrix of lagged periods", pch=20)  # lag 1 by the actual trend, lag 2 by the actual trend, etc.
Acf(series, main="Autocorrelation function")
Pacf(series, main="Partial autocorrelation function")  # Correlation that's only attributable to each lag, starting with 1, then 2, then 3, etc.

# First regular difference
series.1 <- removeNA(diff(series))  # Remove trend
Acf(series.1, main="Autocorrelation function, trend removed")
Pacf(series.1)

# First seasonal difference
series.1.12 <- removeNA(diff(series.1, lag = 12))  # Remove seasonality
Acf(series.1.12, main="Autocorrelation function, seasonality removed")
Pacf(series.1.12)

## Decomposition
# Classical decomposition using moving averages
series.decompose <- decompose(as.ts(series), type = 'additive')
plot(series.decompose)

# Deseasonalize using additive
plot(seasadj(series.decompose))

# Decomposition using stl and loess
series.stl <- stl(series, s.window = "periodic")
plot(series.stl) # plot components
(series.stl.forecast <- forecast(series.stl, h = 12)) # one year forecast
plot(series.stl.forecast, main="Forecast of USSLIND using simple exponential smoothing") # plot of one year forecast

# Deseasonalize using stl
plot(seasadj(series.stl)) # seasonally adjusted data

rm(seriesID, series, series.1, series.1.12, series.decompose, series.stl,
   series.stl.forecast, stlComponents)