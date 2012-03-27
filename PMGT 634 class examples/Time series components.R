# Time Series Components
# March 21, 2012
# Author: Ray Nelson
###############################################################################
library(quantmod)
library(forecast)
library(timeSeries)

## FRED time series ID
# FRED time series ID: PAYNSA,CURRNS,U36AVS, HOUSTNSA, CEU2000000001
seriesID <- 'PAYNSA'

# Use quantmod to get retail sales from FRED
series <- getSymbols(seriesID, src = "FRED", auto.assign = FALSE, return.class = "timeSeries")
plot(series)
monthplot(series)
seasonplot(as.ts(series), s = 12, pch = 20)

# Autocorrelations
lag.plot(series, lags = 12)
Acf(series)
Pacf(series)

# First regular difference
series.1 <- removeNA(diff(series))
Acf(series.1)
Pacf(series.1)

# First seasonal difference
series.1.12 <- removeNA(diff(series.1, lag = 12))
Acf(series.1.12)
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
plot(series.stl.forecast) # plot of one year forecast

# Deseasonalize using stl
plot(seasadj(series.stl)) # seasonally adjusted data

rm(seriesID, series, series.1, series.1.12, series.decompose, series.stl,
   series.stl.forecast, stlComponents)