# Time Series Components
# April 4, 2012
# Author: Ray Nelson
###############################################################################
library(quantmod)
library(timeSeries)
library(forecast)

## Retail Sales Seasonally Adjusted:  RSAFS
seriesID <- 'RSAFS'

# Use quantmod to get retail sales from FRED
series <- getSymbols(seriesID, src = "FRED",
                     auto.assign = FALSE, return.class = "timeSeries")
plot(series)
monthplot(series)

# Autocorrelations
tsdisplay(series)

# First regular difference
series.1 <- removeNA(diff(series))
tsdisplay(series.1)

# Exponential Smoothing
series.ets <- ets(series, model = 'ZZZ')
summary(series.ets)
window(series.ets$states, start = end(series.ets$states)[1] - 1)
(series.ets.forecast <- forecast(series.ets, h = 12)) # one year forecast
plot(series.ets.forecast) # plot of one year forecast

# Cubic Spline
(series.spline.forecast <- splinef(series, h = 12))
plot(series.spline.forecast)

rm(seriesID, series, series.1, series.ets, series.ets.forecast,
   series.spline.forecast) # cleanup seasonally adjusted retail sales

## Retail Sales Not Seasonally Adjusted: RSAFSNA
seriesID <- 'RSAFSNA'

# Use quantmod to get retail sales from FRED
series <- getSymbols(seriesID, src = "FRED",
                     auto.assign = FALSE, return.class = "timeSeries")
plot(series)
monthplot(series)

# Autocorrelations
tsdisplay(series)

# First regular difference
series.1 <- removeNA(diff(series))
tsdisplay(series.1)

# First seasonal difference for first difference
series.1.12 <- removeNA(diff(series.1, lag = 12))
tsdisplay(series.1.12)
rm(series.1, series.1.12, seriesID) # cleanup

## Classical Decomposition (Moving Averages)
# Decompose the time series into trend, seasonal, and remainder
series.decompose <- decompose(as.ts(series), type = 'multiplicative')
plot(series.decompose)
# Seasonal adjustment using classical decomposition
series.decompose.seasadj <- seasadj(series.decompose)
plot(series.decompose.seasadj)
# Forecast trend using classical decomposition
splinef(series.decompose.seasadj, h = 12)
# Seasonal components from classical decomposition
sindexf(series.decompose, h = 12) # seasonal index
monthplot(series.decompose$seasonal)
# Adjust the trend forecasts by the seasonal index to give the forecast
rm(series.decompose, series.decompose.seasadj) # cleanup classical decomposition

## Decomposition using stl (loess rather than moving averages)
# Forecast based on stl
series.stl <- stl(series, s.window = "periodic")
plot(series.stl) # plot components
(series.stl.forecast <- forecast(series.stl, h = 12)) # one year forecast
sindexf(series.stl, h = 12) # seasonal index
plot(series.stl.forecast) # plot of one year forecast
rm(series.stl, series.stl.forecast) # cleanup stl

## Exponential Smoothing
series.ets <- ets(series, model = 'ZZZ', damped = FALSE)
summary(series.ets)
window(series.ets$states, start = end(series.ets$states)[1] - 1)
(series.ets.forecast <- forecast(series.ets, h = 12)) # one year forecast
plot(series.ets.forecast) # plot of one year forecast
rm(series.ets, series.ets.forecast) # cleanup exponential smoothing

# Cubic Spline
(series.spline.forecast <- splinef(series, h = 12))
plot(series.spline.forecast)
rm(series.spline.forecast) # cleanup cubic spline

rm(series) # cleanup the data



