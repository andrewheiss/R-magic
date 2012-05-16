# Time Series Components
# April 4, 2012
# Author: Ray Nelson
###############################################################################
library(quantmod)
library(timeSeries)
library(forecast)

## Retail Sales Not Seasonally Adjusted: RSAFSNA
seriesID <- 'RSAFSNA'

# Use quantmod to get retail sales from FRED
series <- getSymbols(seriesID, src = "FRED", auto.assign = FALSE, return.class = "timeSeries")
plot(series, main="Raw time series (RSAFSNA)")
monthplot(series, main="Month plot (RSAFSNA)")

# Autocorrelations
tsdisplay(series, points=FALSE, main="Autocorrelations for raw data")

# First regular difference
series.1 <- removeNA(diff(series))
tsdisplay(series.1, points=FALSE, main="Autocorrelations for first regular difference (trend removed)")

# First seasonal difference for first difference
series.1.12 <- removeNA(diff(series.1, lag = 12))
tsdisplay(series.1.12, points=FALSE, main="Autocorrelations for first seasonal difference (seasonality removed)")
rm(series.1, series.1.12, seriesID) # cleanup

## Classical Decomposition (Moving Averages)
# Decompose the time series into trend, seasonal, and remainder
series.decompose <- decompose(as.ts(series), type = 'multiplicative')  # use additive to get actual numbers; use multiplicative to get percents
plot(series.decompose)
# Seasonal adjustment using classical decomposition
series.decompose.seasadj <- seasadj(series.decompose)
plot(series.decompose.seasadj)
# Forecast trend using classical decomposition
plot(splinef(series.decompose.seasadj, h = 12), type="l")
# Seasonal components from classical decomposition
series.decompose$seasonal
monthplot(series.decompose$seasonal, main="Percent seasonality adjustment for each month")
# Adjust the trend forecasts by the seasonal index to give the forecast
rm(series.decompose, series.decompose.seasadj) # cleanup classical decomposition

## Decomposition using stl (loess rather than moving averages)
# Forecast based on stl
series.stl <- stl(series, s.window = "periodic")
plot(series.stl) # plot components
(series.stl.forecast <- forecast(series.stl, h = 12)) # one year forecast
plot(series.stl.forecast) # plot of one year forecast
rm(series.stl, series.stl.forecast) # cleanup stl

## Exponential Smoothing
series.ets <- ets(series, model = 'AAA', damped = FALSE)
summary(series.ets)
window(series.ets$states, start = end(series.ets$states)[1] - 1)
(series.ets.forecast <- forecast(series.ets, h = 12)) # one year forecast
plot(series.ets.forecast, main="Holt-Winters forecast") # plot of one year forecast
rm(series.ets, series.ets.forecast) # cleanup exponential smoothing

# Cubic Spline
(series.spline.forecast <- splinef(series, h = 12))
plot(series.spline.forecast)
rm(series.spline.forecast) # cleanup cubic spline

rm(series) # cleanup the data

## Retail Sales Not Seasonally Adjusted: RSAFSNA
seriesID <- 'USCSCOMHPISA'

# Use quantmod to get retail sales from FRED
series <- getSymbols(seriesID, src = "FRED", auto.assign = FALSE, return.class = "timeSeries")
plot(series, main="Raw time series (USCSCOMHPISA)")
monthplot(series, main="Month plot (USCSCOMHPISA)")

# Autocorrelations
tsdisplay(series, points=FALSE, main="Autocorrelations for raw data")

# First regular difference
series.1 <- removeNA(diff(series))
tsdisplay(series.1, points=FALSE, main="Autocorrelations for first regular difference (trend removed)")

# No need to remove seasonality

## Classical Decomposition (Moving Averages)
# Decompose the time series into trend, seasonal, and remainder
series.decompose <- decompose(as.ts(series), type = 'additive')  # use additive to get actual numbers; use multiplicative to get percents
plot(series.decompose)
# Seasonal adjustment using classical decomposition
series.decompose.seasadj <- seasadj(series.decompose)
plot(series.decompose.seasadj)
# Forecast trend using classical decomposition
plot(splinef(series.decompose.seasadj, h = 12), type="l")
# Seasonal components from classical decomposition
series.decompose$seasonal
monthplot(series.decompose$seasonal, main="Percent seasonality adjustment for each month")
# Adjust the trend forecasts by the seasonal index to give the forecast
rm(series.decompose, series.decompose.seasadj) # cleanup classical decomposition

## Decomposition using stl (loess rather than moving averages)
# Forecast based on stl
series.stl <- stl(series, s.window = "periodic")
plot(series.stl) # plot components
(series.stl.forecast <- forecast(series.stl, h = 12)) # one year forecast
plot(series.stl.forecast) # plot of one year forecast
rm(series.stl, series.stl.forecast) # cleanup stl

## Exponential Smoothing
series.ets <- ets(series, model = 'AAN', damped = FALSE)
summary(series.ets)
window(series.ets$states, start = end(series.ets$states)[1] - 1)
(series.ets.forecast <- forecast(series.ets, h = 12)) # one year forecast
plot(series.ets.forecast, main="Holt-Winters forecast (α=0.9993)") # plot of one year forecast

series.ets <- ets(series, model = 'AAN', damped = FALSE, alpha=0.1)
summary(series.ets)
window(series.ets$states, start = end(series.ets$states)[1] - 1)
(series.ets.forecast <- forecast(series.ets, h = 12)) # one year forecast
plot(series.ets.forecast, main="Holt-Winters forecast (α=0.1)") # plot of one year forecast
rm(series.ets, series.ets.forecast) # cleanup exponential smoothing

# Cubic Spline
(series.spline.forecast <- splinef(series, h = 12))
plot(series.spline.forecast)
rm(series.spline.forecast) # cleanup cubic spline

rm(series) # cleanup the data


