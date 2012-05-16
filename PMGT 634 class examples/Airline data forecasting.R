# Time Series Components
# April 4, 2012
# Author: Ray Nelson
###############################################################################
library(forecast)
library(timeSeries)

# Time series of airline passengers
Series <- AirPassengers
plot(Series)
monthplot(Series)
seasonplot(Series)

# Logarithmic Transformation
Series.log <- log(Series)
plot(Series.log)
rm(Series.log)

# Box Cox Transformation of time series
(best.lambda <- BoxCox.lambda(Series))
Series.bc <- BoxCox(Series, best.lambda)
plot(Series.bc)

# Autocorrelations
lag.plot(Series, lags = 12)
lag.plot(Series.bc, lags = 12)
tsdisplay(Series.bc)

# First regular difference
Series.bc.1 <- removeNA(diff(Series.bc))
tsdisplay(Series.bc.1)

# First seasonal difference of first regular difference
Series.bc.1.12 <- removeNA(diff(Series.bc.1, lag = 12))
tsdisplay(Series.bc.1.12)
rm(Series.bc.1, Series.bc.1.12)

# ARIMA stuff could filter more out, but we don't know those

## Classical Decomposition (Moving Averages)
# Decompose the time series into trend, seasonal, and remainder
# Moving average of order 12
Series.bc.decompose <- decompose(Series.bc, type = "multiplicative")
plot(Series.bc.decompose)
# Seasonal adjustment using classical decomposition
Series.bc.decompose.seasadj <- seasadj(Series.bc.decompose)
plot(Series.bc.decompose.seasadj)
plot(InvBoxCox(Series.bc.decompose.seasadj, best.lambda))
# Forecast trend using classical decomposition
splinef(Series.bc.decompose.seasadj, h = 12)
# Seasonal components from classical decomposition
sindexf(Series.bc.decompose, h = 12) # seasonal index
monthplot(Series.bc.decompose$seasonal)
# Adjust the trend forecasts by the seasonal index and then reverse BC
rm(Series.bc.decompose, Series.bc.decompose.seasadj) # cleanup

## Decomposition using stl (loess rather than moving averages)
# Forecast based on stl
Series.bc.stl <- stl(Series.bc, s.window = "periodic")
plot(Series.bc.stl) # plot components
(Series.bc.stl.forecast <- forecast(Series.bc.stl, h = 12,
                                    lambda = best.lambda)) # one year forecast
summary(Series.bc.stl.forecast)
plot(Series.bc.stl.forecast) # plot of one year forecast
rm(Series.bc.stl, Series.bc.stl.forecast, Series.bc) # cleanup stl

# Exponential Smoothing
Series.ets <- ets(Series, model = 'ZZZ', damped = FALSE, lambda = best.lambda)
summary(Series.ets)
round(window(Series.ets$states, start = end(Series.ets$states)), 3)
(Series.ets.forecast <- forecast(Series.ets, h = 12))
plot(Series.ets.forecast)
rm(Series.ets, Series.ets.forecast)

# Cubic Spline
(Series.spline.forecast <- splinef(Series, h = 12, lambda = best.lambda))
plot(Series.spline.forecast)
# Apply seasonal indexes from decomposition or exponential smoothing
rm(Series.spline.forecast) # cleanup cubic spline

rm(Series, best.lambda)