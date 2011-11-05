# install libraries forecast

library(forecast)

# Get airline passenger data

air <- AirPassengers

fit <- stl(air, s.window = "periodic")
plot(fit)

# Forecasts

forecast(fit)
plot(forecast(fit))
