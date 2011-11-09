# Nonfarm employment forecasting using the forecast package
# Author: Ray Nelson
###############################################################################
# Required libraries
# install using install.packages("quantmod", dependencies = TRUE)
# install using install.packages("forecast", dependencies = TRUE)
# install using install.packages("timeSeries", dependencies = TRUE)

# function definition

forecastFRED <- function(series, horizon, scaleFactor) {
	# series is the FRED symbol
	# horizon is the numer of months or quarters to forecast
	# scaleFactor scales the FRED data
	# load libraries
	require(quantmod)
	require(forecast)
	require(timeSeries)
	# Retrieve data from FRED
	indicator <- getSymbols(series, src = 'FRED', auto.assign = FALSE) / scaleFactor
	# forecast next two years or 24 months
	indicator.ets = ets(as.timeSeries(indicator), model = "ZZZ")
	plot(forecast(indicator.ets, h = horizon))
	print(forecast(indicator.ets, h = horizon))
}

forecastFRED('PAYEMS', 24, 1000)