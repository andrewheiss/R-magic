# Smoothing Lecture
# Author: Ray Nelson
###############################################################################
# load libraries
install.packages(c("ggplot2", "quantmod", "forecast", "timeSeries"))
library(quantmod); library(forecast); library(timeSeries); library(ggplot2)

# Identification of points in scatterplot
with(cars, {
			plot(x = horsepower, y = basePrice)
      identify(x = horsepower, y = basePrice, labels = model)
		})

# Apple Computer price data
getSymbols('AAPL', src = 'google')

# Retail sales from FRED
retailSales <- getSymbols("RRSFS", src = 'FRED', auto.assign = FALSE)
retailSales <- as.timeSeries(retailSales)
plotData <- data.frame(as.Date(time(retailSales)), series(retailSales))
names(plotData) <- c("Time", "Sales")

# Old Faithful eruption durations and waiting times from R datasets
data(faithful)

# quantmod and moving averages and exponential moving average
chartSeries(AAPL)
addSMA(n = 10, col = "blue")
addEMA(n = 20, col = "red")

## Retail Sales
# Create a LOWESS smoothed timeplot using ggplot2

qplot(data = plotData, x = Time, y = Sales / 1000, geom = c('point', 'smooth'), span=0.5, xlab = "", ylab = "Billions of Dollars", main = "US Retail Sales\n")

ggplot(data = plotData, aes(x = Time, y = Sales / 1000)) + 
		geom_point(colour = "red") +
		geom_smooth(colour = "blue", size = 0.75, span = 0.2) +
    geom_smooth(colour = "green", size = 0.75, span = 0.5) +
		opts(title = "US Retail Sales\n") +
		labs(x = "", y = "Billions of Dollars")

# forecast of retail sales
retailSales.ets <- ets(retailSales, model = 'ZZZ')
retailSales.forecast <- forecast(retailSales.ets, h = 24)
print(retailSales.forecast)
plot(retailSales.forecast)

# Smoothing of histograms and density traces
# basic histogram of eruption duration
qplot(data = faithful, x = eruptions, geom = 'histogram')

# histogram with a density trace
ggplot(data = faithful, aes(x = eruptions)) +
		geom_histogram(aes(y = ..density..), fill = "cadetblue") +
		geom_density() +
		labs(x = "Eruption Duration", y = "")

# histogram with control over binwidth and degree of smoothing for density
ggplot(data = faithful, aes(x = eruptions)) +
		stat_bin(aes(y = ..density..), binwidth = 0.05, fill = "cadetblue") +
		geom_density(adjust = 1/5) +
		labs(x = "Eruption Duration", y = "")

# basic histogram of waiting time
qplot(data = faithful, x = waiting, geom = 'histogram')

# histogram with a density trace
ggplot(data = faithful, aes(x = waiting)) +
		geom_histogram(aes(y = ..density..), fill = "cadetblue") +
		geom_density()  +
		labs(x = "Eruption Duration", y = "")

# histogram with control over binwidth and degree of smoothing for density
ggplot(data = faithful, aes(x = waiting)) +
		stat_bin(aes(y = ..density..), binwidth = 1, fill = "cadetblue") +
		geom_density(adjust = 1/2)  +
		labs(x = "Eruption Duration", y = "")

# heat map for eruptions and waiting times
# Density of eruptions with three colour schemes.  (First) Default
# gradient colour scheme, (Second) customised gradient from white to
# black and (Third) 3 point gradient with midpoint set to the mean
# density.
f2d <- with(faithful, MASS::kde2d(eruptions, waiting, 
				h = c(1, 10), n = 50))
df <- with(f2d, cbind(expand.grid(x, y), as.vector(z)))
names(df) <- c("eruptions", "waiting", "density")
erupt <- ggplot(df, aes(waiting, eruptions, fill = density)) +
		geom_tile() +
		scale_x_continuous(expand = c(0, 0)) + 
		scale_y_continuous(expand = c(0, 0))
erupt + scale_fill_gradient(limits = c(0, 0.04))
erupt + scale_fill_gradient(limits = c(0, 0.04), 
		low = "white", high = "black") 
erupt + scale_fill_gradient2(limits = c(-0.04, 0.04), 
		midpoint = mean(df$density))

