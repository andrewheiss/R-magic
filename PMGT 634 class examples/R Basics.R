# R Basics
# Author: Ray Nelson
###############################################################################
# load libraries
library(ggplot2)
library(gdata)
library(timeSeries)
library(xts)

# Airline Data ts object from internal R data sets
data(AirPassengers)
AirPassengers
class(AirPassengers)
coredata(AirPassengers)
plot(AirPassengers)

# Read data from Excel using the clipboard and create ts and xts objects
# Copy only the data and no dates to the clipboard
Passengers <- as.numeric(readClipboard())
(Passengers.ts <- ts(Passengers, start = 1949, frequency = 12))
plot(Passengers.ts)
Passengers.xts <- as.xts(Passengers.ts)
plot(Passengers.xts)

# Read data from Excel using the clipboard and create a timeSeries object
# Format the date into a UK format
# Copy the dates and the data with the variable names to the clipboard
Passengers <- read.table("clipboard", header = TRUE)
Passengers.timeSeries <- as.timeSeries(Passengers)
coredata(Passengers.timeSeries)
time(Passengers.timeSeries)
plot(Passengers.timeSeries)

# Read data from Excel using PERL and gdata and plot ts, xts, and timeSeries
Passengers <- read.xls(file.choose(), header = TRUE)
Passengers.timeSeries <- as.timeSeries(Passengers)
plot(Passengers.timeSeries)
plot(as.xts(Passengers.timeSeries))
plot(as.ts(Passengers.timeSeries))

# Parts of a timeSeries object
(Time <- time(Passengers.timeSeries))
(Time <- as.Date(Time))
(Months <- months(time(Passengers.timeSeries)))
(Passengers <- coredata(Passengers.timeSeries))

## Presentation graph of air passengers
plotData <- data.frame(Time, Months, Passengers)

# Simple line plot
qplot(data = plotData, x = Time, y = Passengers, geom = "line")
# Lowess smooth plot by month
qplot(data = plotData, x = Time, y = Passengers, geom = c("point", "smooth"), 
  xlab = "", ylab = "Passengers\n", colour = factor(Months))
# Logarithmic transformation of the data
qplot(data = plotData, x = Time, y = log(Passengers),
	geom = c("point", "smooth"), xlab = "",
	ylab = "Natural Logarithm of Passengers\n")
# Plot for lecture page
ggplot(data = plotData, aes(x = Time, y = Passengers)) +
		geom_point(aes(colour = factor(Months))) +
		geom_smooth() +
		scale_colour_discrete("Months") +
		opts(title = "Airline Passengers\n") +
		labs(x = "", y = "Thousands\n")
ggsave("passengers.png", width = 4, height = 3)

read.table("clipboard", header = TRUE, sep = "\t")