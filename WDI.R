library(ggplot2)
library(WDI)
library(maps)
library(timeSeries)

lifeExpectancy <- WDI(indicator="SP.DYN.LE00.FE.IN", start=1989, end=2009)

lifeExpectancy.ts <- as.timeSeries(lifeExpectancy)
plot(lifeExpectancy.ts$SP.DYN.LE00.FE.IN)

world <- subset(lifeExpectancy, iso2c == "1W")
year2000 <- subset(lifeExpectancy, year==2000)

