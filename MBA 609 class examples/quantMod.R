# load the package quantmod
library(quantmod)

# Get Data from google

getSymbols("AAPL", src = "google")

# example of some plots and technical analysis

# Bar Chart

barChart(AAPL)

# Candle Chart

candleChart(AAPL, theme = "white", type = "candles")
reChart(major.ticks = 'months', subset = 'first 16 weeks')

# Technical Analysis

chartSeries(AAPL, theme = "white", TA = "addVo(); addBBands(); addCCI()")