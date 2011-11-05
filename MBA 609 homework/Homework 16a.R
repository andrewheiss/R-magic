#---------------------------------------------------------------------------
# R stuff for MBA 609 Homework 16, question 1
# November 2011
# Andrew Heiss
#
# Special instructions:
#   * Update the path to the dataset
#   * Change "variable" to the variable you want to plot and analyze
#   * Change the "title", "xlab", and "ylab" variables
#   * Change the "histcolor" variable if you want
#   * Set the scale manually or uncomment the `bins <- "Sturges"` line
#
# Note: What follows is a lot of code to make a pretty graph with both a histogram and a boxplot on the same graph. It also adds summary statistics to the plot's caption. It's a little complicated, but awesome :) 
# If you don't care about combining them on one plot, just run the barebone hist() or boxplot() functions
#--------------------------------------------------------------------------

# Load required libraries
library(car)

# Load dataset
load("~/Documents/BYU 2011-2012/Fall 2011/MBA 609/Datasets/MBA609-19.RData")

variable <- techStocks$IBM

# Recode missing values if needed
variable <- recode(variable, "-99=NA")

#---------------------------------------------
#                 Graph setup
#---------------------------------------------

# title <- "Distribution of miles per gallon (highway)"
# xlabel <- "Miles per gallon (highway)"
# ylabel <- "Number of cars"
title <- "IBM monthly returns (1990-2005)"
xlabel <- "Monthly return"
ylabel <- "Frequency"

histColor <- "#283C5D"

# X axis scale
# Either set values below
# uncomment the `bins <- "Sturges"` line and comment out `bins <- seq(…)` for automatic scaling
xmin <- -0.5
xmax <- 0.5
by <- 0.1

# Set up precise bin intervals
bins <- seq(xmin, xmax, by=by)
# bins <- "Sturges"

#---------------------------------------------

# Save current plot settings to revert back to later
def.par <- par(no.readonly = TRUE)

# Build a matrix to hold the plots
# matrix(series of numbers), rows, columns, [byrow=T/F]
m <- matrix(c(1,2), 2, 1)

# Creates something like this:
#      [,1]
# [1,]    1
# [2,]    2
#
# Each number in the matrix corresponds to the area where the first, second, etc. plot will go
#
# Something like this:
# matrix(c(1, 0, 1, 3, 2, 3, 2, 0), nrow = 2, ncol = 4)
#      [,1] [,2] [,3] [,4]
# [1,]    1    1    2    2
# [2,]    0    3    3    0
# 
# means that there will be three plots, two on the top row and one centered in the bottom row

# Build the layout grid
# layout(matrix, width of column(s), height of row(s))
layout(m, 1, c(1,3))

# Set the margins for the top skinny row and build the boxplot
# (bottom, left, top, right)
par(mar=c(0, 6, 3, 2.5), family="Helvetica", font.lab=2, cex.main=1.7, col.axis="#666666")
boxplot <- boxplot(variable, horizontal=T, axes=F, main=title, ylim=c(xmin,xmax))

# Set the margins for the bottom row and build the histogram
par(mar=c(8, 4.5, 0, 1))

# Build colorless histogram
hist(variable, xlab=xlabel, ylab=ylabel, main=NULL, breaks=bins)
# Add background color and lines
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#dadada")
abline(h=axTicks(2), lty=1, col="#ffffff")
# Add colored histogram
histogram <- hist(variable, add=TRUE, col=histColor)

# Calculate statistical stuff
n <- boxplot$n
avg <- mean(variable, na.rm=TRUE)
stdev <- sd(variable, na.rm=TRUE)
coefvar <- stdev / avg
median <- median(variable, na.rm=TRUE)

mtext(paste("n =", n,
            "\t\t Mean:", round(avg, digits=2), 
            "\t\t Median:", round(median, digits=2),
            sep=" "), 1, 5)
mtext(paste("Standard deviation:", round(stdev, digits=2),
            "\t\t Coefficient of variance:", round(coefvar, digits=2),
            sep=" "), 1, 6.4)

# Revert to original plot settings
par(def.par)