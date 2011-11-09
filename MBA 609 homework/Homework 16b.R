#---------------------------------------------------------------------------
# R stuff for MBA 609 Homework 16, question 2
# November 2011
# Andrew Heiss
#
# Special instructions:
#   * Update the path to the dataset
#   * Change any of the colors below. R will cycle through the array of colors (so if you have three colors and four categories to plot, the first and the last will be the same color)
#--------------------------------------------------------------------------

# Load dataset
load("~/Documents/BYU 2011-2012/Fall 2011/MBA 609/R magic/MBA 609 datasets/MBA609-19.RData")

# Load required libraries
library(car)

# Fill colors
light <- "#661C0E"
medium <- "#D6AA5C"
dark <- "#5D7359"
colors <- c(light, medium, dark)

# Border colors
light1 <- "#240A05"
medium1 <- "#6E582F"
dark1 <- "#1D241C"
borders <- c(light1, medium1, dark1)

# Convert the data from wide to long
StackedData <- stack(techStocks[, c("Dell","IBM","Microsoft")])
names(StackedData) <- c("return", "company")

# Save current plot settings to revert back to later
def.par <- par(no.readonly = TRUE)

# Change plot formatting
par(family="Helvetica", font.lab=2, cex.main=1.7, col.axis="#666666")

# Build plot for boxplot
boxplot(return~company, ylab="Company", xlab="Monthly return", data=StackedData, horizontal=TRUE, ylim=c(-.4, .6), main="Monthly returns for Dell, IBM, and Microsoft (1990-2005)")

# Add grey background with white lines
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="#dadada")
abline(v=axTicks(1), lty=1, col="#ffffff")

# Build colored boxplots
boxplot(return~company, data=StackedData, col=colors, border=borders, horizontal=TRUE, add=TRUE)

# Revert to original plot settings
par(def.par)