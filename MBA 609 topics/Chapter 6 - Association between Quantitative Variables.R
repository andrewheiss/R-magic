#---------------------------------------------------------------------------
# Association between Quantitative Variables
#
# Chapter 6 of Robert Stine and Dean Foster, *Statistics for Business: Decision Making and Analysis* 
#
# Andrew Heiss
# MBA 609 - November 2011
#
# Special instructions:
#   * This contains all the R stuff necessary to do all the analysis the chapter talks about
#   * Run each line or each chunk of code individually
#
#--------------------------------------------------------------------------

# Load dataset
load("~/Documents/BYU 2011-2012/Fall 2011/MBA 609/R magic/MBA 609 datasets/stineFoster06.RData")

#--------------------
# Basic scatterplot
#--------------------

# Plot original scatterplot of heating degree-days and natural gas
plot(gasHeating$heatingDD, gasHeating$naturalGas)


#-------------------------------
# Visual tests for association
#-------------------------------

# Shuffle the natural gas column and replot the data
# Repeat these two lines as many times as you want, or use layout() to put a bunch of them on one plot
random <- transform(gasHeating, naturalGas = sample(naturalGas))
plot(random$heatingDD, random$naturalGas)

# If you can tell the difference between the original plot and the shuffled plots, there's probably association


#------------------------
# Measuring association
#------------------------

#*******
# Create a covariance plot with different colors and quadrants
#*******
x <- gasHeating$heatingDD 
y <- gasHeating$naturalGas

# Color all the dots blue
color <- rep("green", length(x))                # Upper left
color[(x < mean(x, na.rm=TRUE))&(y > mean(y, na.rm=TRUE))] <-"red"      # Upper right
color[(x > mean(x, na.rm=TRUE))&(y < mean(y, na.rm=TRUE))] <-"red"      # Lower left
color[(x < mean(x, na.rm=TRUE))&(y < mean(y, na.rm=TRUE))] <-"green"    # Lower right

# Plot the scatter plot
plot(x, y, col=color, pch=20, xlab="Heating Degree-Days", ylab="Natural Gas (MCF)") 

# Add mean lines for reference
abline(h = mean(y)) 
abline(v = mean(x))

# Add explanatory notes
text(1, 1, "+", cex=1.5)
text(max(x), 1, "-", cex=1.5)
text(1, max(y), "-", cex=1.5)
text(max(x), max(y), "+", cex=1.5)

# Add rugs just for fun
rug(x, col="#999999")
rug(y, side=2, col="#999999")


# Scatterplot matrix
pairs(~naturalGas+heatingDD+sqFt, data=gasHeating, main="Simple Scatterplot Matrix")


#*******
# Get actual measures of association
#*******

# Find covariance
cov(gasHeating$heatingDD, gasHeating$naturalGas)

# Find correlation for one variable
cor(gasHeating$heatingDD, gasHeating$naturalGas)

# Generate a scatterplot matrix
cor(gasHeating)


#--------------------------------------
# Summarizing association with a line
#--------------------------------------

# Estimate the formula for the line
# response variable ~ explanatory variable, or y ~ x
lm(y ~ x)

# Add the line to an existing plot
abline(lm(y ~ x))

lines(loess.smooth(x,y), col="red")