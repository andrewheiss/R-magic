#---------------------------------------------------------------------------
# R stuff for MBA 609 Homework 14
# October 2011
# Andrew Heiss
#
# Special instructions:
#   * Update the paths to the datasets
#   * Run each part individually (in RStudio select all the lines for the part and press Ctrl+Enter (or Cmd+ Enter on OS X))
#--------------------------------------------------------------------------

#------------
# Part 1
#------------
# Load the Rcmdr library to get access to numSummary() 
library(Rcmdr)

# Load the dataset 
load("~/Downloads/MBA609-17.RData")

# Build summary statistics table 
numSummary(cars$MPGCity, groups=cars$transmission)


#------------
# Part 2
#------------
# Load the car library (for scatterplot goodness)
library(car) 

# Load the dataset
load("~/Downloads/MBA609-17.RData")

# Build the scatterplot
scatterplot(curbWeight ~ MPGCity, xlab="Miles per Gallon (city)", ylab="Curb Weight", data=cars, reg.line=lm, smooth=TRUE, boxplots=FALSE, spread=FALSE)


#------------
# Part 3
#------------
# Load the car library (for scatterplot goodness)
library(car) 

# Load the dataset
load("~/Downloads/MBA609-17.RData")

# Build the scatterplot
scatterplot(MPGHighway ~ MPGCity, xlab="Miles per Gallon (city)", ylab="Miles per Gallon (highway)", data=cars, reg.line=lm, smooth=TRUE, boxplots=FALSE, spread=FALSE)

# Find Pearson's correlation coefficient 
cor.test(cars$MPGHighway, cars$MPGCity)


#------------
# Part 4
#------------
# Load the car library (for 3D scatterplot goodness)
library(car) 

# Load the dataset
load("~/Downloads/MBA609-17.RData")

# Because missing values are coded as "." instead of "NA", R is interpreting the distribution variable as categorical, or a factor. It needs to be converted to continuous/numeric before it can be plotted. 
# Slow way
#cars$displacementFixed <- as.numeric(as.character(cars$displacement))
# Faster, more efficient way (according to ? factors)
cars$displacementFixed <- as.numeric(levels(cars$displacement))[cars$displacement]

# Build the actual graph
scatter3d(cars$curbWeight, cars$MPGCity, cars$displacementFixed, fit="linear", residuals=TRUE, bg="white", axis.scales=TRUE, grid=TRUE, ellipsoid=FALSE, xlab="Weight", ylab="Miles per Gallon (city)", zlab="Displacement")