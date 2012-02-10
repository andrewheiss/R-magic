#---------------------------------------------------------------------------
# Manipulatable Box-Cox transformations in RStudio
# PMGT 634 - February 10, 2011
# Andrew Heiss
#---------------------------------------------------------------------------

# Load required packages
library(manipulate)
library(car)

# Load prestige data set
data(Prestige)

# Create plotting function
magicPlot <- function(x.var, y.var, x.lambda, y.lambda) {
  x <- bcPower(x.var, x.lambda)  # bcPower performs the Box-Cox transformation with the given lambda
  y <- bcPower(y.var, y.lambda)
  model <- lm(y ~ x)
  xlab <- deparse(substitute(x.var))  # deparse(substitute(x)) gets the passed variable name as a string
  ylab <- deparse(substitute(y.var))
  title <- paste(ylab, "as a function of", xlab)
  plot(x, y, pch=20, xlab=paste(xlab, "( λ =", x.lambda, ")"), ylab=paste(ylab, " ( λ =", y.lambda, ")"), main=title)
  abline(model, col="red", lwd=2)  # Add a regression line
  lines(loess.smooth(x, y), col="blue", lwd=2)  # Add a lowess curve
}

# Plot the manipulatable graph
manipulate(
  magicPlot(Prestige$income, Prestige$prestige, x.lambda, y.lambda),
  x.lambda=slider(-2, 3, initial=1, step=0.1),
  y.lambda=slider(-2, 3, initial=1, step=0.1)
)