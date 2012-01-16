#---------------------------------------------------------------------------
# PMGT 634 Homework 3
# January 2011
# Andrew Heiss
#---------------------------------------------------------------------------

# Load required libraries
library(gdata)
library(ggplot2)

#-------------
# Question 1
#-------------
# Import data from Excel and the clipboard
diamonds <- read.xls("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/21_diamonds.xlsx")
diamonds.clipboard <- read.delim(file=pipe("pbpaste"), header=T)

# Basic scatterplots
diamondTitle <- "Diamond price as a function of carat weight"
xlab <- "Price ($)"
ylab <- "Carats"
with(diamonds, plot(price, weight, pch=20, main=diamondTitle, xlab=xlab, ylab=ylab))   # x, y notation
with(diamonds, plot(weight ~ price, pch=20, main=diamondTitle, xlab=xlab, ylab=ylab))   # y ~ x notation

# Linear model
(model.diamonds <- with(diamonds, lm(weight~price)))

# Add estimated regression line to the plot
abline(model.diamonds, col="blue", lwd=2)

# Add newlines to labels
diamondTitle <- paste(diamondTitle, "\n", sep="")
xlab <- paste("\n", xlab, sep="")
ylab <- paste(ylab, "\n", sep="")

# Basic scatterplot with ggplot2
(diamond.plot <- ggplot(data=diamonds, aes(x=price, y=weight, xlab=xlab, ylab=ylab, main=diamondTitle)) + 
  geom_point(aes(colour=color)) +
  scale_colour_brewer(palette="Paired") +
  opts(title = diamondTitle, plot.title=theme_text(size=16)) +
  labs(x=xlab, y=ylab))

# Add a smoothed line
(diamond.plot <- diamond.plot + geom_smooth())

# Add a regression line
(diamond.plot <- diamond.plot + geom_smooth(method = "lm", se = FALSE, color="red"))

# Box plot of prices by color using base hist()
with(diamonds, boxplot(price ~ color, col=rainbow(max(as.numeric(color))), main="Diamond price by color", xlab="Color", ylab="Price ($)"))

# Box plot of prices by color using ggplot2
(plot <- qplot(color, price, data=diamonds, geom = "boxplot", main="Diamond price by color\n", xlab="\nColor", ylab="Price ($)\n", fill=color))
plot + opts(legend.position = "none", plot.title=theme_text(size=16)) 


#-------------
# Question 2
#-------------
HomePrices <- read.xls("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/23_home_prices.xls")
HomePrices.clipboard <- read.delim(file=pipe("pbpaste"), header=T)

# Basic scatterplots
homeTitle <- "Home price as a function of square feet"
xlab <- "Price ($)"
ylab <- "Square feet"
with(HomePrices, plot(price, sqFeet, pch=20, main=homeTitle, xlab=xlab, ylab=ylab))   # x, y notation
with(HomePrices, plot(sqFeet ~ price, pch=20, main=homeTitle, xlab=xlab, ylab=ylab))   # y ~ x notation

# Linear model
(model.homes <- with(HomePrices, lm(sqFeet~price)))

# Add estimated regression line to the plot
abline(model.homes, col="blue", lwd=2)

# Add newlines to labels
homeTitle <- paste(homeTitle, "\n", sep="")
xlab <- paste("\n", xlab, sep="")
ylab <- paste(ylab, "\n", sep="")

# Basic scatterplot with ggplot2
(homes.plot <- ggplot(data=HomePrices, aes(x=price, y=sqFeet, xlab=xlab, ylab=ylab)) + 
  geom_point(aes(colour=fireplace)) +
  scale_colour_manual(values = c("red","gold3")) +
  opts(title = homeTitle, plot.title=theme_text(size=16)) +
  labs(x=xlab, y=ylab))

# Add a smoothed line
(homes.plot <- homes.plot + geom_smooth())

# Add a regression line
(homes.plot <- homes.plot + geom_smooth(method = "lm", se = FALSE, color="red"))

# Box plot of prices by color using base hist()
with(HomePrices, boxplot(price ~ fireplace, main="Home price by fireplace", xlab="Fireplace", ylab="Price ($)", col=rainbow(max(as.numeric(fireplace)))))

# Box plot of prices by color using ggplot2
(plot <- qplot(fireplace, price, data=HomePrices, geom = "boxplot", main="Home price by color\n", xlab="\nFireplace", ylab="Price ($)\n", fill=fireplace))
plot + opts(legend.position = "none", plot.title=theme_text(size=16))