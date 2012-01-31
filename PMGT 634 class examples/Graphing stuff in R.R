# PMGT 634 Lecture #3
# Scripts from R Companion to Applied Regression Chapter 7
# Scripts from ggplot2 Chapter 2
# Author: Ray Nelson
###############################################################################
# Load Libraries
library(car)
library(ggplot2)
library(maps)
library(lattice)
library(latticeExtra)

## Layers

# Scatterplot
data(Prestige)
plot(prestige ~ income, type="n", data=Prestige)
grid(lty="solid")
with(Prestige, points(income, prestige, pch=20, cex=1.5))
points(Prestige$income, Prestige$prestige, )

# Multiple sin and cos graphs
par(mfrow=c(1, 2))
curve(x*cos(25/x), 0.01, pi, n=1000)
curve(sin, 0, 2*pi, ann=FALSE, axes=FALSE, lwd=2)
axis(1, pos=0, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
  	labels=c(0, expression(pi/2), expression(pi),
				expression(3*pi/2), expression(2*pi)))
axis(2, pos=0)
curve(cos, add=TRUE, lty="dashed", lwd=2)
legend(pi, 1, lty=1:2, lwd=2, legend=c("sine", "cosine"), bty="n")
par(mfrow=c(1, 1))

## Colors

rainbow(10)
gray(0:9/9)
colors()[1:20]

palette()
pie(rep(1, 10), col=1:10)

library(colorspace)
pie(rep(1, 100), col=rainbow(100), labels=rep("", 100))
pie(rep(1, 100), col=rainbow_hcl(100), labels=rep("", 100))
pie(rep(1, 100), col=gray(0:100/100), labels=rep("", 100))

## Lattice graphs

# Gender comparison of salaries of professors
xyplot(salary ~ yrs.since.phd | discipline:rank, groups=sex,
		data=Salaries, type=c("g", "p", "r"), auto.key=TRUE)

useOuterStrips(
		bwplot(salary ~ sex | rank + discipline, data=Salaries,
				scales = list(x=list(rot=45), y=list(log=10, rot=0) )),
		strip.left=strip.custom(strip.names=TRUE, var.name="Discipline"))

## Maps

# Maps using base R
data(Depredations)
head(Depredations)
par(mfrow=c(1, 2))
map("county", "minnesota", col=gray(0.4))
with(Depredations, points(longitude, latitude,
				cex=sqrt(early), pch=20))
title("Depredations, 1976-1991", cex.main=1.5)
map("county", "minnesota", col=grey(0.4))
with(Depredations, points(longitude, latitude,
				cex=sqrt(late), pch=20))
title("Depredations, 1992-1998", cex.main=1.5)
par(mfrow=c(1, 1))

# Maps using ggplot2
# Big Cities
data(us.cities)
big_cities <- subset(us.cities, pop > 500000)
qplot(long, lat, data = big_cities) + borders("state", size = 0.5)

tx_cities <- subset(us.cities, country.etc == "TX")
ggplot(tx_cities, aes(long, lat)) +
		borders("county", "texas", color = "grey70") +
		geom_point(color = alpha("black", 0.5))

# Crime
states <- map_data("state")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))

choro <- merge(states, arrests, by = "region")
choro <- choro[order(choro$order), ]
qplot(long, lat, data = choro, group = group,
		fill = assault, geom = "polygon")
qplot(long, lat, data = choro, group = group,
		fill = murder/urbanpop, geom = "polygon")

## ggplot2 graphcis

set.seed(1410) # Make the sample reproducible
dsmall <- diamonds[sample(nrow(diamonds), 100), ]

# simple plot of all data
qplot(carat, price, data = diamonds)

# Mapping point colour to diamond colour (left), and point shape to cut
# quality (right).
qplot(carat, price, data = dsmall, colour = color)
qplot(carat, price, data = dsmall, shape = cut)

# Reducing the alpha value from 1/10 (left) to 1/100 (middle) to 1/200
# (right) makes it possible to see where the bulk of the points lie.
qplot(carat, price, data = diamonds, alpha = I(1/10))
qplot(carat, price, data = diamonds, alpha = I(1/100))
qplot(carat, price, data = diamonds, alpha = I(1/200))

# Smooth curves add to scatterplots of carat vs.\ price. The dsmall
# dataset (left) and the full dataset (right).
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), main="Hey!")
qplot(carat, price, data = diamonds, geom = c("point", "smooth"))

# The effect of the span parameter.  (Left) \code{span = 0.2}, and
# (right) \code{span = 1}.
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), 
		span = 0.2)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), 
		span = 1)

# Using jittering (left) and boxplots (right) to investigate the
# distribution of price per carat, conditional on colour.  As the
# colour improves (from left to right) the spread of values decreases,
# but there is little change in the centre of the distribution.
qplot(color, price / carat, data = diamonds, geom = "jitter")
qplot(color, price / carat, data = diamonds, geom = "boxplot")

# Varying the alpha level.  From left to right: $1/5$, $1/50$, $1/200$.
# As the opacity decreases we begin to see where the bulk of the data
# lies.  However, the boxplot still does much better.
qplot(color, price / carat, data = diamonds, geom = "jitter",
		alpha = I(1 / 5))
qplot(color, price / carat, data = diamonds, geom = "jitter",
		alpha = I(1 / 50))
qplot(color, price / carat, data = diamonds, geom = "jitter",
		alpha = I(1 / 200))

# Displaying the distribution of diamonds.  (Left) \code{geom =
# "histogram"} and (right) \code{geom = "density"}.
qplot(carat, data = diamonds, geom = "histogram")
qplot(carat, data = diamonds, geom = "density")

# Varying the bin width on a histogram of carat reveals interesting
# patterns.  Binwidths from left to right: 1, 0.1 and 0.01 carats. Only
# diamonds between 0 and 3 carats shown.
qplot(carat, data = diamonds, geom = "histogram", binwidth = 1, 
		xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1,
		xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.01,
		xlim = c(0,3))

# Mapping a categorical variable to an aesthetic will automatically
# split up the geom by that variable.  (Left) Density plots are
# overlaid and (right) histograms are stacked.
qplot(carat, data = diamonds, geom = "density", colour = color)
qplot(carat, data = diamonds, geom = "histogram", fill = color)

# Bar charts of diamond colour.  The left plot shows counts and the
# right plot is weighted by \code{weight = carat} to show the total
# weight of diamonds of each colour.
qplot(color, data = diamonds, geom = "bar")
qplot(color, data = diamonds, geom = "bar", weight = carat) +
		scale_y_continuous("carat")

# Two time series measuring amount of unemployment.  (Left) Percent of
# population that is unemployed and (right) median number of weeks
# unemployed.  Plots created with {\tt geom="line"}.
qplot(date, unemploy / pop, data = economics, geom = "line")
qplot(date, uempmed, data = economics, geom = "line")

# Histograms showing the distribution of carat conditional on colour.
# (Left) Bars show counts and (right) bars show densities (proportions
# of the whole).  The density plot makes it easier to compare
# distributions ignoring the relative abundance of diamonds within each
# colour. High-quality diamonds (colour D) are skewed towards small
# sizes, and as quality declines the distribution becomes more flat.
qplot(carat, data = diamonds, facets = color ~ ., 
		geom = "histogram", binwidth = 0.1, xlim = c(0, 3))
qplot(carat, ..density.., data = diamonds, facets = color ~ .,
		geom = "histogram", binwidth = 0.1, xlim = c(0, 3))

## Old Faithful Eruptions
# Density of eruptions with three colour schemes.  (Left) Default
# gradient colour scheme, (middle) customised gradient from white to
# black and (right) 3 point gradient with midpoint set to the mean
# density.

f2d <- with(faithful, MASS::kde2d(eruptions, waiting, 
				h = c(1, 10), n = 50))
df <- with(f2d, cbind(expand.grid(x, y), as.vector(z)))
names(df) <- c("eruptions", "waiting", "density")
erupt <- ggplot(df, aes(waiting, eruptions, fill = density)) +
		geom_tile() +
		scale_x_continuous(expand = c(0, 0)) + 
		scale_y_continuous(expand = c(0, 0)) +
		opts(title = "Durations of Old Faithful Eruptions\n") +
		labs(x = "\nTime between Eruptions (Minutes)",
			y = "Eruption Duration (Minutes)\n")
erupt + scale_fill_gradient(limits = c(0, 0.04))
ggsave("oldFaith.png", )
erupt + scale_fill_gradient(limits = c(0, 0.04), 
		low = "white", high = "black") 
erupt + scale_fill_gradient2(limits = c(-0.04, 0.04), 
		midpoint = mean(df$density))