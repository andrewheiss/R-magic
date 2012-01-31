# Lecture #4 on exploratory data analysis
# 
# Author: Ray Nelson
###############################################################################
# Load libraries
library(car)
library(ggplot2)
library(timeDate)

# Summary statistics for income in the Prestige dataset
min(Prestige$income)
max(Prestige$income)
quantile(Prestige$income, c(0, .25, .50, .75, 1))
mean(Prestige$income) # mean
median(Prestige$income) # median
sd(Prestige$income) # standard deviation
sd(Prestige$income)/mean(Prestige$income) # coefficient of variation
IQR(Prestige$income) # interquartile range
skewness(Prestige$income) # symmetry
kurtosis(Prestige$income) # coefficient of excess or tail thickness

# Histograms and density traces
head(Prestige) # first 6 rows
with(Prestige, hist(income))
with(Prestige, hist(income, breaks="FD", col="gray"))
box()

args(hist.default)
with(Prestige, {
  		hist(income, breaks="FD", freq=FALSE, ylab="Density")
			lines(density(income), lwd=2)
			lines(density(income, adjust=0.5), lwd=1)
			rug(income)
			box()
		})

qplot(data = Prestige, x = income, geom = "histogram")
qplot(data = Prestige, x = income, geom = "histogram", binwidth = 1000)
ggplot(data = Prestige, aes(x = income)) +
	geom_histogram(aes(y = ..density..)) +
	geom_density(colour = "blue", size = 1.0)

# Quantile-Quantile Plots
# Normal distribution versus normal
qqPlot(rnorm(100, 10, 30), distribution = "norm")
# Chi-squared versus normal
qqPlot(rchisq(100, 2), distribution = "norm")
# Prestige versus normal
with(Prestige, qqPlot(income, labels=row.names(Prestige), id.n=3))

# Boxplots
Boxplot(~ income, data=Prestige)

# Covariance
cov(Prestige$income, Prestige$prestige)

# Correlation
cor(Prestige$income, Prestige$prestige)

# Scatterplots

with(Prestige, plot(income, prestige))
scatterplot(prestige ~ income, span=0.6, lwd=3,
		id.n=4, data=Prestige)
scatterplot(prestige ~ income | type, data=Prestige, boxplots=FALSE,
		span=0.75, col=gray(c(0, .25, .5)), id.n=0)

head(Vocab)
nrow(Vocab)
plot(vocabulary ~ education, data=Vocab)
plot(jitter(vocabulary) ~ jitter(education), data= Vocab)
plot(jitter(vocabulary, factor=2) ~ jitter(education, factor=2),
		col="gray", cex=0.5, data=Vocab)
with(Vocab, {
			abline(lm(vocabulary ~ education), lwd=3, lty="dashed")
			lines(lowess(education, vocabulary, f=0.2), lwd=3)
		})

Boxplot(interlocks ~ nation, data = Ornstein, main="(a)")
# Rotating 3d scatterplot
scatter3d(prestige ~ income + education, id.n=3, data=Duncan, revolutions = 6)
# Scatterplot matrix
scatterplotMatrix(~ prestige + income + education + women,
		span=0.7, id.n=0, data=Prestige)

# long and wide datasets from ggplot2 Chapter 9 Manipulating Data

# When the economics dataset is stored in wide format, it is easy to
# create separate time series plots for each variable (left and
# centre), and easy to create scatterplots comparing them (right).
qplot(date, uempmed, data = economics, geom = "line")
qplot(date, unemploy, data = economics, geom = "line")
qplot(unemploy, uempmed, data = economics) + geom_smooth()

# The two methods of displaying both series on a single plot produce
# identical plots, but using long data is much easier when you have
# many variables.  The series have radically different scales, so we
# only see the pattern in the \code{unemploy} variable. You might not
# even notice \code{uempmed} unless you're paying close attention: it's
# the line at the bottom of the plot.
ggplot(economics, aes(date)) + 
		geom_line(aes(y = unemploy, colour = "unemploy")) + 
		geom_line(aes(y = uempmed, colour = "uempmed")) + 
		scale_colour_hue("variable")

emp <- melt(economics, id = "date", 
		measure = c("unemploy", "uempmed"))
qplot(date, value, data = emp, geom = "line", colour = variable)

# When the series have very different scales we have two alternatives:
# left, rescale the variables to a common scale, or right, display the
# variables on separate facets and using free scales.
range01 <- function(x) {
	rng <- range(x, na.rm = TRUE)
	(x - rng[1]) / diff(rng)
}
emp2 <- ddply(emp, .(variable), transform, value = range01(value))
qplot(date, value, data = emp2, geom = "line", 
		colour = variable, linetype = variable)
qplot(date, value, data = emp, geom = "line") + 
		facet_grid(variable ~ ., scales = "free_y")

## Professor Salaries
data(Salaries)
ggplot(data = Salaries, aes(x = rank, y = salary / 1000)) +
	geom_boxplot(aes(fill = rank)) +
	coord_flip() +
	labs( x = '', y = "\nThousands of Dollars") +
	opts(title = "Professor Salaries by Ranks\n", legend.position = 'none')

ggsave("salaries.png", width = 6.8, height = 5, dpi = 100)