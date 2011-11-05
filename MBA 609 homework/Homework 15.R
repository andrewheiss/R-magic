#---------------------------------------------------------------------------
# R stuff for MBA 609 Homework 15
# October 2011
# Andrew Heiss
#
# Special instructions:
#   * Update the path to the NFL dataset
#   * Run this line-by-line to see the results of each test
#--------------------------------------------------------------------------

# Load required packages
library(car)

# Load NFL data
NFL <- read.csv("~/Documents/BYU 2011-2012/Fall 2011/MBA 609/Datasets/NFL.csv")

# Check normalness with Shapiro-Wilk goodness of fit test
plot(density(NFL$Total), main="Distribution of total compensation")
shapiro.test(NFL$Total)
# Null hypothesis = data is distributed normally
# p = .00001*, so reject the null. Data is not normally distributed

# Try transforming the data with a log to make it more normal
NFL$normTotal <- log(NFL$Total)

# Check distribution again
plot(density(NFL$normTotal), main="Distribution of total compensation (transformed)")
shapiro.test(NFL$normTotal)
# p = .00001*, which still rejects the null. Data is still not normal, but it looks better
# T-tests are robust enough to handle mostly non-normal data, so it should be okay

# Check the variance
leveneTest(NFL$normTotal, NFL$Conference)
# Null hypothesis = data is equally variated/shows no variance
# p = .6936, which means that the null is true; the data has equal variance

# Run the actual t-test assuming equal variance
t.test(normTotal~Conference, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=NFL)
# t=-.3014; p = .7631, which means that the null is true. 
# There is no statistically significant difference in means in pay for the two conferences

# Build boxplot chart just for fun
boxplot(Total~Conference, ylab="Total Salary", xlab="Conference", data=NFL, main="Comparison of salaries by NFL conference")
