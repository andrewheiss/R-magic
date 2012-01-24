library(ggplot2)
library(timeDate)

# Part a
x <- seq(-5,40,by=.5)
y <- dchisq(x,df=10)
plot(x, y)
qplot(x, y, main="Probability density function for the Chi-square distribution with 10 degrees of freedom\n")

# Part b
# Generate a random sample of 50 observations
set.seed(12345)     # Just for consistency's sake
# rchisq() builds a sample
sample <- rchisq(50, df=10)

# Part c
round(mean(sample), 3)
round(median(sample), 3)
round(sd(sample), 3)
round(IQR(sample), 3)
round(skewness(sample), 3)
round(kurtosis(sample, method=c("excess")), 3)