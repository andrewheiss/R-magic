#--------------------------------------------------------------
# Create and interpret z-scores for normally distributed data
#--------------------------------------------------------------
# Generate some normally distributed data
# Ordinarily you'd use real data here
normally.distributed.data <- rnorm(1000) * 10
head(normally.distributed.data)

# Convert to z-scores with the scale() function
zscores <- scale(normally.distributed.data)
head(zscores)


#--------------------------------------------------------------
# Recognize and discuss how different data processes can generate different data distributions
#--------------------------------------------------------------
# Um... I'm assuming there are functions in STATA that can create different types of data distributions
# Here are a few 1000-observation samples
normal.distribution <- rnorm(1000, mean=0, sd=1)
chisq.distribution <- rchisq(1000, df=5)
uniform.distriubtion <- runif(1000)
exponential.distribution <- rexp(1000)

# ...and their corresponding histograms
hist(normal.distribution)
hist(chisq.distribution)
hist(uniform.distriubtion)
hist(exponential.distribution)

# Here are all the distributions built in R:
?Distributions

# And here are all the possible distributions available in packages: 
# http://cran.r-project.org/web/views/Distributions.html


#--------------------------------------------------------------
# Use probabilities to identify cutoff points in normally distributed data
#--------------------------------------------------------------
# If you're using hypothetical data that follows a given distribution, you can identify cutoff points (also known as quantiles) using the q... function (i.e. qnorm, qchisq, qunif, etc.)
# So, in a normal distribution with a mean of 5 and a standard deviation of 2, what's the score that corresponds with 85% probability?
qnorm(.85, mean=5, sd=2)  # 7.07

# And what are the scores that correspond to 5%, 10%, 35%, 85%, and 95% probabilities?
qnorm(c(.05, .10, .35, .85, .95), mean=5, sd=2)


# If you're using actual data and want to identify quantiles, use the quantile() function
fake.data <- rnorm(1000, mean=10, sd=3)
quantile(fake.data, .05)  # Quantile at 5% probability
quantile(fake.data, c(.05, .10, .35, .85, .95))  # Quantiles at a bunch of different probabilities


#--------------------------------------------------------------
# Find probabilities associated with cutoff points in normally distributed data
#--------------------------------------------------------------
# If you're using hypothetical data that follows a given distribution, you can calculate the probabilities of different given cutoff points or quantiles using the p... function (i.e. pnorm, pchisq, punif, etc.)
# So, in a normal distribution with a mean of 5 and a standard deviation of 2, what's the probability associated with a score of 3?
pnorm(3, mean=5, sd=2)  # 0.1587; 16th percentile, or 100-16 = 84% chance of seeing at least a 3

# You can do lots of quantiles at once too
pnorm(c(1, 3, 5, 7, 9), mean=5, sd=2)


# Using actual data is a little tricker because there's no convenient function like quantile(). Instead you have to use a weird function named ecdf() (which stands for Empirical Cumulative Distribution Function)
# Use it like this: ecdf(variable)(quantiles)
fake.data <- rnorm(1000, mean=10, sd=3)
ecdf(fake.data)(5)  # Probability/percentile of 5
ecdf(fake.data)(c(5, 10, 12, 14, 17))  # Probability/percentile of a bunch of quantiles


#--------------------------------------------------------------
# Standardize data in STATA using the generate command and formula for z-scores ((x-mean)/st.dev.)
#--------------------------------------------------------------
# Adding a standardized variable to a data frame is really easy, and you don't even need to use the actual z-score formula
# First, create a real data frame (or use your own)
OldFaithful <- faithful

# This is a dataset that comes with R. Here's its explanation:
?faithful

# To create a standardized variable for waiting time, do this:
OldFaithful$waiting.standardized <- scale(OldFaithful$waiting)

# Boom. That's all. View the data frame for fun
View(OldFaithful)


#--------------------------------------------------------------
# Create index variables by combining and weighting standardized scales (including in STATA)
#--------------------------------------------------------------
# The psych package has a function for calculating Cronbach's alpha, which is useful for seeing if variables follow similar trends
library(psych)
alpha(OldFaithful)

# It's easy to combine variables into a standardized index variable
# This is a horrible example, but it shows the syntax
OldFaithful$stupid.index <- scale(OldFaithful$eruptions) + scale(OldFaithful$waiting)
View(OldFaithful)

#--------------------------------------------------------------
# Calculate confidence intervals
#--------------------------------------------------------------
# You can calculate confidence intervals for individual variables by running a t-test
t.test(OldFaithful$waiting)  # Default is 95%
t.test(OldFaithful$waiting, conf.level=0.85)  # But you can specify others

# And if you don't want to see all the t-test output, you can look at just the confidence intervals
t.test(OldFaithful$waiting)$conf.int

# You can also build a linear model using only the intercept as an explanatory variable. Doing this lets you use confint() to get the interval
fake.model <- lm(OldFaithful$waiting ~ 1)
confint(fake.model)
confint(fake.model, level=0.85)


#--------------------------------------------------------------
# Calculate confidence intervals adjusted for different distributions (poisson, binomial)
#--------------------------------------------------------------
# You can calculate confidence intervals for poisson and binomial distributions the same way as t-test confidence intervals, both with a fake model and with binom.test() or poisson.test()

# Basic tests (gives exact interval)
x.poisson <- rpois(1000, 6)  # Random Poisson data
poisson.test(sum(x.poisson), length(x.poisson))
poisson.test(sum(x.poisson), length(x.poisson), conf.level=0.85)

x.binomial <- sample(0:1, 100, replace=TRUE)  # Lots of 0s and 1s
binom.test(sum(x.binomial), length(x.binomial))
binom.test(sum(x.binomial), length(x.binomial), conf.level=0.85)

# Poisson test using GLM and confint()
fake.model.poisson <- glm(x.poisson ~ 1, family=poisson)
exp(confint(fake.model.poisson))  # Poisson GLMs take the log of the mean, so you have to transform the intervals to get the right answer
exp(confint(fake.model.poisson, level=0.85))


#--------------------------------------------------------------
# Generate binary variables from a single nominal (categorical) variable in STATA
#--------------------------------------------------------------
# It's really easy to recode data, although R is smart enough to handle categorical variable without using numbers (0, 1, etc.)
# Here's a sample built-in data set of Titanic passengers
?Titanic
Passengers <- as.data.frame(Titanic)
View(Passengers)

# Look at the structure. If a column is a factor, it's categorical
str(Passengers)

# So, to create a binary variable (which you don't really have to do), you just need to recode the factors
library(car)  # There's a nifty recode() function in the car package
Passengers$male <- recode(Passengers$Sex, '"Male" = 1; "Female" = 0', as.factor.result=TRUE)

# Alternatively, if you don't want to use the recode() function, do this:
Passengers$male1 <- as.factor(ifelse(Passengers$Sex == "Male", 1, 0))  # Use an if statement to recode the Sex variable
Passengers$male1 <- as.factor(Passengers$male1)  # Convert the new variable to a factor (categorical)

# The reason why this is kind of lame is that the 0s and 1s still have to be considered factors to work in any categorical analysis, so there's not really any point to renaming Male to 1 and Female to 0.