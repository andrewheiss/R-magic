# PMGT 634 Lecture #8
# Author: Ray Nelson
###############################################################################
library(mvtnorm)
library(ggplot2)
library(reshape)

# securities Parameters
er1 <- 16
er2 <- 10
risk1 <- 8
risk2 <- 5
rho <- 0
numberRandom <- 100000
weight1 <- .5

# Construct expected return vector and variance-covariance matrix
er <- cbind(er1, er2)
risk <- cbind(risk1, risk2)
sigma <- matrix(rho, ncol = 2, nrow = 2)
diag(sigma) <- 1
sigma <- t(risk) %*% risk * sigma
rm(er1, er2, risk1, risk2, rho)

# Generate multivariate normal random variables
Securities <- data.frame(rmvnorm(n = numberRandom, mean = er, sigma = sigma))
names(Securities) <- c("one", "two")
plot(Securities, main = "Multivarate Normal Securities",
     xlab = "Security 1 Return", ylab = "Security 2 Return")
rm(numberRandom, er, risk, sigma)

# Kernel smoothed multivariate density plot of returns
f2d <- with(Securities, MASS::kde2d(one, two, h = c(1, 10), n = 100))
df <- with(f2d, cbind(expand.grid(x, y), as.vector(z)))
names(df) <- c("one", "two", "density")
ggplot(df, aes(one, two, fill = density)) +
  geom_tile() +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "aliceblue", high = "darkred") +
  opts(title = "Multivariate Normal Securities\n", legend.position = "none") +
  labs(x = "Security 1 Return", y = "Security 2 Return")
rm(df, f2d)

# Equally weighted portfolio
Portfolio <- transform(Securities,
                       portfolio = weight1 * one + (1 - weight1) * two)
erPortfolio <- apply(Portfolio, 2, mean)
riskPortfolio <- apply(Portfolio, 2, sd)
cbind(erPortfolio, riskPortfolio)
# change the portfolio into a long format
PortfolioLong <- melt(Portfolio, variable_name = "security")
ggplot(data = PortfolioLong, aes(x = value, fill = security)) +
  geom_density() + 
  facet_grid(security ~ .) +
  opts(title = "Portfolio Distribution\n", legend.position = "none") +
  geom_vline(xintercept = 0) +
  labs(x = "Return", y = "")

# Cleanup
rm(weight1, Securities, Portfolio, erPortfolio, riskPortfolio, PortfolioLong)