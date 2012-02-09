#---------------------------------------------------------------------------
# PMGT 634 Homework 7
# February 2011
# Andrew Heiss
#---------------------------------------------------------------------------

# Load required libraries
library(mvtnorm)
library(ggplot2)
library(reshape)

# Create two fake stocks
stock1.er <- 15
stock2.er <- 7
stock1.risk <- 8
stock2.risk <- 5
rho <- -0.99
numberTrials <- 10000
stock1.weight <- 0.5

# Put the EV and risk into columns
expected_return <- cbind(stock1.er, stock2.er)
risk <- cbind(stock1.risk, stock2.risk)

# Create the sigma matrix using rho
sigma <- matrix(rho, ncol=2, nrow=2)
diag(sigma) <- 1
(sigma <- t(risk) %*% risk * sigma)

Stocks <- data.frame(rmvnorm(n=numberTrials, mean=expected_return, sigma=sigma))
names(Stocks) <- c("one", "two")
# plot(Stocks, main = "Multivariate Normal Stocks")

# Equally weighted portfolio
Portfolio <- transform(Stocks, portfolio = stock1.weight * one + (1 - stock1.weight) * two)
erPortfolio <- apply(Portfolio, 2, mean)
riskPortfolio <- apply(Portfolio, 2, sd)
cbind(erPortfolio, riskPortfolio)

PortfolioLong <- melt(Portfolio, variable_name = "security")

ggplot(data = PortfolioLong, aes(x = value, fill = security)) +
  geom_density() + 
  facet_grid(security ~ .) +
  opts(title = "Portfolio Distribution, ρ=-1\n", legend.position = "none") +
  geom_vline(xintercept = 0) +
  labs(x = "Return", y = "")



