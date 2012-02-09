#---------------------------------------------------------------------------
# PMGT 634 Homework 7
# February 2011
# Andrew Heiss
#---------------------------------------------------------------------------

# Load required libraries
library(car)
library(ggplot2)

# Load data
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster19.RData")

#-----------------
# Question 19.37
#-----------------

# Part A
Rings <- diamondRings
p <- ggplot(data=Rings, aes(weight, price))
p + geom_point(size=3, colour="black") + 
  # geom_point(size=5, colour="darkgreen", alpha = I(0.2)) + 
  xlab("\nWeight (carats)") + ylab("Price (Singapore $)\n") + 
  geom_smooth() + 
  geom_smooth(method="lm", colour="red", se=FALSE) + 
  opts(title = "Diamond ring price as a function of carat weight\n", plot.title=theme_text(size=18))

# Part B
rings.lm <- lm(price ~ weight, data=Rings)
(rings.summary <- summary(rings.lm))

# Part C
parameters <- c("r²", "σ", "n")
values <- c(rings.summary$r.squared, rings.summary$sigma, length(diamondRings$price))
cbind(parameters, round(values, 3))
rm(parameters, values)

# Part D
prediction <- predict(rings.lm, data.frame(weight = c(0.25, 0.35)), interval = "prediction")
prediction[2,1] - prediction[1,1]

# Part E
rings.lm$coefficients["weight"] * 0.65

# Part G
predict(rings.lm, data.frame(weight = c(0.18)), interval = "prediction")

# Part H
rings.residuals <- as.vector(rings.lm$residuals)
Rings <- transform(Rings, residuals=rings.residuals)

r <- ggplot(data=Rings, aes(as.numeric(row.names(Rings)), residuals))
r + geom_point(size=3, colour="black") + 
  geom_hline(y=0, colour="red", size=1) + 
  xlab("\nIndex") + ylab("Residuals\n") + 
  opts(title="Residuals\n", plot.title=theme_text(size=18))

#-----------------
# Question 19.45
#-----------------

# Part A
OECD <- oecd
p <- ggplot(data=OECD, aes(tradeBalance, gdp))
p <- p + geom_point(size=3, colour="black") + 
  geom_smooth(method="lm", colour="red", se=FALSE) +
  xlab("\nTrade Balance (of GDP)") + ylab("GDP per capita\n") + 
  opts(title = "GDP per capita as a function of trade balance\n", plot.title=theme_text(size=18))
p + geom_smooth() # Add a smooth line just to this plot
  

# Part B
oecd.lm <- lm(gdp ~ tradeBalance, data=OECD)
(oecd.summary <- summary(oecd.lm))

# Part C
parameters <- c("r²", "σ", "n")
values <- c(oecd.summary$r.squared, oecd.summary$sigma, length(OECD$nation))
cbind(parameters, round(values, 3))
rm(parameters, values)

predict(oecd.lm, data.frame(tradeBalance = c(0)), interval = "prediction")

# Part D
oecd.residuals <- as.vector(oecd.lm$residuals)
OECD <- transform(OECD, residuals=oecd.residuals)

r <- ggplot(data=OECD, aes(as.numeric(row.names(OECD)), residuals))
r + geom_point(size=3, colour="black") + 
  geom_hline(y=0, colour="red", size=1) + 
  xlab("\nIndex") + ylab("Residuals\n") + 
  opts(title="Residuals\n", plot.title=theme_text(size=18))

# Part E
trade.high <- which.max(OECD$tradeBalance)
gdp.high <- which.max(OECD$gdp)
us <- which(OECD$nation == "United States")

p + geom_text(data=OECD[trade.high, ], label=OECD$nation[trade.high], vjust=1.5, colour="darkred") + 
  geom_text(data=OECD[gdp.high, ], label=OECD$nation[gdp.high], vjust=1.5, colour="darkgreen") + 
  geom_text(data=OECD[us, ], label=OECD$nation[us], vjust=1.5, colour="darkblue")

OECD[us,"residuals"]