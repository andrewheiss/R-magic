#---------------------------------------------------------------------------
# PMGT 634 Homework 11
# February 2011
# Andrew Heiss
#---------------------------------------------------------------------------

# Load required libraries
library(car)
library(ggplot2)
library(forecast)
library(timeSeries)

# Load data
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster22.RData")

#--------
# 22.36
#--------

Shopping <- convenience

shopping.model <- lm(sales ~ volume, data=Shopping)
Shopping$residuals <- shopping.model$residuals

# Check conditions for SRM, just for fun
# 1. Linear?
p <- ggplot(aes(x=volume, y=sales), data=Shopping)
p <- p + geom_point() + 
  geom_smooth() + 
  geom_smooth(method="lm", colour="red", se=FALSE) +
  labs(x="\nVolume (gallons)", y="Sales ($)\n") +
  opts(title="Sales as a function of volume\n")
p

r <- ggplot(aes(x=volume, y=residuals), data=Shopping)
r <- r + geom_point() + 
  geom_smooth() + 
  geom_smooth(method="lm", colour="red", se=FALSE) +
  labs(x="\nVolume (gallons)", y="Residuals\n") +
  opts(title="Residual plot\n")
r

# 2. Lurking variables?
# There might be, but for simplicity's sake, I won't worry about them

# 3. Independent?
dwt(shopping.model, max.lag = 15)
Acf(Shopping$residuals)

# 4. Similar variance?
ncvTest(shopping.model)

# 5. Normally distributed?
hist(Shopping$residuals, breaks = "FD", freq = FALSE)
lines(density(Shopping$residuals))
rug(Shopping$residuals)
qqPlot(Shopping$residuals)
skewness(Shopping$residuals)
kurtosis(Shopping$residuals)
jarque.bera.test(Shopping$residuals)
shapiro.test(Shopping$residuals)


# Part A
r
Acf(Shopping$residuals)


# Part B
dwt(shopping.model)


# Parts C and D
# Remove row 14
Shopping.sans.outliers <- Shopping[-14,]
shopping.model.sans.outliers <- lm(sales ~ volume, data=Shopping.sans.outliers)

mean(Shopping$volume)

summary(shopping.model)
summary(shopping.model.sans.outliers)

# Create a subset of points to label (in this case just one)
labels <- Shopping[14,]

p1 <- ggplot(aes(x=volume, y=sales), data=Shopping)
p1 + geom_point() + 
  geom_smooth(aes(colour="Smoothed")) + 
  geom_smooth(method="lm", se=FALSE, aes(colour="Fitted")) +
  geom_smooth(aes(x=volume, y=sales, colour="Fitted (no outlier)"), data=Shopping.sans.outliers, method="lm", se=FALSE) + 
  geom_smooth(aes(x=volume, y=sales, colour="Smoothed (no outlier)"), data=Shopping.sans.outliers) +
  geom_point(data=labels, aes(x=volume, y=sales), colour="darkgreen", shape=17, size=3) +
  geom_text(data=labels, aes(x=volume, y=sales, label="Row 14"), vjust=0.25, hjust=-0.25, colour="darkgreen") + 
  labs(x="\nVolume (gallons)", y="Sales ($)\n") +
  opts(title="Sales as a function of volume\n (with and without row 14)\n") +
  scale_colour_manual(name="Lines", values=c("Smoothed"="blue", "Fitted"="red", "Fitted (no outlier)"="yellow4", "Smoothed (no outlier)"="yellow2")) 

# Woot! scale_colour_manual is how you can get custom legends. Magic!


#--------
# 22.39
#--------

Seattle <- seattleHomes[-29,]

# Part A
big.house <- data.frame(price=1500, squareFeet=2500, priceSqFt=1500000/2500, reciprocalSqFt=1/2500, lotSize=871000)
Seattle.big.house <- rbind(Seattle, big.house)

p <- ggplot(aes(x=squareFeet, y=priceSqFt), data=Seattle)
p + geom_point() + 
  geom_point(aes(x=squareFeet, y=priceSqFt), data=big.house, colour="red", size=5) +
  geom_text(data=big.house, aes(x=squareFeet, y=priceSqFt, label="Really expensive house"), vjust=0.3, hjust=-0.1, colour="red") +
  geom_smooth(aes(colour="Fitted (no outlier)"), method="lm", se=FALSE) + 
  geom_smooth(aes(colour="Smoothed (no outlier)"), se=FALSE) + 
  geom_smooth(data=Seattle.big.house, aes(colour="Fitted (outlier)"), method="lm", se=FALSE) +
  geom_smooth(data=Seattle.big.house, aes(colour="Smoothed (outlier)"), se=FALSE) + 
  labs(x="\nSquare feet", y="Price per square foot\n") +
  opts(title="Price per square foot as a function of square footage\n") +
  scale_colour_manual(name="Lines", values=c("Smoothed (no outlier)"="blue", "Fitted (no outlier)"="red", "Fitted (outlier)"="darkgoldenrod2", "Smoothed (outlier)"="darkgoldenrod4"))


# Part B
seattle.model <- lm(priceSqFt ~ squareFeet, data=Seattle)
seattle.model.big.house <- lm(priceSqFt ~ squareFeet, data=Seattle.big.house)
summary(seattle.model)
summary(seattle.model.big.house)

# Part D
top.three <- head(subset(Seattle.big.house[order(Seattle.big.house$lotSize, decreasing=TRUE),]), 3)
top.three[1,"lotSize"] / top.three[2,"lotSize"]