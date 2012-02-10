##-------------------------------------------------------##
## PMGT 634 Lecture 11
## Stine and Foster Chapter 20 Transformations
## Fox and Weisberg Chapter 3 Transformations
## Ray Nelson February 8, 2012
##-------------------------------------------------------##
## load Libraries and data
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster20.RData")
library(car)
library(ggplot2)
library(timeSeries)

## Passenger cars example from Stine and Foster
# linear relationship ??
scatterplot(mpgCity ~ weight, data = cars)
cars.lm <- lm(mpgCity ~ weight, data = cars)
summary(cars.lm)
scatterplot(residuals(cars.lm) ~ cars$weight)
residualPlot(cars.lm)

# Reciprocal transformation
scatterplot(100/mpgCity ~ weight, data = cars)
cars.lm <- lm(100/mpgCity ~ weight, data = cars)
scatterplot(residuals(cars.lm) ~ cars$weight)
residualPlot(cars.lm)
rm(cars.lm)

## Heating Cost
## linear relationship
scatterplot(cost ~ avgTemperature, data = costs)
costs.lm <- lm(cost ~ avgTemperature, data = costs)
summary(costs.lm)
scatterplot(residuals(costs.lm) ~ costs$avgTemperature)
residualPlot(costs.lm)
rm(costs.lm)

## Pet Food
# linear relationship ??
par(mfrow = (c(2,1)))
plot(salesVolume ~ week, data = petFood, type = "l")
plot(price ~ week, data = petFood, type = "l")
par(mfrow = (c(1,1)))

scatterplot(salesVolume ~ price, data = petFood)
petFood.lm <- lm(salesVolume ~ price, data = petFood)
summary(petFood.lm)
scatterplot(residuals(petFood.lm) ~ petFood$price)
residualPlot(petFood.lm)

# Logarithmic transformation
scatterplot(salesVolume ~ price, data = petFood, log = "xy")
petFood.lm <- lm(log(salesVolume) ~ log(price), data = petFood)
summary(petFood.lm)
scatterplot(residuals(petFood.lm) ~ log(petFood$price))
residualPlot(petFood.lm)
rm(petFood.lm)

## Orange juice optimal pricing
# linear relationship ??
scatterplot(sales ~ price, data = juice)
juice.lm <- lm(sales ~ price, data = juice)
summary(juice.lm)
scatterplot(residuals(juice.lm) ~ juice$price)
residualPlot(juice.lm)

# Logarithmic transformation
scatterplot(sales ~ price, data = juice, log = "xy")
juice.lm <- lm(log(sales) ~ log(price), data = juice)
summary(juice.lm)
scatterplot(residuals(juice.lm) ~ log(juice$price))
residualPlot(juice.lm)
rm(juice.lm)

## CAR scripts from Chapter 3.4

## Ornstein data set)
# Density comparison
par(mfrow=c(1, 2))
with(Ornstein, plot(density(assets), xlab="assets", main="(a)"))
with(Ornstein, plot(density(log10(assets)),
                    xlab="base-10 log of assets", main="(b)"))
par(mfrow=c(1,1))
# Skewness and kurtosis comparisons of raw and transformed
Raw <- c(skewness(Ornstein$assets), kurtosis(Ornstein$assets))
Transformed <- c(skewness(log10(Ornstein$assets)),
                 kurtosis(log(Ornstein$assets)))
summaryStats <- data.frame(Raw, Transformed)
rownames(summaryStats) <- c("Skewness", "Coefficient of Excess")
summaryStats
rm(Raw, Transformed, summaryStats)

# Infant mortality
scatterplot(infant.mortality ~ gdp, data=UN, xlab="GDP per Capita",
            ylab="Infant Mortality Rate (per 1000 births)", main="(a)",
            boxplot=FALSE)
scatterplot(infant.mortality ~ gdp, data=UN, xlab="GDP per capita",
            ylab="Infant Mortality Rate (per 1000 births)", main="(b)",
            log="xy", boxplots=FALSE, id.n=4)
lm(log(infant.mortality) ~ log(gdp), data=UN)

# Special power transformation functions
bcPower(1:5, 0.5)
yjPower(-5:5, 0.5)
symbox(~ gdp, data=UN)

# Transformations to equalize spread
spreadLevelPlot(interlocks + 1 ~ nation, Ornstein)
oldmar <- par(mar=c(5.1, 4.1, 4.1, 4.1))
Boxplot(log10(interlocks + 1) ~ nation, data=Ornstein)
basicPowerAxis(power=0, base=10, at=c(1, 3, 6, 11, 21, 51, 101),
               start=1, axis.title="Interlocks")
par(oldmar)

# Transformations towards linearity
par(mfrow=c(1, 2))
invTranPlot(prestige ~ income, data=Prestige, lwd=2,
            xlab="income", main="(a)", col.lines=gray((0:3)/6))
plot(prestige ~ I(income^(1/3)), data=Prestige,
     xlab=expression(income^{1/3}), main="(b)")
abline(lm(prestige ~ I(income^(1/3)), data=Prestige))
summary(powerTransform(UN))
par(mfrow=c(1, 1))