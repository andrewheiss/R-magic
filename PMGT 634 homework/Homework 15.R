#---------------------------------------------------------------------------
# PMGT 634 Homework 15
# March 2011
# Andrew Heiss
#---------------------------------------------------------------------------

# Load required libraries
library(car)
library(ggplot2)

# Load data
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster24.RData")

#--------
# 24.38
#--------

Cars <- cars
cars.model <- lm(log10(basePrice) ~ log10(horsepower) + log10(weight), data=Cars)
summary(cars.model)

# Part A
scatterplotMatrix(~ log10(basePrice) + log10(horsepower) + log10(weight), data=Cars)
spm(~ basePrice + horsepower + weight, data=Cars)

with(Cars, cor(cbind(basePrice, horsepower, weight)))
with(Cars, cor(cbind(log10(basePrice), log10(horsepower), log10(weight))))


# Part C
summary(cars.model)

price.horsepower <- lm(log10(basePrice) ~ log10(horsepower), data=Cars)
price.weight <- lm(log10(basePrice) ~ log10(weight), data=Cars)

summary(price.horsepower)
summary(price.weight)

compareCoefs(cars.model, price.horsepower, price.weight)

# Part D
leveragePlots(cars.model, pch=20)

# Part D
marginalModelPlots(cars.model, pch=20)


#--------
# 24.42
#--------

Apple <- apple
apple.model <- lm(appleExcessReturn ~ marketExcessReturn + SP500ExcessReturn + IBMExcessReturn, data=Apple)
summary(apple.model)

# Part A
spm(~ date + appleExcessReturn + marketExcessReturn + SP500ExcessReturn + IBMExcessReturn, data=Apple, pch=20)

plotData <- melt(Apple, id.vars = c("date"),
                 measure.vars = c("appleExcessReturn", 'marketExcessReturn', 'IBMExcessReturn', 'SP500ExcessReturn'))
names(plotData) <- c("date", "companyIndex", "excessReturn")
ggplot(data = plotData, aes(x = date, y = excessReturn, colour = companyIndex)) +
  geom_hline(yintercept = 0, colour = 'grey65') +
  geom_path() +
  facet_grid(companyIndex ~ .) +
  opts(legend.position = "none", title="Comparison of excess returns over time\n") + 
  labs(x = "\nDate", y = "Excess returns\n")

# Part B
fancy.summary(apple.model)
summary(apple.model)


# Part C
leveragePlots(apple.model, pch=20)
marginalModelPlots(apple.model, pch=20)

# Part D
vif.summary(apple.model)

# Part E
summary(apple.model)