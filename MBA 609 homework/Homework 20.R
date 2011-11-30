load("/Users/andrew/Documents/BYU 2011-2012/Fall 2011/MBA 609/R magic/MBA 609 datasets/stineFoster19.RData")

# y <- seattleHomes$price
# x <- seattleHomes$squareFeet
# xlabel <- "Square feet"
# ylabel <- "Price (thousands of dollars)"
# title <- "Price as a function of square feet"

x <- cars$horsepower
y <- cars$basePrice
xlabel <- "Horespower"
ylabel <- "Price ($)"
title <- "Price as a function of horespower"



model <- lm(y~x)

plot(x, y, pch=20, xlab=xlabel, ylab=ylabel, main=title)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="#e5e5e5")
abline(h=axTicks(2), v=axTicks(1), lty=1, col="#ffffff", lwd=2)    # Add thick lines on major tick marks

abline(model, col="red", lwd=2)
lines(loess.smooth(x,y), col="blue", lwd=2)

par(new=TRUE)
plot(x, y, pch=20, xlab="", ylab="", axes=FALSE)

summary(model)
# 2/3rds of data lie within standard error of the regression statistic

# 
# # residuals(model)
# plot(residuals(model), main="Residuals", pch=20) # Or plot(model$residuals)
# abline(0, 0) # horizon line

# hist(residuals(model), main="Histogram of residuals")

# model.stdres = rstandard(model)
# qqnorm(model.stdres)
# qqline(model.stdres)

# layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
# plot(model)

predict(lm(y ~ x), data.frame(x = c(250)), se = TRUE)
# predict(lm(y ~ x), data.frame(x = c(2690)), se = TRUE)