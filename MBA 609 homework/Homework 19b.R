load("~/Documents/BYU 2011-2012/Fall 2011/MBA 609/R magic/MBA 609 datasets/stineFoster06.RData")

x <- costAccounting$millingOps
y <- costAccounting$cost
ylabel <- "Cost ($)"
xlabel <- "Milling operations"
title <- "Relationship of milling operations to total cost"

# Save current plot settings to revert back to later
first_par <- par(no.readonly = TRUE)

# Set the margins (bottom, left, top, right) and some font settings
par(mar=c(5, 5, 3, 0), family="Helvetica", font.lab=2, col.axis="#666666")

# This could all be done with layout, but I want to see how fig() works...
# fig() takes a vector in the form of (x1, x2, y1, y2), where the bottom left is (0,0) and the top right of the plot is (1,1)
# So this first plot goes from 0 to 0.9 on the x-axis and 0 to 0.85 on the y axis
par(fig=c(0,0.9,0,0.85))

# Plot the data
plot(x, y, xlab=xlabel, ylab=ylabel)

# Add minor tick marks
minor.tick()

# Add grey background with gridlines
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="#e5e5e5")
abline(h=axTicks(2), v=axTicks(1), lty=1, col="#ffffff", lwd=2)    # Add thick lines on major tick marks

# Determine minor lines (unfortunately minor.tick() doesn't return the marks it creates...)
minor_lines <- function(x) { c(x, max(x)+x)/2 }
abline(h=minor_lines(axTicks(2)), v=(minor_lines(axTicks(1))), lty=1, col="#ffffff", lwd=.5)    # Add thin lines on minor tick marks using the minor_lines() function

# par(new=TRUE) adds the next plot to the existing plot (kind of like using add=TRUE with histograms)
par(new=TRUE)

# Put the data points back on the plot without lables and axes 
plot(x, y, pch=20, axes=FALSE, xlab="", ylab="")
# legend("bottomright", inset=0.05, lty=c(1,1), lwd=c(2.5, 2.5), legend=c("Regression line", "Loess smooth line"), col=c("red", "blue"), cex=.7)

# Add a linear and a smooth curve
abline(lm(y~x), col="red", lwd=2)
lines(loess.smooth(x,y), col="blue", lwd=2)

# Add rugs just for fun
rug(x, col="#333333")
rug(y, side=2, col="#333333")

# 0 to 0.9 on the x axis; .55 to 1 on the y axis (to pull the boxplot closer to the scatterplot)
par(fig=c(0,0.9,0.5,1), new=TRUE)
boxplot(x, horizontal=TRUE, axes=FALSE, pch=20)

# 0.78 to 1 on the x axis (to pull the boxplot closer to the scatterplot); 0 to 0.85 on the y axis
par(fig=c(0.78,1,0,0.85), new=TRUE)
boxplot(y, axes=FALSE, pch=20)

# Add a fake title (since putting a title on the boxplots or scatterplot puts the text in the graphics)
mtext(title, side=3, outer=T, line=-3, cex=1.3, font=2)
# Revert to original plot settings
par(first_par)

scatterplotMatrix(~cost+laborHours+materialCost+millingOps, reg.line=lm, smooth=TRUE, spread=FALSE, span=0.5, diagonal = 'histogram', data=costAccounting, main="Scatterplot matrix")

cor(costAccounting[,c("cost","laborHours","materialCost","millingOps")], use="complete")
cor.test(costAccounting$cost, costAccounting$laborHours)
cor.test(costAccounting$cost, costAccounting$materialCost)
cor.test(costAccounting$cost, costAccounting$millingOps)

model <- lm(cost~laborHours+materialCost+millingOps, data=costAccounting)
summary(model)