#---------------------------------------------------------------------------
# R stuff for MBA 609 Homework 19
# November 2011
# Andrew Heiss
#
# Special instructions:
#   * Update the path to the dataset
#
#--------------------------------------------------------------------------

load("~/Documents/BYU 2011-2012/Fall 2011/MBA 609/R magic/MBA 609 datasets/stineFoster06.RData")
library(car)

# # Save current plot settings to revert back to later
# first_par <- par(no.readonly = TRUE)
# par(mar=c(3,4,4,2))
# m <- matrix(c(1,2), 1, 2)
# layout(m, 1, 1)
# 
# plot(microsoftPrice~date, data=stocks, pch=20, type="o", xlab="", ylab="Price ($)")
# plot(microsoftReturn~date, data=stocks, pch=20, type="o", xlab="", ylab="Return (%)")
# mtext("Figure 1: Timeplots of Microsoft’s stock prices and monthly returns", side=3, outer=T, line=-3, cex=1.3, font=2)
# 
# # Revert to original plot settings
# par(first_par)



# variable <- stocks$ibmReturn
# title <- "IBM returns (1990-2005)"
# xlabel <- "Monthly return"
# ylabel <- "Frequency"
# color <- "#5D7359"
# 
# # Save current plot settings to revert back to later
# second_par <- par(no.readonly = TRUE)
# 
# m <- matrix(c(1,2), 2, 1)
# layout(m, 1, c(1,3))
# 
# par(mar=c(0, 6, 3, 2.5), family="Helvetica", font.lab=2, cex.main=1.4, col.axis="#666666")
# 
# boxplot <- boxplot(variable, horizontal=T, axes=F, main=title, pch=20)
# 
# # Set the margins for the bottom row and build the histogram
# par(mar=c(5, 4.5, 0, 1))
# 
# # Build colorless histogram
# hist(variable, xlab=xlabel, ylab=ylabel, main=NULL, breaks=seq(-0.5, 0.5, by=0.1))
# # hist(variable, xlab=xlabel, ylab=ylabel, main=NULL)
# # Add background color and lines
# rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#e5e5e5")
# abline(h=axTicks(2), lty=1, col="#ffffff")
# # Add colored histogram
# histogram <- hist(variable, add=TRUE, col=color)
# 
# # Revert to original plot settings
# par(second_par)



numSummary(stocks[,c("dellReturn", "ibmReturn", "microsoftReturn")], statistics=c("mean", "sd", "quantiles", "cv", "skewness"), quantiles=c(0,.25,.5,.75,1), type="2")

scatterplotMatrix(~dellReturn+ibmReturn+microsoftReturn, reg.line=lm, smooth=TRUE, spread=FALSE, span=0.5, diagonal = 'histogram', data=stocks, main="Scatterplot matrix (Dell, IBM, and Microsoft)", pch=20)
cor(stocks[,c("dellReturn","ibmReturn","microsoftReturn")], use="complete")

x <- stocks$date
y <- stocks$dellReturn

plot(x, y, pch=20, type="o", xlab="", ylab="Return (%)", main="Timeplot for Dell (1990-2005)")
# Add grey background with gridlines
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="#e5e5e5")
abline(h=axTicks(2), v=axTicks(1), lty=1, col="#ffffff", lwd=2)    # Add thick lines on major tick marks
par(new=TRUE)
# Put the data points back on the plot without lables and axes 
plot(x, y, pch=20, axes=FALSE, xlab="", ylab="", type="o")
