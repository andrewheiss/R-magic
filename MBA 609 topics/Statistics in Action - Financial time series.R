# Statistics in action

load("~/Documents/BYU 2011-2012/Fall 2011/MBA 609/R magic/MBA 609 datasets/statsInAction1.RData")

# Basic timeplot
plot(enron$date, enron$price, xlab="Year", ylab="Price ($)", xlim=c(1985, 2005), ylim=c(0, 100), pch=20, main="Timeplot of the price of Enron stock", type="o", col="blue")

# Histogram and time plot of the percentage changes
plot(enron$date, enron$percentageChange, xlab="Year", ylab="Percentage change", xlim=c(1985, 2005), ylim=c(-30, 30), type="p", pch=20)
hist(enron$percentageChange, xlim=c(-30, 30), breaks=60, xlab="Percentage change")

#-------------

# Basic histogram... kind of a lame plot
hist(ceo_comp$totalCompensation, breaks=40)

# Transform stuff with a log
fixed <- log(ceo_comp$totalCompensation)
hist(fixed, breaks=40)