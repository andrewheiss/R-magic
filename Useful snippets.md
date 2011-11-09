Get World Bank Data!

	library(WDI)
	DF <- WDI(country=c("US","CA","MX"), indicator="NY.GDP.MKTP.KD.ZG", start=1990, end=2008)

Converting to z-scores:

	# Converting a whole column or vector
	scale(variable)
	
	# Converting an arbitrary number
	x <- 5
	mean <- mean(variable, na.rm=TRUE)
	sd <- sd(variable, na.rm=TRUE)
	(x-mean)/sd

Reading from or writing to the clipboard on OS X:

	read.table(pipe("pbpaste"), header = T, ...)
	write.table(pipe("pbcopy"), ...)
	# OR
	read.delim(pipe("pbpaste"))

Reading from the clipboard on Windows:

	read.delim("clipboard")

Add text to a plot with `text()` or `mtext()`

Recoding data: (see also `recode()`)

	# create 2 age categories 
	mydata$agecat <- ifelse(mydata$age > 70, 
	c("older"), c("younger")) 
	
	# another example: create 3 age categories 
	attach(mydata)
	mydata$agecat[age > 75] <- "Elder"
	mydata$agecat[age > 45 & age <= 75] <- "Middle Aged"
	mydata$agecat[age <= 45] <- "Young"
	detach(mydata)
	
	# recode 99 to missing for variable v1
	# select rows where v1 is 99 and recode column v1 
	mydata[mydata$v1==99,"v1"] <- NA
	
	# This works too, and is probably easier
	variable[variable > 50] <- NA

Best way to recode: (in the car library)

	cars$MPGHighwayFixed <- recode(cars$MPGHighway, "-99=NA")
	# or for the whole data frame:
	cars <- recode(cars, "-99=NA")

Use the gdata library to do `read.xls(file, sheet=x)`

Adding a horizontal box plot to a histogram (kind of)

	boxplot(test, horizontal=TRUE, at=4, add=TRUE)

Clear all variables and stuff: `rm(list=ls())`

Clear plots and stuff (also stop using quartz() or windows()): `dev.off()` or `graphics.off()`

Make quartz() or windows() universal:

	if(.Platform$OS.type=="windows") {
		quartz<-function() windows()
	}
