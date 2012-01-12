#---------------------------------------------------------------------------
# PMGT 634 Homework 2
# January 2011
# Andrew Heiss
#---------------------------------------------------------------------------

# Load required libraries
library(quantmod)
library(ggplot2)
library(gdata)

#-------------
# Question 1
#-------------
# Get data from FRED as a timeSeries! No need to convert it later
CarRegistrations <- getSymbols("USASACRMISMEI", src = "FRED", auto.assign = FALSE, return.class = "timeSeries")

# Seperate all the time series components out
time <- as.Date(time(CarRegistrations))
months <- months(time(CarRegistrations))
registrations <- coredata(CarRegistrations)

# Save it to a dataframe
PlotStuff <- data.frame(time, months, registrations)

# Plot it
qplot(data=PlotStuff, x=time, y=USASACRMISMEI, geom="line", ylab="Car Registrations\n", xlab="", main="Car Registrations, 1960-2011\n")


#-------------
# Question 2
#-------------
# Read from an Excel file
CarData.excel <- read.xls("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/Homework/cars04.xlsx", header = TRUE)

# Read from a CSV file
# Convert data frame to CSV using gdata and read it back in as a CSV, just for the sake of the assignment :)
# ... also because I'm too lazy to open Excel and export the file
tempName <- "temp_car_data.csv"         # Create file name
write.csv(CarData.excel, file=tempName) # Save data frame as CSV
CarData.csv <- read.csv(tempName)       # Create new data frame from CSV
unlink(tempName); remove(tempName)      # Delete temporary CSV and file name variable

# Read from the clipboard
# Again, out of laziness, this puts the dataframe into the clipboard and then reads it back again. The same concept works with anything on the clipboard :)
write.table(file=pipe("pbcopy"), CarData.excel)
CarData.clipboard <- read.table(file=pipe("pbpaste"), header=T)

# Summarize the data
summary(CarData.excel)

# Remove missing value
CarData.fixed <- subset(CarData.excel, MPGCity > 0)

# Plot it
qplot(data=CarData.fixed, x=Horsepower, y=MPGCity, 
      main="Horespower as a function of miles per gallon (city)\n", 
      xlab="\nHorespower", ylab="Miles per gallon (city)\n", 
      geom = c("point", "smooth"), shape=Transmission)

# Fancier plot
ggplot(data=CarData.fixed, aes(x=Horsepower, y=MPGCity, shape=Transmission)) +
  geom_point(aes(colour = Transmission)) +
  scale_colour_manual(values = c("red","gold3")) + 
  scale_shape_manual(values=c(20,6)) +
	geom_smooth() + 
	geom_smooth(method = "lm", se = FALSE) +
	opts(title = "Horespower as a function of miles per gallon (city)\n") +
	labs(x = "\nHorsepower", y = "Miles Per Gallon (city)\n")