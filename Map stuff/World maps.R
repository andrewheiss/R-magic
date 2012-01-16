library(maps)
library(mapdata)

# Get the names of all the countries in the database
names <- map("worldHires", plot=FALSE, namesonly=TRUE)
names <- map("world2", plot=FALSE, namesonly=TRUE)

middleEast <- c("Egypt", "Libya", "Tunisia", "Israel", "Jordan", "Algeria", "Morocco", "Syria", "Lebanon", "Iraq", "Saudi Arabia", "Yemen", "Kuwait", "Oman", "Bahrain", "Qatar", "United Arab Emirates", "West Bank", "Gaza Strip")
map("worldHires", middleEast)
title("The Middle East")

map("worldHires", c("West Bank", "Gaza Strip", "Israel"))
