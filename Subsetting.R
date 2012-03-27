#--------------------------
# Useful stuff for Quiz 1
#--------------------------

#-----------------------------------
# Custom function to find the mode
mode.find <- function(x) {
  names(sort(-table(x)))[1]
}
#-----------------------------------

# Load the dta file
library(foreign)
Quiz <- read.dta(file.choose())

# In this case, R will import everything as continuous numbers. You can verify that by looking at the data frame's structure:
str(Quiz)

# See how all the variables are either integers (int) or numeric (num)? You need to force some of the columns to be categorical, or factors
Quiz <- transform(Quiz, 
                  male=as.factor(male), 
                  marital=as.factor(marital), 
                  income=as.factor(income), 
                  race=as.factor(race),
                  money=as.factor(money),
                  prayer=as.factor(prayer)
                  )

# Voila. Check the structure now...
str(Quiz)
# Now there are a bunch of factors with different numbers of levels (2 genders, yes/no for money and prayer, 11 levels of income, etc)

# The describe.by() function is in the psych package and lets you group summary statistics for individual groups
# This shows the summary stats for each of the genders
library(psych)
describe.by(Quiz$moneyamt, Quiz$male)

# You can also subset columns and not use the psych package. The subset() function creates a new data fram based on the conditions you tell it
# So, if you want to see the mean of women who prayed more than 5 times, you would create a new data frame like so
WomenWhoDonatedFiveTimes <- subset(Quiz, Quiz$male == 0 & Quiz$prayeramt > 5)

# Look at the new data frame 
View(WomenWhoDonatedFiveTimes)

# Notice how it only has 149 rows instead of 897, and that they're all female and prayed for more than 5 minutes. Yay!
# Now you can get the summary statistics
mean(WomenWhoDonatedFiveTimes$moneyamt)
# or
summary(WomenWhoDonatedFiveTimes$moneyamt)
boxplot(WomenWhoDonatedFiveTimes$moneyamt, horizontal=TRUE)
mode.find(WomenWhoDonatedFiveTimes$income)
# and so on

# You also need to be able to remove outliers, which is really easy
# So, for example, you might want to get rid of the people who prayed more than 500 times a day (because that's nuts)
# Before:
boxplot(Quiz$prayeramt)

# Make a new variable so you're not messing with the original data
Quiz$prayeramtFixed <- Quiz$prayeramt

# Mark anything that's greater than 500 as missing
Quiz$prayeramtFixed[Quiz$prayeramtFixed > 500] <- NA

# Check it now
boxplot(Quiz$prayeramtFixed)
# It's fixed!

# You would do the same thing for age. Remove anything that's greater than 100
hist(Quiz$age)  # Messed up
Quiz$ageFixed <- Quiz$age
Quiz$ageFixed[Quiz$ageFixed > 100] <- NA
hist(Quiz$ageFixed)  # All better