library(car)
#--------------
# Enclosing a command in parentheses causes the result to be printed:
#--------------
(x <- c(1, 2, 3, 4))
# [1] 1 2 3 4


#--------------
# scan() lets you input space delimited text
#--------------
(cooperation <- scan())
# 1: 32 24 34 12 09 234 
# 7: 123 543
# 9: 
# Read 8 items
# [1]  32  24  34  12   9 234 123 543


#--------------
# Use with() to attach data. It's magic
#--------------
with(Duncan, lm(prestige ~ income + education))
# 
# Call:
# lm(formula = prestige ~ income + education)
# 
# Coefficients:
# (Intercept)       income    education  
#     -6.0647       0.5987       0.5458
#
# or
with(Freedman, {
  plot(density, crime)
  identify(density, crime, row.names(Freedman))
})


#--------------
# Dealing with missing values
#--------------
# Use complete.cases()
fixed <- with(Freedman, complete.cases(crime, density))
head(fixed, 20)
#  [1]  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE
# [19]  TRUE  TRUE
with(Freedman, lines(lowess(log(density[fixed], base=10), crime[fixed], f=1.0)))

# Or use na.omit()
Freedman.good <- na.omit(Freedman)
head(Freedman.good)
dim(Freedman.good)

# Count the number of missing values
sum(is.na(Freedman))

#--------------
# Modifying and transforming data
#--------------
# transform() lets you modify individual variables in a data frame
# For example, 
# Data <- transform(Data, c=-c, asq=a^2, a.over.b=a/b)
# ... replaces the Data data frame leaving a and b unmodified, negates c, and adds two new variables, asq and a.over.b

# cut() divides numeric values into bins
Guyer$perc.coop <- 100*Guyer$cooperation/120
Guyer$coop.4 <- cut(Guyer$perc.coop, 4)
summary(Guyer$coop.4)
# (22.5,33.3] (33.3,44.2]   (44.2,55]   (55,65.9] 
#           6           7           5           2 
# Divides the values into four equal width bins

# Divide the values into low, medium, and high
Guyer$coop.groups <- with(Guyer, cut(perc.coop, 
  quantile(perc.coop, c(0, 1/3, 2/3, 1)),
  include.lowest=TRUE,
  labels=c("Low", "Medium", "High")))
summary(Guyer$coop.groups)
#    Low Medium   High 
#      7      6      7 
# 
# quantile() locates the cut points
# Four groups, just for fun
# Divide the values into low, medium, and high
Guyer$coop.groups4 <- with(Guyer, cut(perc.coop, 
  quantile(perc.coop, c(0, 0.25, 0.5, 0.75, 1)),
  include.lowest=TRUE,
  labels=c("Group 1", "Group 2", "Group 3", "Group 4")))
summary(Guyer$coop.groups4)


# recode() in car is better though
(Guyer$coop.recode <- recode(Guyer$perc.coop, "lo:50=1; 50:hi=2"))
