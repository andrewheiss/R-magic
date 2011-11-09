# Set up data
gas_sales <- matrix(c(126, 62, 103, 28, 448, 115), 3, 2, byrow=TRUE)
rownames(gas_sales) <- c('Premium', 'Plus', 'Regular')
colnames(gas_sales) <- c('Weekday', 'Weekend')

# Fancy contingency table stuff
library(gmodels)

# Part (a): Find marginal totals
CrossTable(gas_sales, prop.r=T, prop.c=T, prop.t=F, prop.chisq=F)

# Or just get the numbers
margin.table(mailinglist, 1)    # Row totals
margin.table(mailinglist, 2)    # Column totals

# Part (b): Conditional distribution of purchase type for weekday purchases (technically both weekday and weekend)

prop.table(gas_sales, 2)[,"Weekday"]

# OR
# prop.table(gas_sales, 2)[,1]
# subset(prop.table(gas_sales, 2), select=1)
# CrossTable(gas_sales, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F)

# Part (c): Conditional distriubtion of premium purchases during the week and weekend

prop.table(gas_sales, 1)["Premium",]

# Other ways
# prop.table(gas_sales, 1)[1,]
# CrossTable(gas_sales, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F)

# Build fancy mosaic plot
library(vcd)
mosaic(gas_sales, shade=TRUE)




# Airline stuff (although it's better to do lots of this stuff in Excel)
airlines <- matrix(c(1536, 11796, 416, 3343), 2, 2, byrow=T)
colnames(airlines) <- c("American", "Delta")
rownames(airlines) <- c("On-time", "Delayed")

round(prop.table(airlines, 2), 3)*100
