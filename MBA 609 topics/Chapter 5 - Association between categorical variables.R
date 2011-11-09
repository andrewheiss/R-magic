#---------------------------------------------------------------------------
# Association between Categorical Variables
#
# Chapter 5 of Robert Stine and Dean Foster, *Statistics for Business: Decision Making and Analysis* 
#
# Andrew Heiss
# MBA 609 - November 2011
#
# Special instructions:
#   * This contains all the R stuff necessary to do all the analysis the chapter talks about
#   * Run each line or each chunk of code individually
#
#--------------------------------------------------------------------------

#--------------------
# Data to work with
#--------------------

# Use any two categorical variables in a data frame...
# contingency_table <- xtabs(var1~var2)     # like this
#
# ...or build a contingency table on your own like so:
#
# Mailing list and sales
#       Join Decline
#   Yes   52      12
#   No   343    3720
#
# matrix(list of numbers, numer of rows, number of columns, byrow=TRUE/FALSE)
mailinglist <- matrix(c(52,12,343,3720), 2, 2, byrow=TRUE)

# Add row and column names
rownames(mailinglist) <- c('Yes', 'No')
colnames(mailinglist) <- c('Join', 'Decline')

# If byrow was false here, the table would look like this:
#       Join Decline
#   Yes   52    *343*
#   No   *12*    3720


#---------------
# Visual stuff
#---------------

# Segmented bar chart
barplot(mailinglist, main="Segmented Bar Chart")

# Moasic plot
mosaicplot(mailinglist, main="Mosaic Plot")


#--------------------------
# Contingency table stuff
#--------------------------

# Print counts
mailinglist                     # Counts

# Marginal distributions
margin.table(mailinglist, 1)    # Row totals
margin.table(mailinglist, 2)    # Column totals

# Cell proportions (conditional distributions)
prop.table(mailinglist)         # Cell percentages
prop.table(mailinglist, 1)      # Row percentages 
prop.table(mailinglist, 2)      # Column percentages

# Expected cell values: (Row * Column) / Total
as.array(margin.table(mailinglist,1)) %*% t(as.array(margin.table(mailinglist,2))) / margin.table(mailinglist)

# OR an easier way... the Chi-squared function calculates expected results in the background
# Other values you can get out of the function: statistic, paramter, p.value, method, data.name, observed, expected, residuals. Neato.
chisq.test(mailinglist)$expected


# Alternatively, get a big fancy contingency table with all the counts, percentages, and expected values with the gmodels package
library(gmodels)
CrossTable(mailinglist, expected=TRUE)
# or if you're not using a matrix, CrossTable(var1, var2, ...)


#--------------------------------
# Strength of association stuff
#--------------------------------

# At the end of the CrossTable() output, R gives two different Chi-squared results, Pearson's and Pearson's with Yate's continuity correction (which only applies to 2x2 tables). Here's how you get those numbers with R only:
# Pearson's test alone
chisq.test(mailinglist, correct=FALSE)

# Pearson's test with Yates' correction (whatever that means)
chisq.test(mailinglist)


# You need to load the vcd library to find Cramer's V
library(vcd)
assocstats(mailinglist)

# Or if you just want Cramer's V instead of the whole table...
assocstats(mailinglist)$cramer


#--------------------
# Nerdy extra stuff
#--------------------

# You can use this formula to calculate Cramer's V without loading the VCD library

cramers_v <- function(x){
    CV <- sqrt(chisq.test(x, correct = FALSE)$statistic /
              (sum(x) * min(dim(x) - 1)))

    as.numeric(CV)
}

cramers_v(mailinglist)



# How to determine the threshold Chi-squared value (if you don't want to look at the automatic output...).
# qchisq() basically eliminates the need for those look-up tables
# degree of freedom = number of rows-number of columns
qchisq(0.95, df=1)      # 95% confidence level with 1 degree of freedom
qchisq(0.99, df=7)      # 99% confidence level with 7 degrees of freedom


# Or, if you want to see a whole table of threshold values for different degrees of freedom and confidence levels, run this nifty script:
# Choose how many degrees of freedom and confidence levels you want to show
df <- 1:15                          # 15 degrees of freedom, from 1-15
ci <- c(0.90, 0.95, 0.99, 0.999)    # 4 confidence levels

# Start building chi-squared table with rows for degrees of freedom
chitable <- matrix(df)

# Loop through all the confidence levels and build a table
for (i in 1:length(ci)) {
    # cbind() on an existing table or matrix adds a column, so add a new column for each confidence level
    chitable <- cbind(chitable, qchisq(ci[i], df))
}

# Add column names by appending the ci vector after the first column name
colnames(chitable) <- (append(c("DF"), ci))

# Show the final table
chitable