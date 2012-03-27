variable1 <- rnorm(1000) * 1000
variable2 <- rnorm(1000) * 10

# Mark the 3rd value as missing, just for fun
variable1[3] <- NA

# Build a fake data frame
variables <- data.frame(var1=variable1, var2=variable2)

subset2 <- subset(EMC.2011.v2, select = -c(uselikelytext))

coolSummary <- function(x) {
  if (class(x) == "data.frame") {
    x.min <- apply(x, 2, min, na.rm=TRUE)
    x.max <- apply(x, 2, max, na.rm=TRUE)
    x.sd <- apply(x, 2, sd, na.rm=TRUE)
    x.mean <- as.numeric(lapply(x, mean, na.rm=TRUE))
    x.n <- apply(!is.na(x), 2, sum)
    x.na <- apply(is.na(x), 2, sum)
    row.names <- colnames(x)
  } else {
    x.min <- min(x, na.rm=TRUE)
    x.max <- max(x, na.rm=TRUE)
    x.sd <- sd(x, na.rm=TRUE)
    x.mean <- mean(x, na.rm=TRUE)
    x.n <- sum(!is.na(x))
    x.na <- sum(is.na(x))
    row.names <- deparse(substitute(x))
  }
  
  results <- data.frame(x.mean, x.sd, x.min, x.max, x.n, x.na)
  colnames(results) <- c("Mean", "Std Dev", "Min", "Max", "N", "NAs")
  rownames(results) <- row.names
  results
}

coolSummary(EMC.2011.v2)
coolSummary(EMC.2011.v2$emresident)

coolSummary(variable1)
coolSummary(variables)