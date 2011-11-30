grey.plot <- 
function(x, y, xlab="x", ylab="y", main="Title", pch=20, type="p") {
  plot(x, y, pch=pch, xlab=xlab, ylab=ylab, main=main, type=type)
  # Add grey background with gridlines
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="#e5e5e5")
  abline(h=axTicks(2), v=axTicks(1), lty=1, col="#ffffff", lwd=2)    # Add thick lines on major tick marks
  par(new=TRUE)
  # Put the data points back on the plot without labels and axes 
  plot(x, y, pch=pch, axes=FALSE, xlab="", ylab="", type=type)
}

visual.test <-
function(x, y) {
  #------------------------------
  # Visual test for association
  #------------------------------
  # Save current plot settings to revert back to later
  oldpar <- par(no.readonly = TRUE)

  # Set all margins to 0
  par(mar=c(0, 0, 0, 0))

  # Build a randomized layout matrix
  m <- matrix(sample(1:4), 2, 2)
  layout(m, 1, 1)

  # Plot actual scatterplot somewhere in the layout grid
  plot(x, y, pch=20)
  abline(0, 0, col="red", lwd=1)

  # Plot three random scatterplots somewhere in the layout grid
  for (i in 1:3) {
    plot(x, sample(y), pch=20)
    abline(0, 0, col="red", lwd=1)
  }

  # Revert to original plot settings
  par(oldpar) 
}

# C = cost of item; E = elasticity
# Optimal price = C(E/(E+1))
elasticity <-
function(cost, elasticity) {
  cost * (elasticity/(elasticity + 1))
}

cramers_v <- 
function(contingency_table) {
    CV <- sqrt(chisq.test(contingency_table, correct = FALSE)$statistic /
              (sum(contingency_table) * min(dim(contingency_table) - 1)))

    as.numeric(CV)
}