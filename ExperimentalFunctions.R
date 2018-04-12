# ExperimentalFunctions:
#   Created for the Vega group to evalutate and experiment with data for the
#   MMS CS 780 class project.
# Authors: Daroc Alden, Jeremy Walker, & Samantha Piatt

# ------------------------------------------------------------------ Time Series
# Creates four additional columns with data offset by one for each column.
#   Theoretically, this should create a connection between the past two,
#   current, and future two data points.
#
# Returns a data frame containing 5p columns, where p = % input columns.
series <- function(x) {
  scols <- function(x, pre) {
    n <- length(x)[1]
    tmp <- data.frame(x[7:(n-0)], x[6:(n-1)], x[5:(n-2)],
                      x[4:(n-3)], x[3:(n-4)], x[2:(n-5)], x[1:(n-6)])
    colnames(tmp) <- c(paste(pre, "1"), paste(pre, "2"), paste(pre, "3"),
                       paste(pre, "4"), paste(pre, "5"), paste(pre, "6"),
                      paste(pre, "7"))
    return(tmp)
  }
  names <- colnames(x)
  out <- data.frame(scols(x[, 1], names[1]))
  for(i in length(x) -1){
    tmp <- scols(x[, i + 1], names[i + 1])
    out <- data.frame(out, tmp)
  }
  return(out)
}
