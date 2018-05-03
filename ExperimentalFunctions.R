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
shiftData <- function (data) {
  shift <- function(x, n){
    c(tail(x, -n), rep(NA, n))
  }
  scols <- function(x, pre) {
    y <- as.matrix(x)
    tmp <- data.frame(shift(y, 1), shift(y, 2), shift(y, 3), shift(y, 4), shift(y, 5))
    colnames(tmp) <- c(paste(pre, "1"), paste(pre, "2"), paste(pre, "3"),
                    paste(pre, "4"), paste(pre, "5"))
    return(tmp)
  }
  names <- colnames(data)
  for (i in 1:length(data)) {
    data <- data.frame(data, scols(data[i], names[i]))
  }
  
  return(data)
}

# Creates a column containg the average difference between this data point and 
#   the past and future n datapoints, given by the width parameter.
#
# Returns a data.frame containing each column of the original data frame, plus 
#   one column for each with the suffix ".Past"
series_metric <- function(data, width) {
  shift_metric <- function(column, width, prefix){
    n <- length(column)[1]
    tmp <- data.frame(past = 0, current = column, future = 0)
    for(i in (width+1):(n-width)){
      future = c()
      past = c()
      for(j in 1:width){
        if((i - j) >= 1) past[j] <- column[i] - column[i-j]
        if((i + j) <= n) future[j] <- column[i] - column[i+j]
      }
      tmp$past[i] <- mean(past)
      tmp$future[i] <- mean(future)
    }
    colnames(tmp) <- c(paste(prefix, "Past"), prefix, paste(prefix, "Future"))
    return(tmp)
  }
  
  names <- colnames(data)
  out <- shift_metric(data[, 1], width, names[1])
  for(i in 1:(length(data)-1)){
    tmp <- shift_metric(data[, i + 1], width, names[i + 1])
    out <- data.frame(out, tmp)
  }
  return(out)
}