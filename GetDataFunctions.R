# GetDataFunctions:
#   Created for the Vega group to import, clean and transform data for the
#   MMS CS 780 class project.
# Authors: Daroc Alden, Jeremy Walker, & Samantha Piatt


# ----------------------------------------------------------- Open and Save Data
# Create data frame from mms and sitl data.
#   This is the original function created to parse and merge the feature and
#   response data. This is the second version, which incorporates the
#   convert.time function from the evaluation document.
#
# Returns a data frame containing merged feature set with sitl response columns.
merge_sitl <- function(ina, inb) {
  outdata <- na.omit(read.csv(ina))
  sitl <- read.csv(inb)

  # omit first X column
  outdata <- outdata[,-1]

  # Rectifying date/times
  min.time <- min(lubridate::as_datetime(outdata$Time))
  convert.time <- function(x)
    as.integer(1000 * difftime(x, min.time, units = "secs"))

  sitl$Start <- convert.time(stringr::str_replace_all(sitl$Start, "([T])", " "))
  sitl$End <- convert.time(stringr::str_replace_all(sitl$End, "([T])", " "))
  outdata$Time <- convert.time(outdata$Time)
  outdata[,"Priority"] <- 0
  outdata[,"Selected"] <- 0

  # populate Y in outdata
  for(i in 1:nrow(sitl)){
    for(x in 1:nrow(outdata)){
      if(outdata$Time[x] >= sitl$Start[i] & outdata$Time[x] <= sitl$End[i]){
        outdata$Priority[x] <- sitl$Priority[i]
        outdata$Selected[x] <- 1
      }
    }
  }

  return(outdata)
}

# Create data frame from mms and sitl data.
#   DOES NOT WORK. This is from the evaluation document.
#   Keeps throwing a lot of errors.
#
# Returns a data frame containing merged feature set with sitl response columns.
evaluation_merge <- function(mms_filename, sitl_filename){
  # make sure that X1 is a correct row index (it is 0-based originally)
  mms <- na.omit(read.csv(mms_filename)) %>% dplyr::mutate(X1 = X + 1)
  sitl <- na.omit(read.csv(sitl_filename))

  # convert time to milliseconds ... interval_inner_join works also with
  # date-time directly but then it ingores milliseconds. This makes a small
  # difference in this example
  min.time <- min(mms$Time)
  convert.time <- function(x) as.integer(1000*difftime(x, min.time, units="secs"))

  mms.id <- mms %>% dplyr::transmute(Start = convert.time(Time), End = Start, X1 = X1)
  sitl.id <- sitl %>% select(c(Start,End,Priority)) %>%
    dplyr::mutate(Start = convert.time(Start), End = convert.time(End))

  joined <- interval_inner_join(sitl.id, mms.id, c("Start","End"))
    %>% group_by(X1) %>%

  mms.target <- mms
  mms.target$Selected <- F
  mms.target$Selected[joined$X1] <- T
  mms.target$Selected = as.factor(mms.target$Selected)
  mms.target$Priority <- 0
  mms.target$Priority[joined$X1] <- joined$Priority
  return(mms.target)
}

# Save data.
#   DOES NOT YET WORK. This is from the evaluation document.
#   Needs better input, etc.
save_for_eval <- function(mitl_filename){
  output <- data_frame(ObservationId=mms$X1,Selected=c(probabilities))
  write_csv(output, mitl_filename)
}

# ------------------------------------------------------------------- Split Data
# Calculate indexes for training subset.
#
# Returns a vector of row numbers to include in the training subset.
select_train <- function(indata) {
  select = floor(0.75 * nrow(indata))
  set.seed(0)
  return(sample(seq_len(nrow(indata)), size = select))
}

# Split data set into training data.
#
# Returns a data frame containing the training subset of indata.
split_train <- function(indata){
  points = select_train(indata)
  train <- indata[points, ]
  return(train)
}

# Split data set into test data.
#
# Returns a data frame containing the test subset of indata.
split_test <- function(indata){
  points = select_train(indata)
  test <- indata[-points, ]
  return(test)
}

# -------------------------------------------------------------------- Factorize
# Creates True/False factors for Binary or Priority data.
#   Priority levels: High >= 75%, Medium >= 25%, Low < %10.
#
# Returns a data frame with High, Medium, and Low Priority columns
#   containing true/false factors if Binary = FALSE,
#   or a vector of true/false factors if Binary = TRUE.
factorize <- function(column, binary = FALSE){
  if(binary){
    newc <- as.factor(ifelse(column == 1, TRUE, FALSE))
    return(newc)
  } else {
    percentage <- function(x) (x - min(column)) / (max(column) - min(column))
    ps <- percentage(column)
    high <- as.factor(ifelse(ps >= 0.75, TRUE, FALSE))
    medium <- as.factor(ifelse(ps >= 0.25, TRUE, FALSE))
    low <- as.factor(ifelse(ps >= 0.1, TRUE, FALSE))
    return(data.frame(High_Priority = high,
                      Medium_Priority = medium,
                      Low_Priority = low))
  }
}

# -------------------------------------------------------------- Evaluate Models
# Missclassification error.
#   prediction and true value must be of the same data type;
#   IE: (numeric, numeric) or (factor, factor)
#
# Returns a numeric value that corresponds to the classification error rate.
class_error <- function(pred, tvalue) mean(pred != tvalue)
