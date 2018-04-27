# GetDataFunctions:
#   Created for the Vega group to import, clean and transform data for the
#   MMS CS 780 class project.
# Authors: Daroc Alden, Jeremy Walker, & Samantha Piatt

# --------------------------------------------------------------------- Packages
# Checks if the package (or packages when a vector of names is passed) is 
#   installed. If it is not, then it will be installed and loaded.
packnload <- function(name){
  if(!(name %in% rownames(installed.packages())) )
    install.packages(name)

  lapply(name, require, character.only = TRUE)
}

packnload("dplyr")

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

  # Make index proper (might start at 0) but don't refer to index by name 
  # since the eval says the index column is X1 and I've seen the index column 
  # name as X. Regardless of the name, it should be the first column in dataset.
  outdata[,1] <- outdata[,1] + 1

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

  return(outdata[order(outdata$Time),])
}

# Saves prediction values for evaluation.
#
# Outputs a file containing the index and prediction value.
save_predictions <- function(pred, ID, file){
  output <- data.frame(ObservationId = ID, Selected = c(pred))
  write.csv(output, file)
}

# -----------------------------------------------------------------Evaluate Data
# Open a saved Evaluation file
#   File should be saved using below save_predictions function (or equivalent).
#   Input mms.target should be a data frame containing an 'X' column ofindex 
#   values and a 'Selected' column of sitl values.
#
# Outputs performance data.
evaluate_file <- function(data, file) {
  predictions <- read.csv(file)
  print_evaluation(evaluation(data, predictions))
}

# This cirucumvents saving to a file, and can be used with test data.
#
# Outputs performance data.
evaluate_data <- function(data, prediction){
  predictions <- data.frame(ObservationId = data$X, Selected = c(prediction))
  print_evaluation(evaluation(data, predictions))
}

# Does the same thing as evaluate_data but returns a data.frame instead of 
#   printing it all out.
#
# Returns a data frame with evaluation metrics.
evaluate_data_asFrame <- function(data, prediction){
  predictions <- data.frame(ObservationId = data$X, Selected = c(prediction))
  return(evaluation(data, predictions))
}

# Evaluates target and predictions. To be used with evaluate_file or evaluate_data
#
# Outputs performance data.
evaluation <- function(mms.target, predictions){
  #sort them by prediction weight
  predictions <- predictions[order(predictions$Selected, decreasing = TRUE),]
  
  # use MITL to select data points
  true.predictions.count <- sum(mms.target$Selected == T)
  predictions$MITL_Selected <- F
  predictions$MITL_Selected[1:true.predictions.count] <- T
  
  # select the predictions
  predictions_comparison <- inner_join(predictions %>% select(-Selected),
                                       mms.target, by=c("ObservationId" = "X"))
  
  # computed metrics
  found <- predictions_comparison %>% filter(Selected == T & MITL_Selected == T)
  missed <- predictions_comparison %>% filter(Selected == T & MITL_Selected == F)
  
  return(data.frame("Total SITL" = true.predictions.count, 
    "Found SITL" = nrow(found), "Missed SITL" = nrow(missed), 
    "Class Error" = with(predictions_comparison,{mean(MITL_Selected != Selected)}),
    "ERROR" = sum(missed$Priority^2) / true.predictions.count))
}

# Prints the evaluation data
print_evaluation <- function(eval){
  cat("\nTotal SITL points: ", eval$Total.SITL, "\n")
  cat("Found SITL points: ", eval$Found.SITL, "\n")
  cat("Missed SITL points: ", eval$Missed.SITL, "\n")
  cat("Classification error: ", eval$Class.Error, "\n")
  cat("ERROR: ", eval$ERROR, "\n")
}

# ------------------------------------------------------------------- Split Data
# Calculate indexes for training subset.
#
# Returns a vector of row numbers to include in the training subset.
select_train <- function(indata, size, seed) {
  select = floor(size * nrow(indata))
  set.seed(seed)
  return(sample(seq_len(nrow(indata)), size = select))
}

# Split data set into training data.
#
# Returns a data frame containing the training subset of indata.
split_train <- function(indata, size = 0.75, seed = 0){
  points = select_train(indata, size, seed)
  train <- indata[points, ]
  return(train)
}

# Split data set into test data.
#
# Returns a data frame containing the test subset of indata.
split_test <- function(indata, size = 0.75, seed = 0){
  points = select_train(indata, size, seed)
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
