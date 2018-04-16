library(zoo) # For apply.rolling
library(e1071)

source("GetDataFunctions.R")
source("ExperimentalFunctions.R")

data_1 <- merge_sitl("mms_20151016.csv", "sitl_20151016.csv")

data_rolling <- na.omit(shiftData(data_1))

train_1 <- split_train(data_rolling)
test_1 <- split_test(data_rolling)


# I'm just going to try an SVM with the future-data and see how much better that does.

rsvm.model <- svm(Selected ~ . - Priority - Time, data = train_1, kernel = "radial", cross = 10)
rsvm.prediction <- predict(rsvm.model, test_1)
rsvm.classError <- sum(ifelse(rsvm.prediction == test_1$Selected, 0, 1)) / nrow(test_1)
print(paste("RSVM Classfication Error: ", rsvm.classError))
print(paste("RSVM Total Accuracy:", rsvm.model$tot.accuracy))

evaluate_data(test_1, rsvm.prediction)

# caret::confusionMatrix(rsvm.prediction, test_1$Selected)

deal <- function (z) c(z[-1], sd(z[3]) + sd(z[4]) + sd(z[5])
                       + z[6])

#which.max

#sorted <- windows[order(windows[1]),]

# Take top N, depending on data limit.
# Overlap problems?