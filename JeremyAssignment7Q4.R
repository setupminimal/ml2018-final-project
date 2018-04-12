
```{r include = FALSE}
#get data
knitr::opts_chunk$set(echo = FALSE)
source("GetDataFunctions.R")

data_1 <- merge_sitl("mms_20151016.csv", "sitl_20151016.csv")
train_1 <- split_train(data_1)
test_1 <- split_test(data_1)
```

```{r echo=FALSE, cache=TRUE}
myvars <- c("FGM_Magnetic_Field_w", "FGM_Magnetic_Field_x", "FGM_Magnetic_Field_y", "FGM_Magnetic_Field_z", "Selected")
train_1 <- train_1[, myvars]
test_1 <- test_1[, myvars] 

linear.model <- lm(Selected ~ . , data = train_1)
linear.predictions <- predict(linear.model, test_1)

linear.predictions.actual <- rep(1, nrow(test_1))
linear.predictions.actual[linear.predictions < .5] <- 0

test_1[, "Linear Prediction"] <- linear.predictions.actual

linear.table <- table(test_1[,"Linear Prediction"], test_1[,"Selected"])
colnames(linear.table) <- c("True No", "True Yes")
rownames(linear.table) <- c("Pred. No", "Pred. Yes")

print("The following shows the confusion matrix and the prediction accuracy for a linear model")
linear.table
mean(test_1[,"Linear Prediction"] == test_1[,"Selected"])
```

```{r echo=FALSE, cache=TRUE}
myvars <- c("FGM_Magnetic_Field_w", "FGM_Magnetic_Field_x", "FGM_Magnetic_Field_y", "FGM_Magnetic_Field_z", "Selected")
train_1 <- train_1[, myvars]
test_1 <- test_1[, myvars] 

logistic.model <- glm(Selected ~ . , data = train_1, family = binomial)
logistic.predictions <- predict(logistic.model, test_1, type="response")

logistic.predictions.actual <- rep(1, nrow(test_1))
logistic.predictions.actual[logistic.predictions < .5] <- 0

test_1[, "Logistic Prediction"] <- logistic.predictions.actual

logistic.table <- table(test_1[,"Logistic Prediction"], test_1[,"Selected"])
colnames(logistic.table) <- c("True No", "True Yes")
rownames(logistic.table) <- c("Pred. No", "Pred. Yes")

print("The following shows the confusion matrix and the prediction accuracy for a logistic model")
logistic.table
mean(test_1[,"Logistic Prediction"] == test_1[,"Selected"])
```
