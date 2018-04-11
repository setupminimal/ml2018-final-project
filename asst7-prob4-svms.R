
```{r, warning=FALSE}
# Applying SVMs to the SITL data.

# Read in the data from the datafiles, using the included external script
setwd("/home/@home/daroc/Important/cs780/ml2018-final-project")
source("GetData.R")

train$Selected = as.factor(train$Selected)
test$Selected = as.factor(test$Selected)

```

```{r}
# This produces 'test' and 'train' variables with 0.25 and 0.75 of the data, 
respectivly.

# Let's try linear, polynomial, and radias SVMs.
lsvm.model <- svm(Selected ~ . - Priority - Time, data = train, kernel = "linear")
lsvm.prediction <- predict(lsvm.model, test)
lsvm.classError <- sum(ifelse(lsvm.prediction == test$Selected, 0, 1)) / nrow(test)
print(paste("LSVM Classfication Error: ", lsvm.classError))

psvm.model <- svm(Selected ~ . - Priority - Time, data = train, kernel = "polynomial")
psvm.prediction <- predict(psvm.model, test)
psvm.classError <- sum(ifelse(psvm.prediction == test$Selected, 0, 1)) / nrow(test)
print(paste("PSVM Classfication Error: ", psvm.classError))

rsvm.model <- svm(Selected ~ . - Priority - Time, data = train, kernel = "radial")
rsvm.prediction <- predict(rsvm.model, test)
rsvm.classError <- sum(ifelse(rsvm.prediction == test$Selected, 0, 1)) / nrow(test)
print(paste("RSVM Classfication Error: ", rsvm.classError))

```
