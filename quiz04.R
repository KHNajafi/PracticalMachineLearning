#ML WORKBOOK

## 4th Eval

# REQUIRED LIBRARIES
libraries <- c("data.table", "tidyverse", "caret")
lapply(libraries, require, character.only = T)
rm(libraries)

# # # # # # # # # # #

#### Question 1 ####

# Load dataset
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

# Convert outcome to factor in training and test subsets
vowel.train <- mutate(vowel.train, y = as.factor(y))
vowel.test <- mutate(vowel.test, y = as.factor(y))

set.seed(33833)

## Model Fitting (Training Set)
## i - Random Forest Model
rf_model <- train(y ~ ., vowel.train, method = "rf", prox = T)

## ii - Gradient Boosting Model
gb_model <- train(y ~ ., vowel.train, method = "gbm")


## Model Evaluation (Testing Set)
# i - Predictions based on RF Model
predictions_rf <- predict(rf_model, vowel.test)

# Accuracy of RF
confusionMatrix(predictions_rf, vowel.test$y)

# ii - Predictions based on GB Model
predictions_gbm <- predict(gb_model, vowel.test)

# Accuracy of GB
confusionMatrix(predictions_gbm, vowel.test$y)




#### Question 2 ####

require(caret)
require(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData <- data.frame(diagnosis,predictors)
#index for training/testing set
inTrain <- createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training <- adData[ inTrain,]

testing <- adData[-inTrain,]

## Model Fitting
set.seed(62433)
# i - Random Forests (rf) Model
rf_model2 <- train(diagnosis ~ ., data = training, method = "rf", prox = T)

# ii - Gradient Boosted Trees (gbm) Model
gb_model2 <- train(diagnosis ~ ., training, method = "gbm")

# iii - Linear Discriminant Analysis (lda) Model
lda_model2 <- train(diagnosis ~ ., training, method = "lda")


## Model Evaluation
predictions_rf2 <- predict(rf_model2, testing)

predictions_gb2 <- predict(gb_model2, testing)

predictions_lda2 <- predict(lda_model2, testing)


# iv - Stacked Model using Random Forests
#stacked dataset
dat_stack2 <- data.frame(predictions_rf2, 
                         predictions_gb2, 
                         predictions_lda2, 
                         diagnosis = testing$diagnosis)
#model fit
rf_stackmodel2 <- train(diagnosis ~ ., data = dat_stack2, method = "rf")
#model evaluation
predictions_rfstack2 <- predict(rf_stackmodel2, dat_stack2)

## Model Accuracy
confusionMatrix(predictions_rf2, testing$diagnosis)

confusionMatrix(predictions_gb2, testing$diagnosis)

confusionMatrix(predictions_lda2, testing$diagnosis)

confusionMatrix(predictions_rfstack2, testing$diagnosis)



#### Question 3 ####
require(elasticnet)
set.seed(3523)

library(AppliedPredictiveModeling)
data(concrete)

inTrain <- createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training <- concrete[ inTrain,]
testing <- concrete[-inTrain,]

set.seed(233)
## Model Fitting (Lasso)
lass_model3 <- train(CompressiveStrength ~ ., method = "lasso", training)

# Determining the last coefficient set to zero (via elastic net regression plot)
plot.enet(lass_model3$finalModel, )



#### Question 4 ####
require(data.table)
require(lubridate)

# Dataset load and Training/Testing split
dat <- fread("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")

training <- dat[year(dat$date) < 2012,]
testing <- dat[(year(dat$date)) > 2011,]

tstrain <- ts(training$visitsTumblr)

## Model Fitting (Exponential Smoothing BATS)
bats_model4 <- bats(tstrain)

## Model Evaluation
predictions_bats4 <- forecast(bats_model4, level = 95, h = nrow(testing))

# Plot of predictions vs actual (test)
plot(predictions_bats4)
lines(testing[, c(1, 3)], col = "red")

# Accuracy Measurements - Testing observations
accuracy(predictions_bats4, testing[, visitsTumblr])

# Percentage of predictions within 95 confidence interval?
#data table
dat_predictions <- data.frame(lower = as.numeric(predictions_bats4$lower),
                              upper = as.numeric(predictions_bats4$upper),
                              actual = testing$visitsTumblr)
#determine if each actual observation is within 95 confidence range
dat_predictions <- dat_predictions %>%
        mutate(in95range = actual >= lower & actual <= upper)

#calculate overall accuracy
sum(dat_predictions$in95range)/nrow(dat_predictions)



#### Question 5 ####

set.seed(3523)

# Dataset load and Training/Testing split
library(AppliedPredictiveModeling)
data(concrete)

inTrain <- createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training <- concrete[ inTrain,]
testing <- concrete[-inTrain,]

## Model Fitting (Support Vector Machine)
require(e1071)
set.seed(325)

svm_model5 <- train(CompressiveStrength ~ ., training, method = "svmRadial")

## Model Evaluation
predictions_svm5 <- predict(svm_model5, testing)

# Accuracy (predictions v observed testing)
accuracy(predictions_svm5, testing$CompressiveStrength)
