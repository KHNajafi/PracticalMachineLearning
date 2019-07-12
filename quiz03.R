#ML WORKBOOK

## 3rd Eval


# # # #


# 1 
require(dplyr)
require(AppliedPredictiveModeling)
require(caret)
data("segmentationOriginal")

training <- segmentationOriginal %>% filter(Case == "Train")
test <- segmentationOriginal %>% filter(Case == "Test")
set.seed(125)

model_CART <- train(Class ~ ., method = "rpart", data = training)
print(model_CART$finalModel)
plot(model_CART$finalModel, uniform = T)
text(model_CART$finalModel, use.n = T, all = T)
require(rattle)
fancyRpartPlot(model_CART$finalModel)


# # # #


# 3 - Tree Model for olive
require(pgmm)
data(olive)
olive <- olive[, -1]
model_CART <- train(Area ~ ., method = "rpart", data = olive)
new_data <- as.data.frame(t(colMeans(olive)))
predict(model_CART, newdata = new_data)


# # # #


# 4 - 
require(ElemStatLearn)
data("SAheart")
set.seed(8484)
# convert outcome to factor
SAheart <- SAheart %>%
        mutate(chd = factor(chd))
trains <- sample(1:dim(SAheart)[1], size=dim(SAheart)[1]/2, replace=F)
trainingSA <- SAheart[trains, ]
testingSA <- SAheart[-trains, ]

set.seed(13234)
model_SA <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,
                  family = binomial(),
                  method = "glm", 
                  data = trainingSA)

## Misclassification for Training
1 - model_SA$results[2]

## Misclassification for Testing
#confusion matrix
confusionMatrix(testingSA$chd, predict(model_SA, testingSA))

1 - confusionMatrix(testingSA$chd, predict(model_SA, testingSA))[[3]][1]


# # # #


# 5 - 
require(ElemStatLearn)
data("vowel.train")
data("vowel.test")

# convert outcome to factor for both training and testing
vowel.train <- vowel.train %>%
        mutate(y = as.factor(y))
vowel.test <- vowel.test %>%
        mutate(y = as.factor(y))

# random forest model
set.seed(33833)
model_vowel <- train(y ~ ., method = "rf", data = vowel.train)
model_vowel_rf <- randomForest::randomForest(vowel.test)
