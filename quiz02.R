#ML WORKBOOK

## 2nd Eval

# REQUIRED LIBRARIES
libraries <- c("data.table", "tidyverse", "caret")
lapply(libraries, require, character.only = T)
rm(libraries)

# `CompressiveStrength` plot
require(AppliedPredictiveModeling)
data(concrete)
require(caret)

set.seed(1000)

#index for training/test sets
inTrain <- createDataPartition(mixtures$CompressiveStrength, 
                               p = 3/4)[[1]]
training <- mixtures[inTrain, ]
test <- mixtures[-inTrain, ]
training <- as.data.table(training)
test <- as.data.table(test)

#create index (row numbers) for training set
training <- training %>%
        mutate(index = 1:nrow(training))

## Feature Plot
featurePlot(x = training[, c("Cement", "FlyAsh", "Age", "index")],
            y = training$CompressiveStrength,
            plot = "pairs")

## create plot of `CompressiveStrength` to index
#1 - Cement
ggplot(training, aes(index, CompressiveStrength)) + 
        geom_point(aes(colour = Cement))
#2 - BlastFurnaceSlag
ggplot(training, aes(index, CompressiveStrength)) + 
        geom_point(aes(colour = BlastFurnaceSlag))
#3 - FlyAsh
ggplot(training, aes(index, CompressiveStrength)) + 
        geom_point(aes(colour = FlyAsh))
#4 - Water
ggplot(training, aes(index, CompressiveStrength)) + 
        geom_point(aes(colour = Water))
#5 - Superplasticizer
ggplot(training, aes(index, CompressiveStrength)) + 
        geom_point(aes(colour = Superplasticizer))
#6 - CoarseAggregate
ggplot(training, aes(index, CompressiveStrength)) + 
        geom_point(aes(colour = CoarseAggregate))
#7 - FineAggregate
ggplot(training, aes(index, CompressiveStrength)) + 
        geom_point(aes(colour = FineAggregate))
#8 - Age
ggplot(training, aes(index, CompressiveStrength)) + 
        geom_point(aes(colour = Age))

## create groups of `CompressiveStrength` to explore index further
require(Hmisc)
cutCompressiveStrength <- cut2(training$CompressiveStrength, g = 3)


# # # # 


## histogram of `Superplasticizer`

require(AppliedPredictiveModeling)
data(concrete)
require(caret)

set.seed(1000)

#index for training/test sets
inTrain <- createDataPartition(mixtures$CompressiveStrength, 
                               p = 3/4)[[1]]
training <- mixtures[inTrain, ]
test <- mixtures[-inTrain, ]
training <- as.data.table(training)
test <- as.data.table(test)

#histogram plot
ggplot(training, aes(Superplasticizer)) + geom_histogram()


# # # # 


## PCA processing example
require(caret)
require(AppliedPredictiveModeling)
set.seed(3433)
data("abalone") #AlzheimerDisease dataset

adData <- data.frame(diagnosis, predictors)
inTrain <- createDataPartition(adData$diagnosis, p = 0.75, list = F)
training <- adData[inTrain, ]
test <- adData[-inTrain, ]

#subset training data to only 'IL' variables
trainingIL <- training %>% select(starts_with("il"))
#PCA on set of 'IL' variables
pca_IL <- preProcess(trainingIL, method = "pca", thresh = .9)


# # # # 


## Model comparison - PCA v non-PCA of 'IL' variables
require(caret)
require(AppliedPredictiveModeling)
set.seed(3433)
data("abalone") #AlzheimerDisease dataset

adData <- data.frame(diagnosis, predictors)
inTrain <- createDataPartition(adData$diagnosis, p = 0.75, list = F)
#create training subset of diagnosis and 'IL' variables only
training <- adData[inTrain, ]
training <- training %>% select(diagnosis, starts_with("il"))
test <- adData[-inTrain, ]

## Model Creation (fitted from Training Set)
##non-PCA model
#training the model
il_nonPCA <- train(diagnosis~., 
                   method = "glm", 
                   data = training)

##PCA model (with an 80% threshold for variance retained)
#perform PCA on data
pre_proc <- preProcess(training, method = "pca", thresh = 0.8)
#estimate predictions
train_pc <- predict(pre_proc, training)
#training the model
model_fit <- train(diagnosis ~ ., 
                   method = "glm", 
                   data = train_pc)

## Model Evaluation (applied to Test Set)
#non-PCA model
confusionMatrix(test$diagnosis, predict(il_nonPCA, test))

#PCA model
test_pc <- predict(pre_proc, test)
confusionMatrix(test$diagnosis, predict(model_fit, test_pc))
