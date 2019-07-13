## Course Project
## Practical Machine Learning - JHU Coursera
## July 2019
## Khalil H Najafi
##
## ABSTRACT:
## This report covers the background, creation, and evaluation of an ML based on Jawbone data

# # # # # # # # # #

###### FUNCTIONS & WORKSPACE ######
# REQUIRED LIBRARIES
libraries <- c("data.table", "tidyverse", "lubridate", "caret", "randomForest", "corrplot")
lapply(libraries, require, character.only = T)
rm(libraries)




###### DATASET CREATION ######

#### Raw Dataset Import ####
## The datasets for the project are provided by Velloso et al
## Cited source: http://groupware.les.inf.puc-rio.br/work.jsf?p1=11201

## Training Dataset
data_training <- fread("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
data_training <- as.data.frame(data_training)


# Testing Dataset
data_testing <- fread("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")


#### Dataset Tidying & Preprocessing ####
# 0 - set seed for reproducibility
# 1 - check var names/purpose for IDs, row numbers, non-useful info etc and remove
# 2 - check for missing values vars
# 3a - assess the outcome variable (if classification is it balanced, if pred what's distro)
# 3b - assess the data of resulting subset (distribution etc)
# 4 - cross-validation splits for building and optimizing
# 5 - basic preprocessing

## 0
set.seed(2604)

## 1
# First seven variables are IDs and timestamps, we'll remove
id_vars <- names(data_training)[1:7]
data_training <- data_training %>%
        select(-id_vars)

## 2
# Checking if a variable is entirely missing (NA)
# non_NA <- function(x) {!all(is.na(x))}
# data_training <- data_training %>%
#         select_if(non_NA)


check_NA <- function(x) {sum(is.na(x))}

data_training_NAs <- apply(data_training, 2, check_NA)
data_training <- data_training[,which(data_training_NAs == 0)]

# data_training_clean <- data_training %>%
#         drop_na()

## 3a
# Classification task

# Convert outcome variable type to factor
data_training <- data_training %>%
        mutate(classe = as.factor(classe))

# Plot the outcome variable to check class balance, missing values, etc
ggplot(data_training, aes(classe)) +
        geom_bar(aes(fill = classe)) +
        labs(title = "Classification Task - Target Variable",
             subtitle = "Class balance of training set",
             y = "")


## 3b
# Distribution of explanatory variables/features
x <- data_training %>%
        select(-classe) %>%
        gather() %>%
        filter(!is.na(value))

ggplot(x, aes(value)) +
        geom_density(alpha = 2/3) +
        facet_wrap(~ key, scales = "free")

# Correlation plot for remaining variables
corr_mat <- data_training %>%
        select(-classe) %>%
        cor()
#plot
corrplot(corr_mat, method = "circle", type = "lower")

## 4
# Further subsetting Training for model evaluation and optimization
index_training <- createDataPartition(y = data_training$classe,
                                      p = 0.7,
                                      list = F, )

data_training_build <- data_training %>%
        slice(index_training)
data_training_validation <- data_training %>%
        slice(-index_training)




#### Model Building ####
## Given the task + application, we'll fit a Random Forest model to the data
## For compute considerations we'll avoid too many variable splits per node and model fits
# rf_model <- train(classe ~ .,
#                   data = data_training,
#                   method = "rf")
rf_model <- randomForest(classe ~ .,
                         data = data_training_build)



#### Model Evaluation & Optimization ####
predictions_validation <- predict(rf_model, data_training_validation)
confusionMatrix(predictions_validation, data_training_validation$classe)



#### Application to Test Set ####

predictions_test <- predict(rf_model, data_testing)


