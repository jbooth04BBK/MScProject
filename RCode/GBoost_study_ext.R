# References
#
# https://shirinsplayground.netlify.com/2018/11/ml_basics_gbm
#

# Clear work space
rm(list = ls())

# Load libraries
library(dplyr)
library(caret)
library(xgboost)

set.seed(62)

# Read CSV into R
RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_ext.csv", header=TRUE, sep=",")

#Remove unwanted columns as for decission tree
clean_RDVData <- RDVData %>%
  select(-c(event_id, event_start_date, age_category, case_id, include_in_study, foot_length, crown_rump_length)) %>%
  na.omit()

str(clean_RDVData)

# create_train_test(df, size = 0.8, train = TRUE)
# arguments:
#  -df: Dataset used to train the model.
#  -size: Size of the split. By default, 0.8. Numerical value
#  -train: If set to `TRUE`, the function creates the train set, otherwise the test set. Default value sets to `TRUE`. Boolean value.
#   You need to add a Boolean parameter because R does not allow to return two data frames simultaneously.

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(clean_RDVData, 0.8, train = TRUE)
data_test <- create_train_test(clean_RDVData, 0.8, train = FALSE)
dim(data_train)
dim(data_test)

prop.table(table(data_train$cod2_summ))
prop.table(table(data_test$cod2_summ))

#########################################################
# caret

# Train model with preprocessing & repeated cv
model_gbm <- caret::train(cod2_summ ~ .,
                          data = data_train,
                          method = "gbm",
                          trControl = trainControl(method = "repeatedcv",
                                                   number = 5,
                                                   repeats = 3,
                                                   verboseIter = FALSE),
                          verbose = 0)
model_gbm


caret::confusionMatrix(
  data = predict(model_gbm, data_test),
  reference = data_test$cod2_summ
)




#########################################################
# XGBoost

xgboost_model<- xgboost(data = as.matrix(data_train[, -1]),
           label = as.numeric(data_train$cod2_summ)-1,
           max_depth = 3,
           objective = "binary:logistic",
           nrounds = 10,
           verbose = FALSE,
           prediction = TRUE)

xgboost_model


