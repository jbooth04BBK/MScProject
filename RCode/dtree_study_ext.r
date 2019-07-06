# References
#
# https://medium.com/analytics-vidhya/a-guide-to-machine-learning-in-r-for-beginners-decision-trees-c24dfd490abb
# https://www.guru99.com/r-decision-trees.html
#

# Clear work space
rm(list = ls())

# Load libraries
library(dplyr)
library(rpart)
library(rpart.plot)

set.seed(62)

# Read CSV into R
RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_ext.csv", header=TRUE, sep=",")

head(RDVData)

str(RDVData)

sum(is.na(RDVData$body_weight))
sum(is.na(RDVData$head_circumference))
sum(is.na(RDVData$crown_rump_length))
sum(is.na(RDVData$body_length))
sum(is.na(RDVData$foot_length))
sum(is.na(RDVData$signs_of_treatment_ynid))

#Remove unwanted columns and factorise age_category & season
clean_RDVData <- RDVData %>%
select(-c(event_id, event_start_date, age_category, case_id, include_in_study, foot_length, crown_rump_length)) %>%
  na.omit()

glimpse(clean_RDVData)

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

# rpart(formula, data=, method='')
# arguments:			
#   - formula: The function to predict
#   - data: Specifies the data frame
#   - method: 			
#     - "class" for a classification tree 			
#     - "anova" for a regression tree	

fit <- rpart(cod2_summ~., data = data_train, method = 'class')
rpart.plot(fit)

# predict(fitted_model, df, type = 'class')
# arguments:
#   - fitted_model: This is the object stored after model estimation. 
# - df: Data frame used to make the prediction
# - type: Type of prediction			
#   - 'class': for classification			
#   - 'prob': to compute the probability of each class			
#   - 'vector': Predict the mean response at the node level	

predict_unseen <-predict(fit, data_test, type = 'class')

# Create confusion matrix
table_mat <- table(data_test$cod2_summ, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

print(paste('Accuracy for test', accuracy_Test))

# rpart.control(minsplit = 20, minbucket = round(minsplit/3), maxdepth = 30)
# Arguments:
#   -minsplit: Set the minimum number of observations in the node before the algorithm perform a split
#   -minbucket:  Set the minimum number of observations in the final note i.e. the leaf
#   -maxdepth: Set the maximum depth of any node of the final tree. The root node is treated a depth 0

accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$cod2_summ, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

control <- rpart.control(minsplit = 5,
                         minbucket = round(5 / 3),
                         maxdepth = 5,
                         cp = 0)
tune_fit <- rpart(cod2_summ~., data = data_train, method = 'class', control = control)

accuracy_tune(tune_fit)
