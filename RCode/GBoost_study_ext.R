# References
#
# https://shirinsplayground.netlify.com/2018/11/ml_basics_gbm
# https://rpubs.com/dalekube/XGBoost-Iris-Classification-Example-in-R
#

# Clear work space
rm(list = ls())

# Load libraries
library(dplyr)
library(caret)
library(xgboost)

source("study_functions.R")

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

xgb.data <- create_train_test(clean_RDVData, 0.8, train = TRUE)
data_test <- create_train_test(clean_RDVData, 0.8, train = FALSE)

#########################################################
# caret

# Train model with preprocessing & repeated cv
model_gbm <- caret::train(cod2_summ ~ .,
                          data = xgb.data,
                          method = "gbm",
                          trControl = trainControl(method = "repeatedcv",
                                                   number = 5,
                                                   repeats = 3,
                                                   verboseIter = FALSE),
                          verbose = 0)
model_gbm

#Importance of features
model_gbm.importance <- summary(model_gbm)

caret::confusionMatrix(
  data = predict(model_gbm, data_test),
  reference = data_test$cod2_summ
)


#########################################################
# XGBoost

xgb.data <- clean_RDVData

num_class = length(levels(xgb.data$cod2_summ))
cod2_summ = clean_RDVData$cod2_summ

# Convert from class to numeric
str(xgb.data)

xgb.data$sex <- as.integer(xgb.data$sex) - 1
xgb.data$neglect_ynid <- as.integer(xgb.data$neglect_ynid) - 1
xgb.data$nutrition_nutn_id <- as.integer(xgb.data$nutrition_nutn_id) - 1
xgb.data$dysmorphic_features_ynid <- as.integer(xgb.data$dysmorphic_features_ynid) - 1
xgb.data$jaundice_ynid <- as.integer(xgb.data$jaundice_ynid) - 1
xgb.data$oedema_ynid <- as.integer(xgb.data$oedema_ynid) - 1
xgb.data$pallor_ynid <- as.integer(xgb.data$pallor_ynid) - 1
xgb.data$blood_at_mouth_bmid  <- as.integer(xgb.data$blood_at_mouth_bmid ) - 1
xgb.data$signs_of_trauma_ynid <- as.integer(xgb.data$signs_of_trauma_ynid) - 1
xgb.data$signs_of_treatment_ynid <- as.integer(xgb.data$signs_of_treatment_ynid) - 1

label <- as.integer(xgb.data$cod2_summ) - 1
xgb.data$cod2_summ = NULL

str(xgb.data)

n = nrow(xgb.data)
train.index = sample(n,floor(0.80*n))
train.data = as.matrix(xgb.data[train.index,])
train.label = label[train.index]
test.data = as.matrix(xgb.data[-train.index,])
test.label = label[-train.index]

str(train.data)
str(train.label)

# Transform the two data sets into xgb.Matrix
xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)

params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

# Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)

# Review the final model and results
xgb.fit

# Predict outcomes with the test data
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(cod2_summ)

# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = levels(cod2_summ)[test.label+1]

# Calculate the final accuracy
result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))

# Create confusion matrix
table_mat <- table(xgb.pred$label, xgb.pred$prediction)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

print(paste('Accuracy for test', accuracy_Test))

importance <- xgb.importance(model = xgb.fit)
importance
