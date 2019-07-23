# References
#
# https://shirinsplayground.netlify.com/2018/11/ml_basics_gbm
# https://rpubs.com/dalekube/XGBoost-Iris-Classification-Example-in-R
#

# Load libraries
library(dplyr)
library(caret)
library(xgboost)

# Clear work space
rm(list = ls())

source("study_functions.R")

set.seed(62)

# stage = "ext"
# stage = "int1"
# stage = "int2"
# stage = "int3"
stage = "int3_s"

# Read CSV into R
if (stage == "ext") { 
  RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_ext_adj.csv", header=TRUE, sep=",")
} else if (stage == "int1") {
  RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int1_adj.csv", header=TRUE, sep=",")
} else if  (stage == "int2") {
  RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int2_adj.csv", header=TRUE, sep=",")
} else if  (stage == "int3") {
  RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int2_adj.csv", header=TRUE, sep=",")
} else {
  RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int3_s_adj.csv", header=TRUE, sep=",")
}

#Remove unwanted columns
if (stage == "ext") { 
  clean_RDVData <- RDVData %>%
    select(-c(event_id, event_start_date, age_category, case_id, include_in_study)) %>%
    na.omit()
} else {
  clean_RDVData <- RDVData %>%
    select(-c(event_id, event_start_date, age_category, case_id, include_in_study, foot_length, crown_rump_length)) %>%
    na.omit()
}  

clean_RDVData$cod2_summ <- as.factor(clean_RDVData$cod2_summ)

str(clean_RDVData)

#########################################################
# XGBoost

xgb.data <- clean_RDVData

num_class = length(levels(xgb.data$cod2_summ))
cod2_summ = clean_RDVData$cod2_summ

# Convert from class to numeric
str(xgb.data)

label <- as.integer(xgb.data$cod2_summ) - 1
xgb.data$cod2_summ = NULL

str(xgb.data)

n = nrow(xgb.data)
train.index = sample(n,floor(0.80 * n))
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
  eta=0.3,
  max_depth=9, 
  gamma=0,
  subsample=1,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

# Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=1000,
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

xgb.plot.importance (importance_matrix = importance[1:30]) 
 
