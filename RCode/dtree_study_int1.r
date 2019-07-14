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
library(caret)

source("study_functions.R")

set.seed(62)

# Read CSV into R
RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int1.csv", header=TRUE, sep=",")

str(RDVData)

sum(is.na(RDVData$heart_weight))
sum(is.na(RDVData$comb_lung_weight))
sum(is.na(RDVData$liver_weight))
sum(is.na(RDVData$pancreas_weight))
sum(is.na(RDVData$thymus_weight))
sum(is.na(RDVData$spleen_weight))
sum(is.na(RDVData$comb_adrenal_weight))
sum(is.na(RDVData$thyroid_weight))
sum(is.na(RDVData$comb_kidney_weight))
sum(is.na(RDVData$brain_weight))

#Remove unwanted columns
clean_RDVData <- RDVData %>%
select(-c(event_id, event_start_date, age_category, case_id, include_in_study, foot_length, crown_rump_length, thymus_weight, thyroid_weight )) %>%
  na.omit()

glimpse(clean_RDVData)

data_train <- create_train_test(clean_RDVData, 0.8, train = TRUE)
data_test <- create_train_test(clean_RDVData, 0.8, train = FALSE)
dim(data_train)
dim(data_test)

prop.table(table(data_train$cod2_summ))
prop.table(table(data_test$cod2_summ))
summary(data_test$cod2_summ)

# rpart(formula, data=, method='')
# arguments:			
#   - formula: The function to predict
#   - data: Specifies the data frame
#   - method: 			
#     - "class" for a classification tree 			
#     - "anova" for a regression tree	

fit <- rpart(cod2_summ~., data = data_train, method = 'class')
rpart.plot(fit)

fit.importance <- varImp(fit)
fit.importance

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

r_minsplit = seq(1,200,by=20)
r_maxdepth = seq(1,10,by=1)
row = 0

max_accuracy = 0
max_minsplit = 0
max_maxdepth = 0

accuracy_matrix = matrix(nrow=10,ncol=10)

for (ms in r_minsplit) {
  row = row + 1
  col = 0
  for (md in r_maxdepth) {
    col = col + 1
    control <- rpart.control(minsplit = ms,
                             minbucket = round(ms / 3),
                             maxdepth = md,
                             cp = 0)
    tune_fit <- rpart(cod2_summ~., data = data_train, method = 'class', control = control)

    accuracy_matrix[row,col] = accuracy_tune(tune_fit)
    
    if (accuracy_tune(tune_fit) > max_accuracy) {
      max_accuracy = accuracy_tune(tune_fit)
      max_minsplit = ms
      max_maxdepth = md
    }
    
  }
  if (row == 1) {
   plot(accuracy_matrix[row,], type="o", ylim=c(0.5,0.7), pch=row-1, xlab= "maxdepth", ylab= "accuracy")
  } else {
   lines(accuracy_matrix[row,], type="o", pch=row-1)
  }
  
}

legend(9, 0.58, c("1","2","3","4","5","6","7","8","9","10"), cex = 0.8, pch = 0:9, lty = 1)

title(main="rpart control variables", col.main="red", font.main=4)

print(paste('Max Accuracy for test', max_accuracy))
print(paste('  For minsplit', max_minsplit))
print(paste('      maxdepth', max_maxdepth))

accuracy_matrix

control <- rpart.control(minsplit = max_minsplit,
                         minbucket = round(max_minsplit / 3),
                         maxdepth = max_maxdepth,
                         cp = 0)
tune_fit <- rpart(cod2_summ~., data = data_train, method = 'class', control = control)

tune_fit.importance <- varImp(tune_fit)
tune_fit.importance

rpart.plot(tune_fit)

accuracy_tune(tune_fit)

predict_unseen <- predict(tune_fit, data_test, type = 'class')

# Create confusion matrix
summary(data_test$cod2_summ)
summary(predict_unseen)
table_mat <- table(data_test$cod2_summ, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

print(paste('Accuracy for test', accuracy_Test))
