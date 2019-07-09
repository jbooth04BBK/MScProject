# References
#
# https://www.guru99.com/r-random-forest-tutorial.html
#

# Clear work space
rm(list = ls())

# Load libraries
library(dplyr)
library(randomForest)
library(caret)
library(e1071)

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

# trainControl(method = "cv", number = n, search ="grid")
# arguments
# - method = "cv": The method used to resample the dataset. 
# - number = n: Number of folders to create
# - search = "grid": Use the search grid method. For randomized method, use "grid"
# Note: You can refer to the vignette to see the other arguments of the function.

# Define the control
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

# train(formula, df, method = "rf", metric= "Accuracy", trControl = trainControl(), tuneGrid = NULL)
# argument
# - `formula`: Define the formula of the algorithm
# - `method`: Define which model to train. Note, at the end of the tutorial, there is a list
#             of all the models that can be trained
# - `metric` = "Accuracy": Define how to select the optimal model
# - `trControl = trainControl()`: Define the control parameters
# - `tuneGrid = NULL`: Return a data frame with all the possible combination

# Run the model
set.seed(62)
rf_default <- train(cod2_summ~.,
                    data = data_train,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl)
# Print the results
print(rf_default)

# mtry = 2 with accuracy of 0.679

# Try and find a better mtry
set.seed(62)
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(cod2_summ~.,
                 data = data_train,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)

print(rf_mtry)

# Max accuracy was 0.694 with mtry = 8

rf_mtry$bestTune$mtry
max(rf_mtry$results$Accuracy)
best_mtry <- rf_mtry$bestTune$mtry
print(best_mtry)

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 15)) {
  set.seed(62)
  rf_maxnode <- train(cod2_summ~.,
                      data = data_train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

# Max accuracy was 0.734 with maxnodes = 15
# Try higher max nodes

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(20: 30)) {
  set.seed(62)
  rf_maxnode <- train(cod2_summ~.,
                      data = data_train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

# Max accuracy 0.745 with 23 Maxnodes

# Now find best number of trees

store_maxtrees <- list()
for (ntree in c(150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 800, 1000)) {
  set.seed(1234)
  rf_maxtrees <- train(cod2_summ~.,
                       data = data_train,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 23,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
  print(key)
  print(rf_maxtrees)
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

# Max accuracy of 0.750 with 800 trees

# The best model
set.seed(62)
fit_rf <- train(cod2_summ~.,
                data = data_train,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                maxnodes = 23,
                ntree = 800)

print(fit_rf)
# Accuracy = 0.6917

# predict(model, newdata= df)
# argument
# - `model`: Define the model evaluated before.
# - `newdata`: Define the dataset to make prediction

prediction <-predict(fit_rf, data_test)
confusionMatrix(prediction, data_test$cod2_summ)

varImp(fit_rf)
