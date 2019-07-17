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

source("study_functions.R")

set.seed(62)

# Read CSV into R
RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int1.csv", header=TRUE, sep=",")

str(RDVData)

#Remove unwanted columns
clean_RDVData <- RDVData %>%
  select(-c(event_id, event_start_date, age_category, case_id, include_in_study, foot_length, crown_rump_length, thymus_weight, thyroid_weight )) %>%
  na.omit()

str(clean_RDVData)

data_train <- create_train_test(clean_RDVData, 0.8, train = TRUE)
data_test <- create_train_test(clean_RDVData, 0.8, train = FALSE)
dim(data_train)
dim(data_test)

prop.table(table(data_train$cod2_summ))
prop.table(table(data_test$cod2_summ))
summary(data_test$cod2_summ)

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

# mtry = 23 with accuracy of 0.764

# Try and find a better mtry
set.seed(62)
tuneGrid <- expand.grid(.mtry = c(20: 30))
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

# Max accuracy was 0.772 with mtry = 24

rf_mtry$bestTune$mtry
max(rf_mtry$results$Accuracy)
best_mtry <- rf_mtry$bestTune$mtry
print(best_mtry)

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in seq(20,50,by=2)) {
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

# Max accuracy 0.793 with 38 Maxnodes
best_maxnodes = 38

# Now find best number of trees
store_maxtrees <- list()
for (ntree in c(150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 800, 1000)) {
  set.seed(62)
  rf_maxtrees <- train(cod2_summ~.,
                       data = data_train,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = best_maxnodes,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
  print(key)
  # print(rf_maxtrees)
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

# Max accuracy of 0.792 with 200 trees
best_maxtrees = 800

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
                maxnodes = best_maxnodes,
                ntree = best_maxtrees)

print(fit_rf)
# Accuracy = 0.763

# predict(model, newdata= df)
# argument
# - `model`: Define the model evaluated before.
# - `newdata`: Define the dataset to make prediction

prediction <-predict(fit_rf, data_test)
confusionMatrix(prediction, data_test$cod2_summ)

# Create confusion matrix
summary(data_test$cod2_summ)
summary(prediction)
table_mat <- table(data_test$cod2_summ, prediction)
table_mat

fit_rf.importance <- varImp(fit_rf)$importance
fit_rf.importance
