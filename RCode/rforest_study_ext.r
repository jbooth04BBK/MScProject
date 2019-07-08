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

