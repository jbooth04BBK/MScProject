#
# Functions used by all Studies
#

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

# predict(fitted_model, df, type = 'class')
# arguments:
#   - fitted_model: This is the object stored after model estimation. 
# - df: Data frame used to make the prediction
# - type: Type of prediction			
#   - 'class': for classification			
#   - 'prob': to compute the probability of each class			
#   - 'vector': Predict the mean response at the node level	

accuracy_fit <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$cod2_summ, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

cmatrix_fit <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$cod2_summ, predict_unseen)
  table_mat
}

