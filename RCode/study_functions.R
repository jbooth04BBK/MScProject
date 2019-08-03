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

accuracy_fit <- function(fit, type.str) {
  predict_unseen <- predict(fit, data_test, type = type.str)
  table_mat <- table(data_test$cod2_summ, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

cmatrix_fit <- function(fit, type.str = "class") {
  predict_unseen <- predict(fit, data_test, type = type.str)
  table_mat <- table(data_test$cod2_summ, predict_unseen)
  table_mat
}

setup.fimp.matrix <- function(rdv.type, now, source.dir) {
  
  ######################################
  # Create Feature Importance data frame
  ######################################
  
  #Read in largest CSVs and get unique list of column names
  RDVData <- read.csv(file=paste0(source.dir, "\\rdv_study_int3", rdv.type, ".csv"), header=TRUE, sep=",")
  cn = colnames(RDVData)
  RDVData <- read.csv(file=paste0(source.dir, "\\rdv_study_int3_s", rdv.type, ".csv"), header=TRUE, sep=",")
  cn1 = colnames(RDVData)
  
  cn <- append(cn, cn1, after = length(cn))
  cn <- unique(cn)
  
  # Remove unwanted columns
  cn <- cn[!cn %in% c("event_id", "event_start_date", "age_category", "case_id", "include_in_study", "foot_length", "crown_rump_length")]
  
  col_values  = replicate(length(cn),0.0)
  
  # create an empty data frame
  column_names <- c("feature","ext","int1","int2","int3","int3_s")
  fimp.matrix <- data.frame(cn, col_values, col_values, col_values, col_values, col_values)
  colnames(fimp.matrix) <- column_names
  
  return(fimp.matrix)
  
}

setup.results.matrix <- function(model.abv) {
  
  ######################################
  # Create matrix to store results
  ######################################
  
  if (model.abv == "dt") {
    column_names = c('Stage','run_seed', 'observations', 'max_accuracy','minsplit','maxdepth','accuracy','cm_r1_c1','cm_r1_c2','cm_r2_c1','cm_r2_c2')
  } else if (model.abv == "rf") {
    column_names = c('Stage','run_seed', 'observations','best_def_mtry','bmtd_accuracy','best_mtry','bmt_accuracy','best_maxnodes','bmn_accuracy','best_ntree','bnt_accuracy','max_accuracy','accuracy','cm_r1_c1','cm_r1_c2','cm_r2_c1','cm_r2_c2')
  } else {
    column_names = c('Stage','run_seed', 'observations','accuracy','cm_r1_c1','cm_r1_c2','cm_r2_c1','cm_r2_c2')
  }
  
  results.matrix = matrix(nrow=5,ncol=length(column_names))
  colnames(results.matrix) <- column_names
  
  return(results.matrix)
  
}