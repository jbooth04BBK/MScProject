# References
#
# https://shirinsplayground.netlify.com/2018/11/ml_basics_gbm
# https://rpubs.com/dalekube/XGBoost-Iris-Classification-Example-in-R
#

# Load libraries
library(dplyr)
library(caret)
library(xgboost)
library(lubridate)
library('DiagrammeR') # NB installed package

# Clear work space
rm(list = ls())

source("study_functions.R")

now = Sys.time()
run_seed <- as.integer((second(now) - as.integer(second(now))) * 1000)
set.seed(run_seed)

######################################
# Create Feature Importance data frame
######################################

#Read in largest CSVs and get unique list of column names
RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int3_adj.csv", header=TRUE, sep=",")
cn = colnames(RDVData)
RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int3_s_adj.csv", header=TRUE, sep=",")
cn1 = colnames(RDVData)

cn <- append(cn, cn1, after = length(cn))
cn <- unique(cn)

# Remove unwanted columns
cn <- cn[!cn %in% c("event_id", "event_start_date", "age_category", "case_id", "include_in_study", "foot_length", "crown_rump_length")]

col_values  = replicate(length(cn),0.0)

# create an empty data frame
column_names <- c("feature","ext","int1","int2","int3","int3_s")
fimp_results <- data.frame(cn, col_values, col_values, col_values, col_values, col_values)
colnames(fimp_results) <- column_names

######################################
# Create matrix to store results
######################################

column_names = c('Stage','run_seed', 'observations','accuracy','cm_r1_c1','cm_r1_c2','cm_r2_c1','cm_r2_c2')

results_matrix = matrix(nrow=5,ncol=length(column_names))
colnames(results_matrix) <- column_names

for(n_stage in 1:5) {
  
  rm_col = 1
  
  if (n_stage == 1) { 
    stage = "ext"
    RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_ext_adj.csv", header=TRUE, sep=",")
  } else if (n_stage == 2) {
    stage = "int1"
    RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int1_adj.csv", header=TRUE, sep=",")
  } else if  (n_stage == 3) {
    stage = "int2"
    RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int2_adj.csv", header=TRUE, sep=",")
  } else if  (n_stage == 4) {
    stage = "int3"
    RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int3_adj.csv", header=TRUE, sep=",")
  } else {
    stage = "int3_s"
    RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int3_s_adj.csv", header=TRUE, sep=",")
  }
  
  results_matrix[n_stage,rm_col] = stage
  rm_col = rm_col + 1
  results_matrix[n_stage,rm_col] = run_seed
  rm_col = rm_col + 1
  
  #Remove unwanted columns - gestation_at_delivery_in_days
  if (stage == "ext") { 
    clean_RDVData <- RDVData %>%
      select(-c(event_id, event_start_date, age_category, case_id, gestation_at_delivery_in_days, include_in_study)) %>%
      na.omit()
  } else {
    clean_RDVData <- RDVData %>%
      select(-c(event_id, event_start_date, age_category, case_id, gestation_at_delivery_in_days, include_in_study, foot_length, crown_rump_length)) %>%
      na.omit()
  }  
  
  results_matrix[n_stage,rm_col] = nrow(clean_RDVData)
  rm_col = rm_col + 1
  
  clean_RDVData$cod2_summ <- as.factor(clean_RDVData$cod2_summ)
  
  
  #########################################################
  # XGBoost
  
  xgb.data <- clean_RDVData
  
  num_class = length(levels(xgb.data$cod2_summ))
  cod2_summ = clean_RDVData$cod2_summ
  
  # Convert from class to numeric
  label <- as.integer(xgb.data$cod2_summ) - 1
  xgb.data$cod2_summ = NULL
  
  n = nrow(xgb.data)
  train.index = sample(n,floor(0.80 * n))
  train.data = as.matrix(xgb.data[train.index,])
  train.label = label[train.index]
  test.data = as.matrix(xgb.data[-train.index,])
  test.label = label[-train.index]
  
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
  # xgb.fit
  
  # Predict outcomes with the test data
  xgb.pred = predict(xgb.fit,test.data,reshape=T)
  xgb.pred = as.data.frame(xgb.pred)
  colnames(xgb.pred) = levels(cod2_summ)
  
  # Use the predicted label with the highest probability
  xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
  xgb.pred$label = levels(cod2_summ)[test.label+1]
  
  # Calculate the final accuracy
  result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)

  results_matrix[n_stage,rm_col] = result
  rm_col = rm_col + 1
  
  # Create confusion matrix
  table_mat <- table(xgb.pred$label, xgb.pred$prediction)

  # Loop over my_matrix
  for(row in 1:nrow(table_mat)) {
    for(col in 1:ncol(table_mat)) {
      results_matrix[n_stage,rm_col] = table_mat[row, col]
      rm_col = rm_col + 1
    }
  }
  
  importance <- xgb.importance(model = xgb.fit)
  imp <- as.data.frame(importance)

  total_imp = sum(imp$Gain)
  
  for (imp_row in 1:nrow(imp)){
    # print(paste(imp_row,imp[imp_row,1],(imp[imp_row,2] / total_imp) * 100))
    res_row = which(fimp_results$feature == imp[imp_row,1])
    fimp_results[res_row, n_stage + 1] <- (imp[imp_row,2] / total_imp) * 100
  }
  
  print(xgb.plot.importance (importance_matrix = importance[1:30]) )
  
  print(xgb.plot.tree(model = xgb.fit, trees = 0, show_node_id = TRUE))
 
}

#############################
## graph combined importance
#############################

data <- fimp_results
data$feature <- with(data, reorder(feature, ext + int1 + int2 + int3 + int3_s))
data.m.ss <- subset(melt(data), value > 0)

p <- ggplot(data.m.ss, aes(x=variable, y=feature)) 
p + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "green", high = "red")

#NB Now recorded at top so all files should have the same timestamp
write.csv(results_matrix, file = paste0("xgb_results_matrix_",format(now, "%Y%m%d_%H%M%S"),".csv"),row.names=FALSE, na="")
write.csv(fimp_results, file = paste0("xgb_feature_importance_",format(now, "%Y%m%d_%H%M%S"),".csv"),row.names=FALSE, na="")

#################################