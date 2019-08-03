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
library('rsvg') # NB installed package
library('DiagrammeRsvg') # NB installed package
library(reshape2)

# Clear work space
rm(list = ls())

source("study_functions.R")

now <- Sys.time()
run.seed <- as.integer((second(now) - as.integer(second(now))) * 1000)
set.seed(run.seed)

# Adjusted data or not
data.adjusted <- TRUE
if (data.adjusted) {
  rdv.type = "_adj"
} else {
  rdv.type = ""
}

source.dir <- "I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs"
results.dir <- "I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\Results"
sub.dir <- format(now, "%Y%m%d_%H%M")

sub.dir <- "20190803_1304"

if (!dir.exists(file.path(results.dir, sub.dir))) {
  dir.create(file.path(results.dir, sub.dir))
}

model.name = "XGBoost Tree"
model.abv = "xgb"

fimp.matrix <- setup.fimp.matrix(rdv.type, now, source.dir)

results.matrix <- setup.results.matrix()

for(stage.num in 1:5) {
  
  rm.com <- 1
  
  if (stage.num == 1) { 
    stage = "ext"
  } else if (stage.num == 2) {
    stage = "int1"
  } else if  (stage.num == 3) {
    stage = "int2"
  } else if  (stage.num == 4) {
    stage = "int3"
  } else {
    stage = "int3_s"
  }
  
  RDVData <- read.csv(file=paste0(source.dir, "\\rdv_study_", stage, rdv.type, ".csv"), header=TRUE, sep=",")
  
  results.matrix[stage.num,rm.com] = stage
  rm.com = rm.com + 1
  results.matrix[stage.num,rm.com] = run.seed
  rm.com = rm.com + 1
  
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
  
  results.matrix[stage.num,rm.com] = nrow(clean_RDVData)
  rm.com = rm.com + 1
  
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

  results.matrix[stage.num,rm.com] = result
  rm.com = rm.com + 1
  
  # Create confusion matrix
  table_mat <- table(xgb.pred$label, xgb.pred$prediction)

  # Loop over my_matrix
  for(row in 1:nrow(table_mat)) {
    for(col in 1:ncol(table_mat)) {
      results.matrix[stage.num,rm.com] = table_mat[row, col]
      rm.com = rm.com + 1
    }
  }
  
  importance <- xgb.importance(model = xgb.fit)
  imp <- as.data.frame(importance)

  total_imp = sum(imp$Gain)
  
  for (imp_row in 1:nrow(imp)){
    # print(paste(imp_row,imp[imp_row,1],(imp[imp_row,2] / total_imp) * 100))
    res_row = which(fimp.matrix$feature == imp[imp_row,1])
    fimp.matrix[res_row, stage.num + 1] <- (imp[imp_row,2] / total_imp) * 100
  }
  
  plot.title = paste0("Feature Importance - Model: ",model.name,", Stage: ",stage)
  
  p <- ggplot(imp, aes(x=reorder(Feature, Gain), y=Gain))
  p <- p + geom_point()
  p <- p + geom_segment(aes(x=Feature,xend=Feature,y=0,yend=Gain))
  p <- p + ggtitle(plot.title)
  p <- p + ylab("Relative Importance")
  p <- p + xlab("Feature")
  p <- p + coord_flip()
  
  print(p)
  
  ggsave(paste0(file.path(results.dir, sub.dir), "\\", model.abv, "_feature_importance_",stage,".png"))

  # Single tree plot
  
  p <- xgb.plot.tree(model = xgb.fit, trees = 0, show_node_id = TRUE)
  
  print(p)

  gr <- xgb.plot.tree(model=xgb.fit, trees=0, show_node_id = TRUE, render=FALSE) 
  export_graph(gr, paste0(file.path(results.dir, sub.dir), "\\", model.abv, "_tree_",stage,".png"), width=1500, height=1900)
  
}

#############################
## graph combined importance
#############################

data <- fimp.matrix
# Order results
data$feature <- with(data, reorder(feature, ext + int1 + int2 + int3 + int3_s))
# Remove 0 values and create structure to plot
data.m.ss <- subset(melt(data), value > 0)
# Create plot
plot.title = paste0("Feature Importance Heatmap - Model: ",model.name)
p <- ggplot(data.m.ss, aes(x=variable, y=feature)) 
p <- p + ggtitle(plot.title)
p <- p + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "green", high = "red")

print(p)

ggsave(paste0(file.path(results.dir, sub.dir), "\\", model.abv, "_feature_importance_hm.png"))

#############################
## output results CSV files
#############################

#NB Now recorded at top so all files should have the same timestamp
write.csv(results.matrix, file = paste0(file.path(results.dir, sub.dir), "\\", model.abv, "_results.matrix.csv"),row.names=FALSE, na="")
write.csv(fimp.matrix, file = paste0(file.path(results.dir, sub.dir), "\\", model.abv, "_feature_importance_matrix.csv"),row.names=FALSE, na="")

#################################
