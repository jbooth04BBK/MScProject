# References
#
# https://www.guru99.com/r-random-forest-tutorial.html
#

# Load libraries
library(dplyr)
library(randomForest)
library(caret)
library(e1071)

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

model.name = "Random Forest"
model.abv = "rf"

fimp.matrix <- setup.fimp.matrix(rdv.type, now, source.dir)

results.matrix <- setup.results.matrix(model.abv)

for(stage.num in 1:5) {
  
  rm.col <- 1
  
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
  
  results.matrix[stage.num,rm.col] = stage
  rm.col = rm.col + 1
  results.matrix[stage.num,rm.col] = run.seed
  rm.col = rm.col + 1
  
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
  
  results.matrix[stage.num,rm.col] = nrow(clean_RDVData)
  rm.col = rm.col + 1
  
  clean_RDVData$cod2_summ <- as.factor(clean_RDVData$cod2_summ)
  
  data_train <- create_train_test(clean_RDVData, 0.8, train = TRUE)
  data_test <- create_train_test(clean_RDVData, 0.8, train = FALSE)
  
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
  # - `method`: Define which model to train. 
  # - `metric` = "Accuracy": Define how to select the optimal model
  # - `trControl = trainControl()`: Define the control parameters
  # - `tuneGrid = NULL`: Return a data frame with all the possible combination
  
  # Run the model
  rf_default <- train(cod2_summ~.,
                      data = data_train,
                      method = "rf",
                      metric = "Accuracy",
                      trControl = trControl)
  
  # Print the results
  
  # mtry = 2 with accuracy of 0.679
  # adj mtry = 24 with accuracy of 0.673
  
  best.mtry <- rf_default$bestTune$mtry
  
  print(best.mtry)
  print(max(rf_default$results$Accuracy))
  
  results.matrix[stage.num,rm.col] = best.mtry
  rm.col = rm.col + 1
  results.matrix[stage.num,rm.col] = max(rf_default$results$Accuracy)
  rm.col = rm.col + 1
  
  min.mtry <- best.mtry - 5
  max.mtry <- best.mtry + 5
  
  # Try and find a better mtry
  tuneGrid <- expand.grid(.mtry = c(min.mtry : max.mtry))
  rf_mtry <- train(cod2_summ~.,
                   data = data_train,
                   method = "rf",
                   metric = "Accuracy",
                   tuneGrid = tuneGrid,
                   trControl = trControl,
                   importance = TRUE,
                   nodesize = 14,
                   ntree = 300)
  
  # Max accuracy was 0.694 with mtry = 8
  # adj Max accuracy was 0.700 with mtry = 12
  
  best.mtry <- rf_mtry$bestTune$mtry
  
  print(best.mtry)
  print(max(rf_mtry$results$Accuracy))
  
  results.matrix[stage.num,rm.col] = best.mtry
  rm.col = rm.col + 1
  results.matrix[stage.num,rm.col] = max(rf_mtry$results$Accuracy)
  rm.col = rm.col + 1
  
  # Find best.maxnodes
  
  nodes.seq <- seq(4,30,by=2)
  
  rf.matrix = matrix(nrow = length(nodes.seq),ncol = 2)
  rfm.row <- 1
  
  accuracy.max = 0
  best.maxnodes = 0
  
  tuneGrid <- expand.grid(.mtry = best.mtry)
  
  for (maxnodes in nodes.seq) {
  
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
      
    rf.matrix[rfm.row,1] <- maxnodes
    rf.matrix[rfm.row,2] <- max(rf_maxnode$results$Accuracy)
    
    if (accuracy.max < max(rf_maxnode$results$Accuracy)) {
      accuracy.max <- max(rf_maxnode$results$Accuracy)
      best.maxnodes <- maxnodes
    }
    
    rfm.row <- rfm.row + 1
    
  }
  
  print(best.maxnodes)
  print(accuracy.max)
  
  results.matrix[stage.num,rm.col] = best.maxnodes
  rm.col = rm.col + 1
  results.matrix[stage.num,rm.col] = accuracy.max
  rm.col = rm.col + 1
  
  # Max accuracy was 0.740 with maxnodes = 16
  # adj Max accuracy was 0.751 with maxnodes = 16
  # best_maxnodes = 16
  
  # Now find best number of trees
  
  tree.seq <- c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 800, 1000)
  
  rf.matrix = matrix(nrow = length(tree.seq),ncol = 2)
  rfm.row <- 1
  
  accuracy.max = 0
  best.ntree = 0
  
  for (ntree in tree.seq) {
  
      rf_maxtrees <- train(cod2_summ~.,
                         data = data_train,
                         method = "rf",
                         metric = "Accuracy",
                         tuneGrid = tuneGrid,
                         trControl = trControl,
                         importance = TRUE,
                         nodesize = 14,
                         maxnodes = best.maxnodes,
                         ntree = ntree)
  
      rf.matrix[rfm.row,1] <- ntree
      rf.matrix[rfm.row,2] <- max(rf_maxtrees$results$Accuracy)
      
      if (accuracy.max < max(rf_maxtrees$results$Accuracy)) {
        accuracy.max <- max(rf_maxtrees$results$Accuracy)
        best.ntree <- ntree
      }
      
      rfm.row <- rfm.row + 1
      
  }
  
  print(best.ntree)
  print(accuracy.max)
  
  results.matrix[stage.num,rm.col] = best.ntree
  rm.col = rm.col + 1
  results.matrix[stage.num,rm.col] = accuracy.max
  rm.col = rm.col + 1
  
  # Max accuracy of 0.7457 with 30 trees
  # adj 1 Max accuracy of 0.7457 with 45 trees
  # adj 2 Max accuracy of 0.7514 with 300 trees
  # best_maxtrees = 300
  
  # The best model
  fit_rf <- train(cod2_summ~.,
                  data = data_train,
                  method = "rf",
                  metric = "Accuracy",
                  tuneGrid = tuneGrid,
                  trControl = trControl,
                  importance = TRUE,
                  nodesize = 14,
                  maxnodes = best.maxnodes,
                  ntree = best.ntree)
  
  print(max(fit_rf$results$Accuracy))
  
  results.matrix[stage.num,rm.col] = max(fit_rf$results$Accuracy)
  rm.col = rm.col + 1
  
  #############################
  ## Store and graph importance
  #############################
  
  imp <- as.data.frame(varImp(fit_rf)$importance)
  imp$value <- rep(0, nrow(imp)) 
  
  total_imp = sum(imp$`0`)
  
  for (imp_row in 1:nrow(imp)){
    res_row = which(fimp.matrix$feature == rownames(imp)[imp_row])
    fimp.matrix[res_row, stage.num + 1] <- (imp[imp_row,1] / total_imp) * 100
    imp[imp_row,3]  <- (imp[imp_row,1] / total_imp) * 100 
  }
  
  imp$varnames <- rownames(imp) # row names to column
  rownames(imp) <- NULL  
  
  plot.title = paste0("Feature Importance - Model: ",model.name,", Stage: ",stage)
  
  p <- ggplot(imp, aes(x=reorder(varnames, value), y=value))
  p <- p + geom_point()
  p <- p + geom_segment(aes(x=varnames,xend=varnames,y=0,yend=value))
  p <- p + ggtitle(plot.title)
  p <- p + ylab("Relative Importance")
  p <- p + xlab("Feature")
  p <- p + coord_flip()
  
  print(p)
  
  ggsave(paste0(file.path(results.dir, sub.dir), "\\", model.abv, "_feature_importance_",stage,".png"))
  
  #############################
  ## Store confusion matrix
  #############################
  
  results.matrix[stage.num,rm.col] = accuracy_fit(fit_rf, "raw")
  rm.col = rm.col + 1
  
  table_mat <- cmatrix_fit(fit_rf, "raw")
  
  # Loop over my_matrix
  for(row in 1:nrow(table_mat)) {
    for(col in 1:ncol(table_mat)) {
      results.matrix[stage.num,rm.col] = table_mat[row, col]
      rm.col = rm.col + 1
    }
  }



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
p <- p + theme(axis.text=element_text(size=6))
print(p)

ggsave(paste0(file.path(results.dir, sub.dir), "\\", model.abv, "_feature_importance_hm.png"))

#############################
## output results CSV files
#############################

write.csv(results.matrix, file = paste0(file.path(results.dir, sub.dir), "\\", model.abv, "_results.matrix.csv"),row.names=FALSE, na="")
write.csv(fimp.matrix, file = paste0(file.path(results.dir, sub.dir), "\\", model.abv, "_feature_importance_matrix.csv"),row.names=FALSE, na="")

#################################

