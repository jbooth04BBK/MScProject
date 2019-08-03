# References
#
# https://medium.com/analytics-vidhya/a-guide-to-machine-learning-in-r-for-beginners-decision-trees-c24dfd490abb
# https://www.guru99.com/r-decision-trees.html
#

# Load libraries
library(dplyr)
library(ggplot2) 
library(rpart)
library(rpart.plot)
library(caret)
library(lubridate)
library(reshape2)

# Clear work space
rm(list = ls())

source("study_functions.R")

now <- Sys.time()
run.seed <- as.integer((second(now) - as.integer(second(now))) * 1000)
set.seed(run.seed)

model.name = "Decision Tree"
model.abv = "dt"

source.dir <- "I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs"
results.dir <- "I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\Results"
sub.dir <- format(now, "%Y%m%d_%H%M")

if (!dir.exists(file.path(results.dir, sub.dir))) {
  dir.create(file.path(results.dir, sub.dir))
}

# Adjusted data or not
data.adjusted <- TRUE
if (data.adjusted) {
  rdv.type = "_adj"
} else {
  rdv.type = ""
}
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
fimp_results <- data.frame(cn, col_values, col_values, col_values, col_values, col_values)
colnames(fimp_results) <- column_names

######################################
# Create matrix to store results
######################################

column_names = c('Stage','run_seed', 'observations', 'max_accuracy','minsplit','maxdepth','accuracy','cm_r1_c1','cm_r1_c2','cm_r2_c1','cm_r2_c2')

results_matrix = matrix(nrow=5,ncol=length(column_names))
colnames(results_matrix) <- column_names

for(n_stage in 1:5) {

  rm_col = 1
  
  if (n_stage == 1) { 
    stage = "ext"
  } else if (n_stage == 2) {
    stage = "int1"
  } else if  (n_stage == 3) {
    stage = "int2"
  } else if  (n_stage == 4) {
    stage = "int3"
  } else {
    stage = "int3_s"
  }
  
  RDVData <- read.csv(file=paste0(source.dir, "\\rdv_study_", stage, rdv.type, ".csv"), header=TRUE, sep=",")
  
  results_matrix[n_stage,rm_col] = stage
  rm_col = rm_col + 1
  results_matrix[n_stage,rm_col] = run.seed
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
  
  data_train <- create_train_test(clean_RDVData, 0.8, train = TRUE)
  data_test <- create_train_test(clean_RDVData, 0.8, train = FALSE)
  
  # rpart(formula, data=, method='')
  # arguments:			
  #   - formula: The function to predict
  #   - data: Specifies the data frame
  #   - method: 			
  #     - "class" for a classification tree 			
  #     - "anova" for a regression tree	
  
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
  
      accuracy_matrix[row,col] = accuracy_fit(tune_fit)
      
      if (accuracy_fit(tune_fit) > max_accuracy) {
        max_accuracy = accuracy_fit(tune_fit)
        max_minsplit = ms
        max_maxdepth = md
      }
      
    }
    if (row == 1) {
     plot(accuracy_matrix[row,], type="o", ylim=c(0.5,0.9), pch=row-1, xlab= "maxdepth", ylab= "accuracy")
    } else {
     lines(accuracy_matrix[row,], type="o", pch=row-1)
    }
    
  }
  
  legend(9, 0.65, c("1","2","3","4","5","6","7","8","9","10"), cex = 0.8, pch = 0:9, lty = 1)
  
  title(main=paste0("rpart control variables, stage - ",stage), col.main="red", font.main=4)
  
  results_matrix[n_stage,rm_col] = max_accuracy
  rm_col = rm_col + 1
  results_matrix[n_stage,rm_col] = max_minsplit
  rm_col = rm_col + 1
  results_matrix[n_stage,rm_col] = max_maxdepth
  rm_col = rm_col + 1
  
  control <- rpart.control(minsplit = max_minsplit,
                           minbucket = round(max_minsplit / 3),
                           maxdepth = max_maxdepth,
                           cp = 0)
  
  tune_fit <- rpart(cod2_summ~., data = data_train, method = 'class', control = control)
  
  #############################
  ## Store and graph importance
  #############################
  
  imp <- as.data.frame(varImp(tune_fit))
  # Remove 0 importance variables
  imp <- subset(imp, Overall>0)

  total_imp = sum(imp)
  
  for (imp_row in 1:nrow(imp)){
    res_row = which(fimp_results$feature == rownames(imp)[imp_row])
    fimp_results[res_row, n_stage + 1] <- (imp[imp_row,1] / total_imp) * 100
  }
  
  imp$varnames <- rownames(imp) # row names to column
  rownames(imp) <- NULL  
  imp$var_categ <- rep(1, nrow(imp)) # random var category
  
  plot.title = paste0("Feature Importance - Model: ",model.name,", Stage: ",stage)
  
  p <- ggplot(imp, aes(x=reorder(varnames, Overall), y=Overall))
  p <- p + geom_point()
  p <- p + geom_segment(aes(x=varnames,xend=varnames,y=0,yend=Overall))
  p <- p + ggtitle(plot.title)
  p <- p + ylab("Relative Importance")
  p <- p + xlab("Feature")
  p <- p + coord_flip()
  
  print(p)
  
  ggsave(paste0(file.path(results.dir, sub.dir), "\\", model.abv, "_feature_importance_",stage,".png"))
  
  ##-------------------------------------------
  
  rpart.plot(tune_fit)
  title(main=paste0("Tree - Model: ",model.name,", Stage: ",stage), col.main="red", font.main=4)
  
  dev.copy(png,filename=paste0(file.path(results.dir, sub.dir), "\\", model.abv, "_tree_",stage,".png"));
  dev.off ();
  
  results_matrix[n_stage,rm_col] = accuracy_fit(tune_fit)
  rm_col = rm_col + 1
  
  table_mat <- cmatrix_fit(tune_fit)
  
  # Loop over my_matrix
  for(row in 1:nrow(table_mat)) {
    for(col in 1:ncol(table_mat)) {
      results_matrix[n_stage,rm_col] = table_mat[row, col]
      rm_col = rm_col + 1
    }
  }
  
}

#############################
## graph combined importance
#############################

data <- fimp_results
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
write.csv(results_matrix, file = paste0(file.path(results.dir, sub.dir), "\\", model.abv, "_results_matrix.csv"),row.names=FALSE, na="")
write.csv(fimp_results, file = paste0(file.path(results.dir, sub.dir), "\\", model.abv, "_feature_importance_matrix.csv"),row.names=FALSE, na="")

#################################
