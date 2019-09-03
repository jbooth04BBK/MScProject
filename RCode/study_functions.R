#
# Functions used by all Studies
#

return_clean_rdvdata <- function(source.dir, stage, rdv.type) {
  
  RDVData <- read.csv(file=paste0(source.dir, "\\rdv_study_", stage, rdv.type, ".csv"), header=TRUE, sep=",")
  
  #Remove unwanted columns - gestation_at_delivery_in_days and thyroid_weight
  if (stage == "ext") { 
    clean_RDVData <- RDVData %>%
      select(-c(event_id, event_start_date, age_category, case_id, include_in_study, gestation_at_delivery_in_days, foot_length, crown_rump_length)) %>%
      na.omit()
  } else {
    clean_RDVData <- RDVData %>%
      select(-c(event_id, event_start_date, age_category, case_id, include_in_study, gestation_at_delivery_in_days, foot_length, crown_rump_length, thyroid_weight)) %>%
      na.omit()
  }  
  
  return(clean_RDVData)
  
}

# create_train_test(df, size = 0.8, train = TRUE)
# arguments:
#  -df: Dataset used to train the model.
#  -size: Size of the split. By default, 0.8. Numerical value
#  -train: If set to `TRUE`, the function creates the train set, otherwise the test set. Default value sets to `TRUE`. Boolean value.
#   You need to add a Boolean parameter because R does not allow to return two data frames simultaneously.

create_train_test <- function(data, size = 0.8, train = TRUE) {
  
  n_row = nrow(data)
  # total_row = size * n_row
  # train_sample <- 1: total_row
  
  train.sample = sample(n_row,floor(size * n_row))

  if (train == TRUE) {
    return (data[train.sample, ])
  } else {
    return (data[-train.sample, ])
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

accuracy_fit <- function(fit, data_test, type.str = "class") {
  predict_unseen <- predict(fit, data_test, type = type.str)
  table_mat <- table(data_test$cod2_summ, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

cmatrix_fit <- function(fit, data_test, type.str = "class") {
  predict_unseen <- predict(fit, data_test, type = type.str)
  table_mat <- table(data_test$cod2_summ, predict_unseen)
  table_mat
}

setup.fimp.matrix <- function(rdv.type, source.dir, run.str) {
  
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
  
  run.col = replicate(length(cn),run.str)
  col_values= replicate(length(cn),0.0)
  
  # create an empty data frame
  column_names <- c("run","feature","ext","int1","int2","int3","int3_s")
  fimp.matrix <- data.frame(run.col, cn, col_values, col_values, col_values, col_values, col_values)
  colnames(fimp.matrix) <- column_names
  
  return(fimp.matrix)
  
}

setup.results.matrix <- function(model.abv, num.stages) {
  
  ######################################
  # Create matrix to store results
  ######################################

  if (model.abv == "dt") {
    column_names = c('run','run_time','rdv_type', 'run_seed', 'stage', 'observations','train_cod2_01', 'train_cod2_02', 'test_cod2_01', 'test_cod2_02', 'max_accuracy','minsplit','maxdepth','accuracy','cm_r1_c1','cm_r1_c2','cm_r2_c1','cm_r2_c2')
  } else if (model.abv == "rf") {
    column_names = c('run','run_time','rdv_type', 'run_seed', 'stage', 'observations','train_cod2_01', 'train_cod2_02', 'test_cod2_01', 'test_cod2_02','best_def_mtry','bmtd_accuracy','best_mtry','bmt_accuracy','best_maxnodes','bmn_accuracy','best_ntree','bnt_accuracy','max_accuracy','accuracy','cm_r1_c1','cm_r1_c2','cm_r2_c1','cm_r2_c2')
  } else {
    column_names = c('run','run_time','rdv_type', 'run_seed', 'stage', 'observations','train_cod2_01', 'train_cod2_02', 'test_cod2_01', 'test_cod2_02','eta','max_depth','gamma','min_child_depth','subsample','colsample_bytree','accuracy','cm_r1_c1','cm_r1_c2','cm_r2_c1','cm_r2_c2')
  }
  
  results.matrix = matrix(nrow = num.stages, ncol = length(column_names))
  colnames(results.matrix) <- column_names
  
  return(results.matrix)
  
}

mergeCSV <- function(df.name, model.abv, file.text, results.sub.dir, max.run = 1) {
  
  df <- data.frame()
  
  for(run.num in 1:max.run){
    
    file.suffix <- sprintf("_%02d", run.num)
    
    add <- read.csv(file = paste0(results.sub.dir, "/", model.abv, file.text, file.suffix, ".csv"), header=TRUE, sep=",")
    
    df <- rbind(df,add)
  }
  
  df$model <- replicate(nrow(df),model.abv)

  assign(df.name, df, envir = .GlobalEnv)
  
}

summary_accuracy_table <- function(model.lst, stage.list, col.num, title1.txt, title2.txt) {
  
  means.matrix = matrix(nrow = length(stage.list), ncol = length(model.list))
  colnames(means.matrix) <- model.list
  rownames(means.matrix) <- stage.list
  
  for(stage.num in 1:length(stage.list)) {
    
    stage.abv <- stage.list[stage.num]
    
    for(model.num in 1:length(model.list)) {
      
      model.abv <- model.list[model.num]
      
      if (col.num == 1){
        means.matrix[stage.num,model.num] <- sprintf("%.2f %%",100 *mean(subset(comb.results, model == model.abv & stage == stage.abv)$accuracy))
      } else if (col.num == 2) {
        means.matrix[stage.num,model.num] <- sprintf("%.2f %%",100 *mean(subset(comb.results, model == model.abv & stage == stage.abv)$c001_accuracy))
      } else if (col.num == 3) {
        means.matrix[stage.num,model.num] <- sprintf("%.2f %%",100 *mean(subset(comb.results, model == model.abv & stage == stage.abv)$c002_accuracy))
      }
    }
    
  }
  
  table.grob <- tableGrob(data.frame(means.matrix), cols = c("Decision Tree", "Random Forest", "XGBoost"))
  title1.grob <- textGrob(title1.txt,just = "centre" )
  title2.grob <- textGrob(title2.txt,just = "centre" )
  padding <- unit(5,"mm")
  
  table.comb <- gtable(unit(10, c("cm")), unit(c(0.5,0.5,4), "cm"))
  table.comb <- gtable_add_grob(table.comb, title1.grob, 1, 1)
  table.comb <- gtable_add_grob(table.comb, title2.grob, 2, 1)
  table.comb <- gtable_add_grob(table.comb, table.grob, 3, 1)
  
  return(table.comb)
  
}

