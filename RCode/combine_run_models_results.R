
# Load libraries
library(dplyr)
library(ggplot2) 
library(rpart)
library(rpart.plot)
library(caret)
library(lubridate)
library(reshape2)
# Random Forest added:
library(randomForest)
library(e1071)
# xgboost added:
library(xgboost)
library('DiagrammeR') # NB installed package
library('rsvg') # NB installed package
library('DiagrammeRsvg') # NB installed package

# Clear work space
rm(list = ls())

source("study_functions.R")

source.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/DataExtraction/CSVs"
results.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/Results"

study.prefix <- "run_10_"

# now <- Sys.time()
# sub.dir <- paste0(study.prefix,format(now, "%Y%m%d_%H%M"))
sub.dir <- 'run_10_20190812_0627'
results.sub.dir <- file.path(results.dir, sub.dir)

model.list = c("dt","rf","xgb")
stage.list = c("ext","int1","int2","int3")
num.runs <- 5

# combine results for this study
# defining the function

#######################################
# Compare accuracy by random seed (run)
#######################################

comb.results <- data.frame()

for(model.num in 1:length(model.list)) {
  
  model.abv <- model.list[model.num]
  
  mergeCSV("df.results", model.abv,"_results_matrix", results.sub.dir, num.runs)
  
  comb.results <- rbind(comb.results,df.results[,c("model",'run','run_time','rdv_type', 'run_seed', 'stage', 'observations','train_cod2_01', 'train_cod2_02', 'test_cod2_01', 'test_cod2_02','accuracy','cm_r1_c1','cm_r1_c2','cm_r2_c1','cm_r2_c2')])
  
}

# Change run into a factor
comb.results$run <- factor(comb.results$run)

write.csv(comb.results, file = paste0(results.sub.dir, "/comb_results_matrix_all", ".csv"),row.names=FALSE, na="")

################################################
# Compare feature Importance by random seed (run)
################################################

# dt_feature_importance_matrix_02

comb.results <- data.frame()

for(model.num in 1:length(model.list)) {
  
  model.abv <- model.list[model.num]
  
  mergeCSV("df.results", model.abv,"_feature_importance_matrix", results.sub.dir, num.runs)

  comb.results <- rbind(comb.results,df.results[,c("model","run","feature","ext","int1","int2","int3")])
  
}

write.csv(comb.results, file = paste0(results.sub.dir, "/comb_feature_importance_matrix_all", ".csv"),row.names=FALSE, na="")

# we want to see how feature importance changes with random seed.
# NB What we have already is by random seed (Run)
#
# Variable = Stage, value = relative importance
#
# x = run, y = feature, z = value
# group by stage (4) or model (3)?

comb.results$feature <- with(comb.results, reorder(feature, ext + int1 + int2 + int3))

importance.min <- 1.5

for(stage.num in 1:length(stage.list)) {
  
  stage <- stage.list[stage.num]

  data.m.ss <- melt(comb.results, id=c("model", "run", "feature"))
  data.m.ss <- subset(data.m.ss, value > importance.min)
  data.m.ss <- subset(data.m.ss, variable == stage)
  
  plot.title = paste0("Compare Feature Importance - stage: ", stage)
  
  p <- ggplot(data.m.ss, aes(x=run, y=feature)) 
  p <- p + ggtitle(plot.title)
  p <- p + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "green", high = "red")
  p <- p + geom_text(aes(label = round(value, 1)), size = 3)
  p <- p + facet_grid(cols = vars(model))
  
  print(p)
  
  ggsave(paste0(results.sub.dir, "/", "compare_feature_importance_stage_", stage, ".png"))

}

for(run.num in 1:num.runs) {
  
  run.str <- sprintf("%02d", run.num)
  
  data.m.ss <- melt(comb.results, id=c("model", "run", "feature"))
  data.m.ss <- subset(data.m.ss, value > importance.min)
  data.m.ss <- subset(data.m.ss, run == run.num)
  
  plot.title = paste0("Compare Feature Importance - run: ", run.str)
  
  p <- ggplot(data.m.ss, aes(x=model, y=feature)) 
  p <- p + ggtitle(plot.title)
  p <- p + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "green", high = "red")
  p <- p + geom_text(aes(label = round(value, 1)), size = 2)
  p <- p + facet_grid(cols = vars(variable))
  
  print(p)
  
  ggsave(paste0(results.sub.dir, "/", "compare_feature_importance_run_", run.str, ".png"))
  
}

