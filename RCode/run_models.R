
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
source("dtree_study.R")
source("rforest_study.R")
source("GBoost_study.R")

source.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/DataExtraction/CSVs"
results.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/Results"

study.prefix <- "run_12_"

now <- Sys.time()
sub.dir <- paste0(study.prefix,format(now, "%Y%m%d_%H%M"))
results.sub.dir <- file.path(results.dir, sub.dir)

if (!dir.exists(results.sub.dir)) {
  dir.create(results.sub.dir)
}

# Adjusted data or not for this study
data.adjusted <- TRUE
if (data.adjusted) {
  rdv.type = "_adj"
} else {
  rdv.type = ""
}

importance.min <- 1.5

model.list = c("dt","rf","xgb")
stage.list = c("ext","int1","int2","int3")

num.runs <- 5

# Each run will have it's own sufix and random seed
for(run.num in 1:num.runs) {
  
  file.suffix <- sprintf("_%02d", run.num)
  
  now <- Sys.time()
  run.seed <- as.integer((second(now) - as.integer(second(now))) * 1000)
  
  # Create training index to be used by all models.
  # have to create a train.index for each stage ext.train.index, etc
  # if comparing models has to be on the same data
  
  stage = "ext"
  clean_RDVData <- return_clean_rdvdata(source.dir, stage, rdv.type)
  ext.train.index = sample(nrow(clean_RDVData),floor(0.80 * nrow(clean_RDVData)))
  
  stage = "int1"
  clean_RDVData <- return_clean_rdvdata(source.dir, stage, rdv.type)
  int1.train.index = sample(nrow(clean_RDVData),floor(0.80 * nrow(clean_RDVData)))
  
  stage = "int2"
  clean_RDVData <- return_clean_rdvdata(source.dir, stage, rdv.type)
  int2.train.index = sample(nrow(clean_RDVData),floor(0.80 * nrow(clean_RDVData)))
  
  stage = "int3"
  clean_RDVData <- return_clean_rdvdata(source.dir, stage, rdv.type)
  int3.train.index = sample(nrow(clean_RDVData),floor(0.80 * nrow(clean_RDVData)))

  RunDTModel(
    run.seed, 
    rdv.type, 
    importance.min, 
    source.dir, 
    results.sub.dir, 
    file.suffix, 
    stage.list,
    ext.train.index,
    int1.train.index,
    int2.train.index,
    int3.train.index
    )
  
  RunRFModel(run.seed, 
             rdv.type, 
             importance.min, 
             source.dir, 
             results.sub.dir, 
             file.suffix,
             stage.list,
             ext.train.index,
             int1.train.index,
             int2.train.index,
             int3.train.index
             )
  
  RunXGBModel(run.seed, 
              rdv.type, 
              importance.min, 
              source.dir, 
              results.sub.dir, 
              file.suffix, 
              stage.list,
              ext.train.index,
              int1.train.index,
              int2.train.index,
              int3.train.index
              )
  
}

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

# Visualization
p <- ggplot(comb.results, aes(x = run, y = accuracy, group = stage)) 
p <- p + geom_line(aes(color = stage), size=2)
p <- p + ylim(0.4, 1.0)
p <- p + ggtitle(paste0("Change of Accuracy by Run, by Model "))
p <- p + facet_grid(rows = vars(model))
# p <- p + scale_color_manual(values = c("darkred", "steelblue"))

print(p)

ggsave(paste0(results.sub.dir, "/", "comb_accuracy_run_model",".png"))

# sort comb.results - not needed, just added group = model
# comb.results <- comb.results[with(comb.results, order(run, model, stage)), ]

# Visualization
p <- ggplot(comb.results, aes(x = stage, y = accuracy, group = model)) 
p <- p + geom_line(aes(color = model), size=2)
p <- p + ylim(0.4, 1.0)
p <- p + ggtitle(paste0("Change of Accuracy by Model, by Run "))
p <- p + facet_grid(rows = vars(run))

print(p)

ggsave(paste0(results.sub.dir, "/", "comb_accuracy_model_run",".png"))

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

