
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

study.prefix <- "run_10_"

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

importance.min <- 1.0
num.runs <- 5

# Each run will have it's own sufix and random seed
for(run.num in 1:num.runs) {
  
  file.suffix <- sprintf("_%02d", run.num)
  
  now <- Sys.time()
  run.seed <- as.integer((second(now) - as.integer(second(now))) * 1000)
  
  RunDTModel(run.seed, rdv.type, importance.min, source.dir, results.sub.dir, file.suffix)
  
  RunRFModel(run.seed, rdv.type, importance.min, source.dir, results.sub.dir, file.suffix)
  
  RunXGBModel(run.seed, rdv.type, importance.min, source.dir, results.sub.dir, file.suffix)
  
}

# combine results for this study
# defining the function

#######################################
# Compare accuracy by random seed (run)
#######################################

comb.results <- data.frame()

model.abv = "dt"
mergeCSV("df.results", model.abv,"_results_matrix", results.sub.dir, num.runs)

comb.results <- rbind(comb.results,df.results[,c("model","run","stage","accuracy")])

model.abv = "rf"
mergeCSV("df.results", model.abv,"_results_matrix", results.sub.dir, num.runs)

comb.results <- rbind(comb.results,df.results[,c("model","run","stage","accuracy")])

model.abv = "xgb"
mergeCSV("df.results", model.abv,"_results_matrix", results.sub.dir, num.runs)

comb.results <- rbind(comb.results,df.results[,c("model","run","stage","accuracy")])

# Change run into a factor
comb.results$run <- factor(comb.results$run)

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

model.abv = "dt"
mergeCSV("df.results", model.abv,"_feature_importance_matrix", results.sub.dir, num.runs)

comb.results <- rbind(comb.results,df.results[,c("model","run","feature","ext","int1","int2","int3")])

model.abv = "rf"
mergeCSV("df.results", model.abv,"_feature_importance_matrix", results.sub.dir, num.runs)

comb.results <- rbind(comb.results,df.results[,c("model","run","feature","ext","int1","int2","int3")])

model.abv = "xgb"
mergeCSV("df.results", model.abv,"_feature_importance_matrix", results.sub.dir, num.runs)

comb.results <- rbind(comb.results,df.results[,c("model","run","feature","ext","int1","int2","int3")])

# we want to see how feature importance changes with random seed.
# NB What we have already is by random seed (Run)
#
# Variable = Stage, value = relative importance
#
# x = run, y = feature, z = value
# group by stage (4) or model (3)?

comb.results$feature <- with(comb.results, reorder(feature, ext + int1 + int2 + int3))

num.stages <- 4

for(stage.num in 1:num.stages) {
  
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
  
  ggsave(paste0(results.sub.dir, "/", "compare_feature_importance_", stage, ".png"))

}

num.models <- 3

for(model.num in 1:num.models) {
  
  rm.col <- 1
  
  if (model.num == 1) { 
    model.abv = "dt"
  } else if (model.num == 2) {
    model.abv = "rf"
  } else {
    model.abv = "xgb"
  }
  
  data.m.ss <- melt(comb.results, id=c("model", "run", "feature"))
  data.m.ss <- subset(data.m.ss, value > importance.min)
  data.m.ss <- subset(data.m.ss, model == model.abv)
  
  plot.title = paste0("Compare Feature Importance - Model: ", model.abv)
  
  p <- ggplot(data.m.ss, aes(x=variable, y=feature)) 
  p <- p + ggtitle(plot.title)
  p <- p + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "green", high = "red")
  p <- p + geom_text(aes(label = round(value, 1)), size = 2)
  p <- p + facet_grid(cols = vars(run))
  
  print(p)
  
  ggsave(paste0(results.sub.dir, "/", "compare_feature_importance_", model.abv, ".png"))

}
