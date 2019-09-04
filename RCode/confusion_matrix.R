
# Load libraries
library(dplyr)
library(ggplot2) 
library(ggmosaic)
library(grid)
library(gridExtra)
library(gtable)
library(viridis)
library(rpart)
library(rpart.plot)
library(caret)
library(lubridate)
library(reshape2)

# Clear work space
rm(list = ls())

source("study_functions.R")

source.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/DataExtraction/CSVs"
results.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/Results"

study.prefix <- "run_15_"

now <- Sys.time()
sub.dir <- paste0(study.prefix,"20190903_1600")
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

# -- end on initialisation

model.names = c("Decision Tree","Random Forest","XGBoost")

file.text <- "_results_matrix"

num.runs <- 5

for(model.num in 1:length(model.list)) {
  
  model.abv <- model.list[model.num]
  model.name <- model.names[model.num]
  
  for(stage.num in 1:length(stage.list)) {
    
    stage.abv <- stage.list[stage.num]

    for(run.num in 1:num.runs) {
      
      file.suffix <- sprintf("_%02d", run.num)
      
      print(paste0(model.abv, "_confusion_matrix_",stage.abv, file.suffix,".png"))
      
      save_confusion_matrix_plot(model.abv, model.name, stage.abv, file.text, results.sub.dir, file.suffix)
        
    }      
  }
}

num.runs <- 1
model.num <- 1

for(model.num in 1:length(model.list)) {
  
  model.abv <- model.list[model.num]
  model.name <- model.names[model.num]
  
  p.list <- list()
  
  for(stage.num in 1:length(stage.list)) {
    
    stage.abv <- stage.list[stage.num]
    
    for(run.num in 1:num.runs) {
      
      file.suffix <- sprintf("_%02d", run.num)
      
      print(paste0(model.abv, "_confusion_matrix_",stage.abv, file.suffix,".png"))

      p.list[[stage.num]] <- plot_confusion_matrix_plot(model.abv, model.name, stage.abv, file.text, results.sub.dir, file.suffix)
      
    }      
  }
  
  g <- do.call(grid.arrange,p.list)
  
  ggsave(paste0(results.sub.dir, "/", model.abv, "_confusion_matrix_grid_",stage.abv, file.suffix,".png"),g)
  
}
