
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

model.abv = "xgb"
model.name = "XGBoost"
file.text <- "comb_feature_importance_matrix_all"

file.name <- paste0(results.sub.dir, "/", file.text, ".csv")

model.run.results <- read.csv(file = file.name, header=TRUE, sep=",")

# Only want XGB values
model.run.results <- subset(model.run.results, model == model.abv)

# remove all zero values
model.run.results <- subset(model.run.results, ext != 0.00 | int1 != 0.00 | int2 != 0.00 | int3 != 0.00)

for(stage.num in 1:length(stage.list)) {
  
  stage <- stage.list[stage.num]
  
  # model.stage.mean <- aggregate(model.stage.results[, 4], list(model.stage.results$model,model.stage.results$feature), mean)
  model.stage.mean <- aggregate(
                                formula = model.stage.results[,(3 + stage.num)] ~ feature,
                                FUN = mean,
                                data = model.stage.results
                                )
  
  colnames(model.stage.mean) <- c("feature", "value")
  
  model.stage.mean <- subset(model.stage.mean, value >= importance.min)
  
  # Visualization NAs
  p <- ggplot(model.stage.mean, aes(x = reorder(feature,value), y = value, fill = value)) 
  p <- p + geom_bar(stat="identity", width=0.5)
  p <- p + scale_fill_viridis(direction = -1) 
  p <- p + xlab("Features")
  p <- p + ylab("Relative Importance")
  p <- p + ggtitle(paste0("Relative Feature Importance, stage: ",stage))
  p <- p + coord_flip()
  p <- p + theme_classic()
  
  print(p)

  ggsave(paste0(results.sub.dir, "/", model.abv, "_feature_importance_mean_", stage, ".png"))
  
}



