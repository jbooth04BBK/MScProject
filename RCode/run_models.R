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

study.prefix <- "run_09_"

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
  
  # RunRFModel(run.seed, rdv.type, importance.min, source.dir, results.sub.dir)
  
  # RunXGBModel(run.seed, rdv.type, importance.min, source.dir, results.sub.dir)
  
}

# combine results for this study
# defining the function

model.abv = "dt"
mergeCSV("df.results", model.abv,"_results_matrix", results.sub.dir, num.runs)

# Visualization
p <- ggplot(df.results, aes(x = run, y = accuracy)) 
p <- p + geom_line(aes(color = stage))
p <- p + ylim(0, 1)
p <- p + ggtitle("Change of Accuracy by Run")
# p <- p + scale_color_manual(values = c("darkred", "steelblue"))
print(p)
