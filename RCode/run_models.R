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
source("dtree_study.R")

now <- Sys.time()
run.seed <- as.integer((second(now) - as.integer(second(now))) * 1000)

# Adjusted data or not
data.adjusted <- TRUE
if (data.adjusted) {
  rdv.type = "_adj"
} else {
  rdv.type = ""
}

source.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/DataExtraction/CSVs"
results.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/Results"
sub.dir <- format(now, "%Y%m%d_%H%M")
results.sub.dir <- file.path(results.dir, sub.dir)

if (!dir.exists(results.sub.dir)) {
  dir.create(results.sub.dir)
}

RunDTModel(run.seed, rdv.type, source.dir, results.sub.dir)
