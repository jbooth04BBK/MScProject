
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


model.num <- 1
model.abv <- model.list[model.num]
run.num <- 1  
file.suffix <- sprintf("_%02d", run.num)
file.text <- "_results_matrix"
stage.num <- 1
stage.abv <- stage.list[stage.num]
  
file.name <- paste0(results.sub.dir, "/", model.abv, file.text, file.suffix, ".csv")

model.run.results <- read.csv(file = paste0(results.sub.dir, "/", model.abv, file.text, file.suffix, ".csv"), header=TRUE, sep=",")

# Change run into a factor
model.run.results$run <- factor(model.run.results$run)

# Lets have some tables
# add columns for c001 accuracy and c002 accuracy
# c001 accuracy = cm_r1_c1/(cm_r1_c1 + cm_r1_c2)
model.run.results$c001_accuracy <- with(model.run.results, cm_r1_c1 / (cm_r1_c1 + cm_r1_c2))
# c002 accuracy = cm_r2_c2/(cm_r2_c1 + cm_r2_c2)
model.run.results$c002_accuracy <- with(model.run.results, cm_r2_c2 / (cm_r2_c1 + cm_r2_c2))

accuracy.string <- paste0(sprintf("%.2f", (subset(model.run.results, stage == stage.abv)$accuracy*100)),"%")
c001.accuracy.string <- paste0(sprintf("%.2f", (subset(model.run.results, stage == stage.abv)$c001_accuracy*100)),"%")
c002.accuracy.string <- paste0(sprintf("%.2f", (subset(model.run.results, stage == stage.abv)$c002_accuracy*100)),"%")

# cols = models
# rows = stage
# values = mean(accuracy)

output.matrix = matrix(nrow = 4, ncol = 4)
colnames(output.matrix) <- c("Actual","Predicted","Value","Label")
#rownames(output.matrix) <- c("C001","C002")

output.matrix[1,1] <- "C001"
output.matrix[1,2] <- "C001"
output.matrix[1,3] <- subset(model.run.results, stage == stage.abv)$cm_r1_c1
output.matrix[1,4] <- paste0(output.matrix[1,3]," (",c001.accuracy.string,")")
output.matrix[2,1] <- "C001"
output.matrix[2,2] <- "C002"
output.matrix[2,3] <- subset(model.run.results, stage == stage.abv)$cm_r1_c2
output.matrix[2,4] <- output.matrix[2,3]
output.matrix[3,1] <- "C002"
output.matrix[3,2] <- "C001"
output.matrix[3,3] <- subset(model.run.results, stage == stage.abv)$cm_r2_c1
output.matrix[3,4] <- output.matrix[3,3]
output.matrix[4,1] <- "C002"
output.matrix[4,2] <- "C002"
output.matrix[4,3] <- subset(model.run.results, stage == stage.abv)$cm_r2_c2
output.matrix[4,4] <- paste0(output.matrix[4,3]," (",c002.accuracy.string,")")

output.df <- data.frame(output.matrix)
output.df$Value <- strtoi(output.df$Value)

plot.title = paste0("Confusion Matrix, Stage: ",stage.abv,", Accuracy = ",accuracy.string)
p <- ggplot(output.df, aes(x=Predicted, y=Actual)) 
p <- p + geom_tile(aes(fill = Value))
p <- p + scale_fill_viridis_c(direction = -1)
# p <- p + geom_text(aes(label = round(Value, 1)))
p <- p + xlab("Predicted")
p <- p + ggtitle(plot.title)
p <- p + theme_classic()
p <- p + geom_label(aes(label = Label))

print(p)

# ggsave(paste0(results.sub.dir, "/", "accuracy_table",".png"),table)
