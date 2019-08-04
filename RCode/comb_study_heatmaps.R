#
# References
#
# https://sebastianraschka.com/Articles/heatmaps_in_r.html
# https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
# https://www.rdocumentation.org/packages/ggplot2/versions/0.9.1/topics/geom_tile
#

########################################

library(ggplot2)
library(lubridate)
library(reshape2)

# Clear work space
rm(list = ls())

now = Sys.time()

create_save_plot <- function(plot_stage, plot_data) {
  
  plot_title = paste0("Combined Feature Importance Heatmap - Stage: ",plot_stage)
  p <- ggplot(plot_data, aes(x=variable, y=feature)) 
  p <- p + ggtitle(plot_title)
  p <- p + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "green", high = "red")
  # p <- p + theme(axis.text=element_text(size=6))
  
  print(p)
  
  ggsave(paste0(latest_subfolder, "/", "combined_feature_importance_", plot_stage, "_hm.png"))
  
}

importance.min <- 2.0

results.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/Results"
dirs <- file.info(list.dirs(path = results.dir, full.names = TRUE))
latest_subfolder <- rownames(dirs)[which.max(dirs$mtime)]

model.abv = "dt"
data_dt <- read.csv(file=paste0(latest_subfolder, "/", model.abv, "_feature_importance_matrix.csv"), header=TRUE, sep=",")

model.abv = "rf"
data_rf <- read.csv(file=paste0(latest_subfolder, "/", model.abv, "_feature_importance_matrix.csv"), header=TRUE, sep=",")

model.abv = "xgb"
data_xg <- read.csv(file=paste0(latest_subfolder, "/", model.abv, "_feature_importance_matrix.csv"), header=TRUE, sep=",")

data <- merge(data_dt, data_rf, by.x = "feature", by.y = "feature", suffixes = c("_dt","_rf"))

data <- merge(data, data_xg, by.x = "feature", by.y = "feature", suffixes = c("_xgb"))

# Order data by highest overall score, turn into a list, then delete zero values
# NB reorder converts feature into a feature from a string
#data$feature <- with(data, reorder(feature, ext + int1 + int2 + int3 + int3_s))

data$feature <- with(data, reorder(feature, ext_dt + ext_rf + ext))
data.m.ss <- subset(melt(data), value > importance.min & (variable == "ext_dt" | variable == "ext_rf" | variable == "ext"))

stage <- "ext"
create_save_plot(stage, data.m.ss)

data$feature <- with(data, reorder(feature, int1_dt + int1_rf + int1))
data.m.ss <-subset(melt(data), value > importance.min & (variable == "int1_dt" | variable == "int1_rf" | variable == "int1"))

stage <- "int1"
create_save_plot(stage, data.m.ss)

data$feature <- with(data, reorder(feature, int2_dt + int2_rf + int2))
data.m.ss <-subset(melt(data), value > importance.min & (variable == "int2_dt" | variable == "int2_rf" | variable == "int2"))

stage <- "int2"
create_save_plot(stage, data.m.ss)

data$feature <- with(data, reorder(feature, int3_dt + int3_rf + int3))
data.m.ss <-subset(melt(data), value > importance.min & (variable == "int3_dt" | variable == "int3_rf" | variable == "int3"))

stage <- "int3"
create_save_plot(stage, data.m.ss)

data$feature <- with(data, reorder(feature, int3_s_dt + int3_s_rf + int3_s))
data.m.ss <-subset(melt(data), value > importance.min & (variable == "int3_s_dt" | variable == "int3_s_rf" | variable == "int3_s"))

stage <- "int_3_s"
create_save_plot(stage, data.m.ss)

#################################
