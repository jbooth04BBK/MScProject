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

# Clear work space
rm(list = ls())

now = Sys.time()

create_save_plot <- function(plot_stage, plot_data) {
  plot_title = paste0("Combined Feature Importance Heatmap: Stage = ",plot_stage)
  p <- ggplot(plot_data, aes(x=variable, y=feature)) 
  p <- p + ggtitle(plot_title)
  p <- p + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "green", high = "red")
  
  print(p)
  
  file_name <- paste0("comb_study_hm_",plot_stage,"_",format(now, "%Y%m%d_%H%M%S"),".png")
  ggsave(file_name)
}

data_dt <- read.csv(file="dt_feature_importance_20190728_152426_.csv", header=TRUE, sep=",")
data_xg <- read.csv(file="xgb_feature_importance_20190728_153030_.csv", header=TRUE, sep=",")
data <- merge(data_dt, data_xg, by.x = "feature", by.y = "feature", suffixes = c("_dt","_xgb"))

# Order data by highest overall score, turn into a list, then delete zero values
# NB reorder converts feature into a feature from a string
#data$feature <- with(data, reorder(feature, ext + int1 + int2 + int3 + int3_s))

data$feature <- with(data, reorder(feature, ext_dt + ext_xgb))
data.m.ss <- subset(melt(data), value > 0 & (variable == "ext_dt" | variable == "ext_xgb"))

stage <- "ext"
create_save_plot(stage, data.m.ss)

data$feature <- with(data, reorder(feature, int1_dt + int1_xgb))
data.m.ss <-subset(melt(data), value > 0 & (variable == "int1_dt" | variable == "int1_xgb"))

stage <- "int1"
create_save_plot(stage, data.m.ss)

data$feature <- with(data, reorder(feature, int2_dt + int2_xgb))
data.m.ss <-subset(melt(data), value > 0 & (variable == "int2_dt" | variable == "int2_xgb"))

stage <- "int2"
create_save_plot(stage, data.m.ss)

data$feature <- with(data, reorder(feature, int3_dt + int3_xgb))
data.m.ss <-subset(melt(data), value > 0 & (variable == "int3_dt" | variable == "int3_xgb"))

stage <- "int3"
create_save_plot(stage, data.m.ss)

data$feature <- with(data, reorder(feature, int3_s_dt + int3_s_xgb))
data.m.ss <-subset(melt(data), value > 0 & (variable == "int3_s_dt" | variable == "int3_s_xgb"))

stage <- "int_3_s"
create_save_plot(stage, data.m.ss)

#################################