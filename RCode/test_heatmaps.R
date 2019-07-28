#
# References
#
# https://sebastianraschka.com/Articles/heatmaps_in_r.html
# https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
# https://www.rdocumentation.org/packages/ggplot2/versions/0.9.1/topics/geom_tile
#

if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}

#########################################################
### B) Reading in data and transform it into matrix format
#########################################################

# data <- read.csv("../datasets/heatmaps_in_r.csv", comment.char="#")
# "D:\Projects\DropBox\Dropbox\Birkbeck\Project\PMResearchDatabase\MScProject\RCode\\

data <- read.csv(file="dt_feature_importance_20190727_162108_.csv", header=TRUE, sep=",")

rnames <- data[,1]                            # assign labels in column 1 to "rnames"
mat_data <- data.matrix(data[,2:ncol(data)])  # transform column 2-5 into a matrix
rownames(mat_data) <- rnames                  # assign row names


#########################################################
### C) Customizing and plotting the heat map
#########################################################

# creates a own color palette from red to green
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)

# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(-1,0,length=100),  # for red
               seq(0.01,0.8,length=100),           # for yellow
               seq(0.81,1,length=100))             # for green

col_breaks = c(seq(10,25,length=100),  # for red
               seq(1,9.999,length=100),           # for yellow
               seq(0,.999,length=100))             # for green

# creates a 5 x 5 inch image
png("heatmaps_in_r.png",    # create PNG for the heat map        
    width = 5*300,        # 5 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 8)        # smaller font size

heatmap.2(mat_data,
          cellnote = mat_data,  # same data set for cell labels
          main = "Correlation", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(12,9),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          # breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="row",     # only draw a row dendrogram
          Colv="NA")            # turn off column clustering

dev.off()               # close the PNG device

library(ggplot2)
library(plyr)
library(reshape2)
library(scales)
# Clear work space
rm(list = ls())

nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")

nba$Name <- with(nba, reorder(Name, PTS))

nba.m <- melt(nba)
nba.m <- ddply(nba.m, .(variable), transform, rescale = rescale(value))

(p <- ggplot(nba.m, aes(variable, Name)) + geom_tile(aes(fill = rescale), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue"))
# data
# aes = aesthtics
# geoms
#

# Add aesthetic mappings + Change scale
p + geom_tile(aes(fill=z)) + scale_fill_gradient(low="green", high="red")

########################################

library(ggplot2)

# Clear work space
rm(list = ls())

data_dt <- read.csv(file="dt_feature_importance_20190727_162108_.csv", header=TRUE, sep=",")
data_xg <- read.csv(file="xgb_feature_importance_20190727_162424_.csv", header=TRUE, sep=",")
data <- merge(data_dt, data_xg, by.x = "feature", by.y = "feature", suffixes = c("_dt","_xgb"))

# Order data by highest overall score, turn into a list, then delete zero values
# NB reorder converts feature into a feature from a string
#data$feature <- with(data, reorder(feature, ext + int1 + int2 + int3 + int3_s))

data$feature <- with(data, reorder(feature, ext_dt + ext_xgb))
data.m.ss <- subset(melt(data), value > 0 & (variable == "ext_dt" | variable == "ext_xgb"))

p <- ggplot(data.m.ss, aes(x=variable, y=feature)) 
p + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "green", high = "red")

data$feature <- with(data, reorder(feature, int1_dt + int1_xgb))
data.m.ss <-subset(melt(data), value > 0 & (variable == "int1_dt" | variable == "int1_xgb"))

p <- ggplot(data.m.ss, aes(x=variable, y=feature)) 
p + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "green", high = "red")

data$feature <- with(data, reorder(feature, int2_dt + int2_xgb))
data.m.ss <-subset(melt(data), value > 0 & (variable == "int2_dt" | variable == "int2_xgb"))

p <- ggplot(data.m.ss, aes(x=variable, y=feature)) 
p + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "green", high = "red")

data$feature <- with(data, reorder(feature, int3_dt + int3_xgb))
data.m.ss <-subset(melt(data), value > 0 & (variable == "int3_dt" | variable == "int3_xgb"))

p <- ggplot(data.m.ss, aes(x=variable, y=feature)) 
p + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "green", high = "red")

data$feature <- with(data, reorder(feature, int3_s_dt + int3_s_xgb))
data.m.ss <-subset(melt(data), value > 0 & (variable == "int3_s_dt" | variable == "int3_s_xgb"))

p <- ggplot(data.m.ss, aes(x=variable, y=feature)) 
p + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "green", high = "red")


