
#
# Multiple plots on a page
# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
#
# Mosaic plots
# https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html
#
# colour schemes - color blind
# https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
#

library(dplyr)
library(ggplot2) 
library(ggmosaic)
library(gridExtra)
library(viridis)
library(reshape2)

# Clear work space
rm(list = ls())

source("study_functions.R")

source.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/DataExtraction/CSVs"
results.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/Results"
study.prefix <- "run_15_"
sub.dir <- paste0(study.prefix,"20190903_1016")
results.sub.dir <- file.path(results.dir, sub.dir)

# rdv_demo_selection_02

RDVData <- read.csv(file=paste0(source.dir, "\\rdv_demo_selection_02",  ".csv"), header=TRUE, sep=",")

summary(RDVData)
str(RDVData)

# Visualization cod2_summ
p <- ggplot(RDVData, aes(x = cod2_summ, fill = sex)) 
p <- p + geom_bar()
p <- p + scale_fill_viridis(discrete = TRUE, name = "Sex", labels = c("Female", "Male", "Unknown")) 
p <- p + scale_x_discrete(labels = c('No','Yes','Unknown', 'N/A'))
p <- p + xlab("Cause of Death - Determined")
p <- p + ylab("Number of Cases")
p <- p + ggtitle("Cause of Death Determined split by Sex")
p <- p + theme_classic()

print(p)
p1 <- p
 
# Visualization cod2_summ - stacked bar
# p <- ggplot(RDVData, aes(x = cod2_summ, fill = age_category)) 
# p <- p + geom_bar(position=position_dodge())
# p <- p + scale_x_discrete(labels = c('No','Yes','Unknown', 'N/A'))
# p <- p + scale_fill_viridis(discrete = TRUE, name = "Age Catgeory", labels = c("Miscarriage", "Still Birth", "Early Neonatal", "Neonatal","Infant Death","Child Death","N/A"))
# p <- p + xlab("Cause of Death - Determined")
# p <- p + ggtitle("Cause of Death Determined by Age Category")
# p <- p + theme_classic()

p <- ggplot(data = RDVData) + 
  geom_mosaic(aes(weight = 1, x = product(cod2_summ), fill = age_category),na.rm = TRUE) + 
  scale_fill_viridis(discrete = TRUE, 
                     labels = c("Miscarriage", "Still Birth", "Early Neonatal", "Neonatal","Infant Death","Child Death","N/A"),
                     guide = guide_legend(reverse = TRUE))  +
  scale_x_productlist(labels = c('No','Yes','Unknown', 'N/A')) +
  scale_y_productlist(labels = NULL) +
  xlab("Cause of Death - Determined") +
  ggtitle("Cause of Death Determined by Age Category") + 
  theme_classic() + 
  theme(axis.text.x=element_text(angle=-25, hjust= .1),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()
  )

# Add number labels to plot
p <- p +
  geom_label(data = ggplot_build(p)$data %>% as.data.frame() %>% filter(.wt > 0),
             aes(x = (xmin + xmax)/2,
                 y = (ymin + ymax)/2,
                 label = .wt),
             size = 2)

print(p)
p2 <- p

# Visualization cod2_summ
p <- ggplot(RDVData, aes(x = number_of_attributes, fill = cod2_summ)) 
p <- p + geom_histogram(bins = 50)
p <- p + scale_fill_viridis(discrete = TRUE, name = "Cause of Death\nDetermined", labels = c("No", "Yes", "Unknown", "N/A"))
p <- p + xlab("Number of Attributes")
p <- p + ylab("Number of Cases")
p <- p + ggtitle("Number of Attributes")
p <- p + theme_classic()

print(p)
p3 <- p

# Visualization cod2_summ
p <- ggplot(RDVData, aes(x = year, fill = cod2_summ)) 
p <- p + geom_histogram(bins = 50)
p <- p + scale_fill_viridis(discrete = TRUE, name = "Cause of Death\nDetermined", labels = c("No", "Yes", "Unknown", "N/A"))
p <- p + ylab("Number of Cases")
p <- p + ggtitle("Year")
p <- p + theme_classic()

print(p)
p4 <- p

g <- grid.arrange(p1, p2, p3, p4, nrow = 2)

ggsave(paste0(results.sub.dir, "/", "all_data_vis_grid",".png"),g)

# Include in study
# Include in study
# Exclude - COD
# Exclude - Age
# Exclude - Incorrect Measurement
# Exclude - Missing value


# Show the influence of Include in study
p <- ggplot(RDVData, aes(x = include_in_study, fill = include_in_study)) 
p <- p + geom_bar()
p <- p + scale_fill_viridis(discrete = TRUE, name = "Include in Study", labels = c("Include", "Exc - COD", "Exc - Age", "Exc - Measure"))
p <- p + scale_x_discrete(labels = c("Include", "Exc - COD", "Exc - Age", "Exc - Measure"))
p <- p + xlab("Include in Study")
p <- p + ylab("Number of Cases")
p <- p + ggtitle("Include in Study - Breakdown")
p <- p + theme_classic()

print(p)
p1 <- p

# Summarise inc_in_study to 2 variables Yes or No
RDVData$summary_iis <- RDVData$include_in_study
RDVData$summary_iis[RDVData$include_in_study=="C003"] <- "C002"
RDVData$summary_iis[RDVData$include_in_study=="C004"] <- "C002"

summary(RDVData$summary_iis)

# Visualization include in study
p <- ggplot(RDVData, aes(x = summary_iis, fill = sex)) 
p <- p + geom_bar()
p <- p + scale_x_discrete(labels = c("Include", "Exclude"))
p <- p + scale_fill_viridis(discrete = TRUE, name = "Sex", labels = c("Female", "Male", "Unknown"))
p <- p + xlab("Include in Study")
p <- p + ylab("Number of Cases")
p <- p + ggtitle("Include in Study split by Sex")
p <- p + theme_classic()

print(p)
p2 <- p

# # Visualization include in study
# p <- ggplot(RDVData, aes(x = summary_iis, fill = age_category)) 
# p <- p + geom_bar(position=position_dodge())
# p <- p + scale_x_discrete(labels = c("Include", "Exclude"))
# p <- p + scale_fill_viridis(discrete = TRUE, name = "Age Catgeory", labels = c("Miscarriage", "Still Birth", "Early Neonatal", "Neonatal","Infant Death","Child Death","N/A"))
# p <- p + xlab("Include in Study")
# p <- p + ggtitle("Include in Study split by Age Category")
# p <- p + theme_classic()

p <- ggplot(data = RDVData) + 
  geom_mosaic(aes(weight = 1, x = product(summary_iis), fill = age_category),na.rm = TRUE) + 
  scale_fill_viridis(discrete = TRUE, 
                     labels = c("Miscarriage", "Still Birth", "Early Neonatal", "Neonatal","Infant Death","Child Death","N/A"),
                     guide = guide_legend(reverse = TRUE))  +
  scale_x_productlist(labels = c('Include','Exclude','','')) +
  scale_y_productlist(labels = NULL) +
  xlab("Include in study") +
  ggtitle("Include in Study split by Age Category") + 
  theme_classic() + 
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank()
  )

# Add number labels to plot
p <- p +
  geom_label(data = ggplot_build(p)$data %>% as.data.frame() %>% filter(.wt > 0),
             aes(x = (xmin + xmax)/2,
                 y = (ymin + ymax)/2,
                 label = .wt),
             size = 3)

print(p)
p3 <- p

# Visualization cod2_summ
p <- ggplot(RDVData, aes(x = number_of_attributes, fill = summary_iis)) 
p <- p + geom_histogram(bins = 50)
p <- p + scale_fill_viridis(discrete = TRUE, name = "Include\nin Study", labels = c("Include", "Exclude"))
p <- p + xlab("Number of Attributes")
p <- p + ylab("Number of Cases")
p <- p + ggtitle("Number of Attributes by Include in study")
p <- p + theme_classic()

print(p)
p4 <- p

g <- grid.arrange(p1, p2, p3, p4, nrow = 2)

ggsave(paste0(results.sub.dir, "/", "inc_data_vis_grid",".png"),g)

# Plot missing data
# Numeric columns
# load int3 data set as has all data not adjusted.

RDVData <- read.csv(file=paste0(source.dir, "\\rdv_study_int3",  ".csv"), header=TRUE, sep=",")

# Gives a list of columns with NA's creater than 0
na <- sapply(RDVData, function(x) sum(is.na(x)))
# Removes columns with no NAs i.e. categorical data
RDVData.na <- RDVData[na > 0]

# Remove rows where gestational_age = NA
# RDVData.na <- RDVData.na[!is.na(RDVData.na$gestation_at_delivery_in_days),]
# Remove rows where gestational_age = NA
# RDVData.na <- RDVData.na[!is.na(RDVData.na$thyroid_weight),]

# Creates a data set with just one row, number of NAs
RDVData.na <- data.frame(lapply(RDVData.na, function(x) sum(is.na(x))))
# Pivots table to columns becomes row
RDVData.na <- melt(RDVData.na)
# sort by value
str(RDVData.na)


# Visualization NAs
p <- ggplot(RDVData.na, aes(x = reorder(variable,-value), y = value, fill = value)) 
p <- p + geom_bar(stat="identity")
p <- p + scale_fill_viridis(direction = -1) 
p <- p + xlab("Measurements")
p <- p + ylab("Number of NAs")
p <- p + ggtitle("Number of NAs by Measurement")
p <- p + theme_classic()
p <- p + theme(axis.text.x=element_text(angle=45,hjust=1))

print(p)

ggsave(paste0(results.sub.dir, "/", "na_measurements",".png"),p)

# Balanced Data

RDVData <- read.csv(file=paste0(source.dir, "\\rdv_study_int3",  ".csv"), header=TRUE, sep=",")

# Gives a list of columns with NA's creater than 0
na <- sapply(RDVData, function(x) sum(is.na(x)))
# Removes columns with NAs > 0 
RDVData.cat <- RDVData[na == 0]

RDVData.cat <- RDVData.cat %>%
  select(-c(event_id, event_start_date, age_category, case_id, include_in_study))

summary(RDVData.cat)  

# column.list <- c("season", "neglect_ynid", "nutrition_nutn_id", "dysmorphic_features_ynid", "jaundice_ynid")
# column.list <- c("season", "cod2_summ", "nutrition_nutn_id")
column.list <- colnames(RDVData.cat)

p.list <- list()

for(column.num in 1:length(column.list)) {
  
  column_name <- column.list[column.num]
  
  p <- ggplot(RDVData.cat, aes_string(x = column_name, fill = "..count.."))
  p <- p + geom_bar(stat = "count")
  p <- p + scale_fill_viridis(limits=c(1, 500), oob = scales::squish, "count",option = "A") 
  p <- p + ggtitle(column_name)
  p <- p + theme_classic()
  p <- p + theme(axis.text.x=element_text(angle=45,hjust=1),
                 axis.text.y = element_blank(),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 legend.position = "none",
                 plot.title = element_text(size = 9, face = "bold")
                )

    p.list[[column.num]] <- p

}

g <- do.call(grid.arrange,p.list)

# g <- grid.arrange(p1, p2, p3, p4, nrow = 2)

ggsave(paste0(results.sub.dir, "/", "study_categoric_values",".png"),g)

