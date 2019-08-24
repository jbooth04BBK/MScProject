
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

library(ggplot2) 
library(ggmosaic)
library(gridExtra)
library(viridis)

source.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/DataExtraction/CSVs"
results.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/Results"
study.prefix <- "run_12_"
sub.dir <- paste0(study.prefix,"20190819_2005")
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

print(p)
p2 <- p

# Visualization cod2_summ
p <- ggplot(RDVData, aes(x = number_of_attributes, fill = cod2_summ)) 
p <- p + geom_histogram(bins = 50)
p <- p + scale_fill_viridis(discrete = TRUE, name = "Cause of Death\nDetermined", labels = c("No", "Yes", "Unknown", "N/A"))
p <- p + ggtitle("Number of Attributes")
p <- p + theme_classic()

print(p)
p3 <- p

# Visualization cod2_summ
p <- ggplot(RDVData, aes(x = year, fill = cod2_summ)) 
p <- p + geom_histogram(bins = 50)
p <- p + scale_fill_viridis(discrete = TRUE, name = "Cause of Death\nDetermined", labels = c("No", "Yes", "Unknown", "N/A"))
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
p <- p + ggtitle("Cause of Death Determined split by Include in Study")
p <- p + theme_classic()

print(p)
p1 <- p

# Visualization include in study
p <- ggplot(RDVData, aes(x = include_in_study, fill = sex)) 
p <- p + geom_bar()
p <- p + scale_x_discrete(labels = c("Include", "Exc - COD", "Exc - Age", "Exc - Measure"))
p <- p + scale_fill_discrete(name = "Sex", labels = c("Female", "Male", "Unknown"))
p <- p + xlab("Include in Study")
p <- p + ggtitle("Include in Study split by Sex")
p <- p + theme_classic()

print(p)
p2 <- p

# Visualization include in study
p <- ggplot(RDVData, aes(x = include_in_study, fill = age_category)) 
p <- p + geom_bar(position=position_dodge())
p <- p + scale_x_discrete(labels = c("Include", "Exc - COD", "Exc - Age", "Exc - Measure"))
p <- p + scale_fill_discrete(name = "Age Catgeory", labels = c("Miscarriage", "Still Birth", "Early Neonatal", "Neonatal","Infant Death","Child Death","N/A"))
p <- p + xlab("Include in Study")
p <- p + ggtitle("Include in Study split by Age Category")
p <- p + theme_classic()

print(p)
p3 <- p

# Visualization cod2_summ
p <- ggplot(RDVData, aes(x = number_of_attributes, fill = include_in_study)) 
p <- p + geom_histogram(bins = 50)
p <- p + scale_fill_discrete(name = "Include\nin Study", labels = c("Include", "Exc - COD", "Exc - Age", "Exc - Measure"))
p <- p + xlab("Number of Attributes")
p <- p + ggtitle("Number of Attributes by Include in study")
p <- p + theme_classic()

print(p)

p4 <- p

g <- grid.arrange(p1, p2, p3, p4, nrow = 2)

ggsave(paste0(results.sub.dir, "/", "inc_data_vis_grid",".png"),g)

