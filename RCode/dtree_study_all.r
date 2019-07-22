# References
#
# https://medium.com/analytics-vidhya/a-guide-to-machine-learning-in-r-for-beginners-decision-trees-c24dfd490abb
# https://www.guru99.com/r-decision-trees.html
#

# Load libraries
library(dplyr)
library(ggplot2) 
library(rpart)
library(rpart.plot)
library(caret)

# Clear work space
rm(list = ls())

source("study_functions.R")

set.seed(62)

# stage = "ext"
# stage = "int1"
# stage = "int2"
stage = "int3"

# Read CSV into R
if (stage == "ext") { 
  RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_ext_adj.csv", header=TRUE, sep=",")
} else if (stage == "int1") {
  RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int1_adj.csv", header=TRUE, sep=",")
} else if  (stage == "int2") {
  RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int2_adj.csv", header=TRUE, sep=",")
} else {
  RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int3_adj.csv", header=TRUE, sep=",")
}

#Remove unwanted columns
if (stage == "ext") { 
  clean_RDVData <- RDVData %>%
    select(-c(event_id, event_start_date, age_category, case_id, include_in_study)) %>%
    na.omit()
} else {
  clean_RDVData <- RDVData %>%
    select(-c(event_id, event_start_date, age_category, case_id, include_in_study, foot_length, crown_rump_length)) %>%
    na.omit()
}  

clean_RDVData$cod2_summ <- as.factor(clean_RDVData$cod2_summ)

str(RDVData)

glimpse(clean_RDVData)

data_train <- create_train_test(clean_RDVData, 0.8, train = TRUE)
data_test <- create_train_test(clean_RDVData, 0.8, train = FALSE)

prop.table(table(data_train$cod2_summ))
prop.table(table(data_test$cod2_summ))
summary(data_test$cod2_summ)

# rpart(formula, data=, method='')
# arguments:			
#   - formula: The function to predict
#   - data: Specifies the data frame
#   - method: 			
#     - "class" for a classification tree 			
#     - "anova" for a regression tree	

# rpart.control(minsplit = 20, minbucket = round(minsplit/3), maxdepth = 30)
# Arguments:
#   -minsplit: Set the minimum number of observations in the node before the algorithm perform a split
#   -minbucket:  Set the minimum number of observations in the final note i.e. the leaf
#   -maxdepth: Set the maximum depth of any node of the final tree. The root node is treated a depth 0

r_minsplit = seq(1,200,by=20)
r_maxdepth = seq(1,10,by=1)
row = 0

max_accuracy = 0
max_minsplit = 0
max_maxdepth = 0

accuracy_matrix = matrix(nrow=10,ncol=10)

for (ms in r_minsplit) {
  row = row + 1
  col = 0
  for (md in r_maxdepth) {
    col = col + 1
    control <- rpart.control(minsplit = ms,
                             minbucket = round(ms / 3),
                             maxdepth = md,
                             cp = 0)
    tune_fit <- rpart(cod2_summ~., data = data_train, method = 'class', control = control)

    accuracy_matrix[row,col] = accuracy_fit(tune_fit)
    
    if (accuracy_fit(tune_fit) > max_accuracy) {
      max_accuracy = accuracy_fit(tune_fit)
      max_minsplit = ms
      max_maxdepth = md
    }
    
  }
  if (row == 1) {
   plot(accuracy_matrix[row,], type="o", ylim=c(0.6,0.9), pch=row-1, xlab= "maxdepth", ylab= "accuracy")
  } else {
   lines(accuracy_matrix[row,], type="o", pch=row-1)
  }
  
}

legend(9, 0.65, c("1","2","3","4","5","6","7","8","9","10"), cex = 0.8, pch = 0:9, lty = 1)

title(main=paste0("rpart control variables, stage - ",stage), col.main="red", font.main=4)

print(paste('Max Accuracy for test', max_accuracy))
print(paste('  For minsplit', max_minsplit))
print(paste('      maxdepth', max_maxdepth))

control <- rpart.control(minsplit = max_minsplit,
                         minbucket = round(max_minsplit / 3),
                         maxdepth = max_maxdepth,
                         cp = 0)
tune_fit <- rpart(cod2_summ~., data = data_train, method = 'class', control = control)

tune_fit.importance <- varImp(tune_fit)
tune_fit.importance
##-------------------------------------------

imp <- as.data.frame(tune_fit.importance)
# Remove 0 importance variables
imp <- subset(imp, Overall>0)
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  
imp$var_categ <- rep(1, nrow(imp)) # random var category

str(imp)

ggplot(imp, aes(x=reorder(varnames, Overall), weight=Overall)) + 
  geom_bar() +
  ylab("Overall") +
  xlab("Variable Name") +
  coord_flip()

ggplot(imp, aes(x=reorder(varnames, Overall), y=Overall)) + 
  geom_point() +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=Overall)) +
  ylab("Overall") +
  xlab("Variable Name") +
  coord_flip()

##-------------------------------------------

rpart.plot(tune_fit)
title(main=paste0("Decision Tree, Stage - ",stage), col.main="red", font.main=4)

accuracy_Test <- accuracy_fit(tune_fit)

table_mat <- cmatrix_fit(tune_fit)
table_mat

print(paste('Accuracy for test', accuracy_Test))

