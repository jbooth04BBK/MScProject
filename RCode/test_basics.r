# References
#
# https://medium.com/analytics-vidhya/a-guide-to-machine-learning-in-r-for-beginners-decision-trees-c24dfd490abb
# https://www.guru99.com/r-decision-trees.html
#

library(rpart)
library(rpart.plot)
library(dplyr)

set.seed(62)

# Read CSV into R
RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_new_attributes.csv", header=TRUE, sep=",")

head(RDVData)

str(RDVData)

summary(RDVData$number_of_attributes)

#Remove unwanted columns and factorise age_category & season
clean_RDVData <- RDVData %>%
select(-c(event_id, event_start_date, year, number_of_attributes, external_examination, internal_examination)) %>% 
  mutate(age_category = factor(age_category, levels = c(1, 2, 3, 4, 5, 6), labels = c('A1', 'A2', 'A3', 'A4', 'A5', 'A6')),
         season = factor(season, levels = c(1, 2, 3, 4), labels = c('Sp', 'Su', 'Au', 'Wi'))) %>%
  na.omit()

glimpse(clean_RDVData)

# create_train_test(df, size = 0.8, train = TRUE)
# arguments:
#  -df: Dataset used to train the model.
#  -size: Size of the split. By default, 0.8. Numerical value
#  -train: If set to `TRUE`, the function creates the train set, otherwise the test set. Default value sets to `TRUE`. Boolean value.
#   You need to add a Boolean parameter because R does not allow to return two data frames simultaneously.

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(clean_RDVData, 0.8, train = TRUE)
data_test <- create_train_test(clean_RDVData, 0.8, train = FALSE)
dim(data_train)
dim(data_test)

prop.table(table(data_train$cod2_summ))
prop.table(table(data_test$cod2_summ))

# rpart(formula, data=, method='')
# arguments:			
#   - formula: The function to predict
#   - data: Specifies the data frame
#   - method: 			
#     - "class" for a classification tree 			
#     - "anova" for a regression tree	

fit <- rpart(cod2_summ~., data = data_train, method = 'class')
rpart.plot(fit)

# predict(fitted_model, df, type = 'class')
# arguments:
#   - fitted_model: This is the object stored after model estimation. 
# - df: Data frame used to make the prediction
# - type: Type of prediction			
#   - 'class': for classification			
#   - 'prob': to compute the probability of each class			
#   - 'vector': Predict the mean response at the node level	

predict_unseen <-predict(fit, data_test, type = 'class')

# Create confusion matrix
table_mat <- table(data_test$cod2_summ, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

print(paste('Accuracy for test', accuracy_Test))

