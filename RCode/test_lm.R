#
# Reference
#
# https://www.datacamp.com/community/tutorials/linear-regression-R
# https://www.machinelearningplus.com/machine-learning/complete-introduction-linear-regression-r/
#

library(dplyr)
library(lubridate)

# Clear work space
rm(list = ls())

source("study_functions.R")

now = Sys.time()
run_seed <- as.integer((second(now) - as.integer(second(now))) * 1000)
set.seed(run_seed)

RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int3.csv", header=TRUE, sep=",")

names(RDVData)

clean_RDVData <- RDVData %>%
  select(c(age_in_days, heart_weight)) %>% 
  rename(measure = heart_weight) %>%
  na.omit()

lm.fit = lm(measure~age_in_days, data = clean_RDVData)

lm.fit
summary(lm.fit)

lm.fit.intercept <- lm.fit$coefficients["(Intercept)"]
lm.fit.slope <- lm.fit$coefficients["age_in_days"]

plot(clean_RDVData$age_in_days,clean_RDVData$measure)
abline(lm.fit, col="red")

# plot(lm.fit$residuals)

# plot(cooks.distance(lm.fit), pch = 16, col = "blue")

meaure_Pred <- predict(lm.fit, clean_RDVData)  # predict measure

actuals_preds <- data.frame(cbind(age_in_days=clean_RDVData$age_in_days, actuals=clean_RDVData$measure, predicteds=meaure_Pred))

actuals_preds$ape <- abs(actuals_preds$actuals - actuals_preds$predicteds) / actuals_preds$actuals

plot(actuals_preds$age_in_days,actuals_preds$ape)


