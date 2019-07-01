library(dplyr)

set.seed(62)

# Read CSV into R
RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_005_ext_measurements.csv", header=TRUE, sep=",")

str(RDVData)

summary(RDVData$body_weight)

plot(RDVData, pch='.')

plot(RDVData$body_length,RDVData$heart_weight,main = "body length vs heart_weight")

fit = lm(RDVData$heart_weight~RDVData$body_length)
summary(fit)
abline(fit, col="red")

new_x <- data.frame(x=seq(30,100,length = 329))
p_pred <- predict(fit,interval = "prediction",newdata = new_x)
summary(p_pred)
lines(new_x$x,p_pred[,"lwr"],col="blue")
lines(new_x$x,p_pred[,"upr"],col="blue")
summary(new_x)
