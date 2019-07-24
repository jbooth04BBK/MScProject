# Clear work space
rm(list = ls())

#Read in largest CSVs and get column names
RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int3_adj.csv", header=TRUE, sep=",")
cn = colnames(RDVData)
RDVData <- read.csv(file="I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\DataExtraction\\CSVs\\rdv_study_int3_s_adj.csv", header=TRUE, sep=",")
cn1 = colnames(RDVData)
RDVData <- NULL

cn <- append(cn, cn1, after = length(cn))
cn <- unique(cn)

# Remove unwanted columns
cn <- cn[!cn %in% c("event_id", "event_start_date", "age_category", "case_id", "include_in_study", "foot_length", "crown_rump_length")]

col_values  = replicate(length(cn),0.0)

# create an empty data frame
column_names <- c("feature","ext","int1","int2","int3","int3_s")
fimp_results <- data.frame(cn, col_values, col_values, col_values, col_values, col_values)
colnames(fimp_results) <- column_names

row = which(fimp_results$feature == "age_in_days")
# now change its value
fimp_results[row, 2] <- 10.67



library(lubridate)


run_seed <- as.integer(second(Sys.time()))
set.seed(run_seed)
