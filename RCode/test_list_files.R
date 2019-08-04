
# Clear work space
rm(list = ls())

results.dir <- "I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\Results"
results.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/Results"

df <- file.info(list.files(pattern = "^dt_f(.*)csv$", ignore.case=TRUE))
latest_filename <- rownames(df)[which.max(df$mtime)]

df <- file.info(list.dirs(path = results.dir, full.names = TRUE))
latest_filename <- rownames(df)[which.max(df$mtime)]
