
# Clear work space
rm(list = ls())

df <- file.info(list.files(pattern = "^dt_f(.*)csv$", ignore.case=TRUE))
latest_filename <- rownames(df)[which.max(df$mtime)]
