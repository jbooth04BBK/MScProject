
# Clear work space
rm(list = ls())

results.dir <- "I:\\DRE\\Projects\\Research\\0004-Post mortem-AccessDB\\Results"
results.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/Results"

df <- file.info(list.files(pattern = "^dt_f(.*)csv$", ignore.case=TRUE))
latest_filename <- rownames(df)[which.max(df$mtime)]

df <- file.info(list.dirs(path = results.dir, full.names = TRUE))
latest_filename <- rownames(df)[which.max(df$mtime)]

# defining the function
mergeCSV <- function(df.name, model.abv, file.text, results.sub.dir, max.run = 1) {
  
  df <- data.frame()
  
  for(run.num in 1:max.run){
    
    file.suffix <- sprintf("_%02d", run.num)
    
    add <- read.csv(file = paste0(results.sub.dir, "/", model.abv, file.text, file.suffix, ".csv"), header=TRUE, sep=",")
    
    df <- rbind(df,add)
  }
  
  # colnames(df) <- c(..specify the colnames in here..)
  
  assign(df.name, df, envir = .GlobalEnv)
  
}

results.sub.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/Results/run_09_20190805_1917"

model.abv = "dt"
mergeCSV("df.results", model.abv,"_results_matrix", results.sub.dir, 5)


# Visualization
p <- ggplot(df.results, aes(x = suffix, y = accuracy)) 
p <- p + geom_line(aes(color = stage))
# p <- p + scale_color_manual(values = c("darkred", "steelblue"))
print(p)

