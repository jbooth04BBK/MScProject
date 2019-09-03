# References
#
# https://medium.com/analytics-vidhya/a-guide-to-machine-learning-in-r-for-beginners-decision-trees-c24dfd490abb
# https://www.guru99.com/r-decision-trees.html
# https://www.gormanalysis.com/blog/decision-trees-in-r-using-rpart/
# https://infocenter.informationbuilders.com/wf80/index.jsp?topic=%2Fpubdocs%2FRStat16%2Fsource%2Ftopic47.htm
#

#--- Start initialise function

# # Load libraries
# library(dplyr)
# library(ggplot2)
# library(rpart)
# library(rpart.plot)
# library(caret)
# library(lubridate)
# library(reshape2)
# 
# # Clear work space
# rm(list = ls())
# 
# source("study_functions.R")
# 
# source.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/DataExtraction/CSVs"
# results.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/Results"
# 
# study.prefix <- "run_15_"
# 
# now <- Sys.time()
# sub.dir <- paste0(study.prefix,format(now, "%Y%m%d_%H%M"))
# results.sub.dir <- file.path(results.dir, sub.dir)
# 
# if (!dir.exists(results.sub.dir)) {
#   dir.create(results.sub.dir)
# }
# 
# # Adjusted data or not for this study
# data.adjusted <- TRUE
# if (data.adjusted) {
#   rdv.type = "_adj"
# } else {
#   rdv.type = ""
# }
# 
# importance.min <- 1.5
# 
# model.list = c("dt","rf","xgb")
# stage.list = c("ext","int1","int2","int3")
# 
# run.num <- 1
# file.suffix <- sprintf("_%02d", run.num)
# 
# now <- Sys.time()
# run.seed <- as.integer((second(now) - as.integer(second(now))) * 1000)
# 
# stage = "ext"
# clean_RDVData <- return_clean_rdvdata(source.dir, stage, rdv.type)
# ext.train.index = sample(nrow(clean_RDVData),floor(0.80 * nrow(clean_RDVData)))
# 
# stage = "int1"
# clean_RDVData <- return_clean_rdvdata(source.dir, stage, rdv.type)
# int1.train.index = sample(nrow(clean_RDVData),floor(0.80 * nrow(clean_RDVData)))
# 
# stage = "int2"
# clean_RDVData <- return_clean_rdvdata(source.dir, stage, rdv.type)
# int2.train.index = sample(nrow(clean_RDVData),floor(0.80 * nrow(clean_RDVData)))
# 
# stage = "int3"
# clean_RDVData <- return_clean_rdvdata(source.dir, stage, rdv.type)
# int3.train.index = sample(nrow(clean_RDVData),floor(0.80 * nrow(clean_RDVData)))

#--- End initialise function

RunDTModel <- function(run.seed,
                       rdv.type,
                       importance.min,
                       source.dir,
                       results.sub.dir,
                       file.suffix,
                       stage.list,
                       ext.train.index,
                       int1.train.index,
                       int2.train.index,
                       int3.train.index
                       ) {

  set.seed(run.seed)
  
  model.name = "Decision Tree"
  model.abv = "dt"
  
  run.str <- substr(file.suffix, nchar(file.suffix) - 1, nchar(file.suffix))
  
  fimp.matrix <- setup.fimp.matrix(rdv.type, source.dir, run.str)
  
  results.matrix <- setup.results.matrix(model.abv,length(stage.list))
  
  # stage.num <- 1
  
  for(stage.num in 1:length(stage.list)) {
    
    stage <- stage.list[stage.num]
    
    rm.col <- 1
    
    print(paste0("Run: ", run.str, " Model: ",model.name," Stage: ",stage))
    
    now <- Sys.time()
    
    results.matrix[stage.num,rm.col] = run.str
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] = format(now, "%Y-%m-%d %H:%M:%S")
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] = rdv.type
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] = run.seed
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] = stage
    rm.col = rm.col + 1
    
    clean_RDVData <- return_clean_rdvdata(source.dir, stage, rdv.type)
    
    results.matrix[stage.num,rm.col] = nrow(clean_RDVData)
    rm.col = rm.col + 1
  
    clean_RDVData$cod2_summ <- as.factor(clean_RDVData$cod2_summ)
    
    if (stage == "ext") { 
      train.index <- ext.train.index
    } else if (stage == "int1") {
      train.index <- int1.train.index
    } else if (stage == "int2") {
      train.index <- int2.train.index
    } else if (stage == "int3") {
      train.index <- int3.train.index
    }  
    
    data_train <- clean_RDVData[train.index, ]
    data_test  <- clean_RDVData[-train.index, ]
    
    # Store proportional split of COD2_SUMM for this run    
    results.matrix[stage.num,rm.col] =prop.table(table(data_train$cod2_summ))[1]
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] =prop.table(table(data_train$cod2_summ))[2]
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] =prop.table(table(data_test$cod2_summ))[1]
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] =prop.table(table(data_test$cod2_summ))[2]
    rm.col = rm.col + 1
    
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
    
    run.tune <- FALSE  
    
    if (run.tune) {

      # r_minsplit = seq(1,200,by = 20)
      r_minsplit = seq(9,36,by = 3)
      r_maxdepth = seq(1,20,by = 2)
      row = 0
      
      max_accuracy = 0
      max_minsplit = 0
      max_maxdepth = 0
      
      accuracy_matrix = matrix(nrow = length(r_minsplit),ncol = length(r_maxdepth))
      
      for (ms in r_minsplit) {
        row = row + 1
        col = 0
        for (md in r_maxdepth) {
          col = col + 1
          control <- rpart.control(minsplit = ms,
                                   minbucket = round(ms / 3),
                                   maxdepth = md,
                                   cp = 0.01)
          tune_fit <- rpart(cod2_summ~., data = data_train, method = 'class', control = control)
      
          accuracy_matrix[row,col] = accuracy_fit(tune_fit, data_test)
          
          if (accuracy_fit(tune_fit, data_test) > max_accuracy) {
            max_accuracy = accuracy_fit(tune_fit, data_test)
            max_minsplit = ms
            max_maxdepth = md
          }
          
        }
        if (row == 1) {
         plot(accuracy_matrix[row,], type="o", ylim=c(0.5,1.0), pch=row-1, xlab= "maxdepth", ylab= "accuracy")
        } else {
         lines(accuracy_matrix[row,], type="o", pch=row-1)
        }
        
      }
      
      legend(9, 0.65, c("1","2","3","4","5","6","7","8","9","10"), cex = 0.8, pch = 0:9, lty = 1)
      
      title(main=paste0("rpart control variables, stage: ",stage), col.main="red", font.main=4)
      
      dev.copy(png,filename=paste0(results.sub.dir, "/", model.abv, "_tree_variables_",stage, file.suffix,".png"));
      dev.off ();
    
    } else {
      max_accuracy <- 0
      max_minsplit <- 20
      max_maxdepth <- 0
    }
    
    results.matrix[stage.num,rm.col] = max_accuracy
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] = max_minsplit
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] = max_maxdepth
    rm.col = rm.col + 1
    
    control <- rpart.control(minsplit = max_minsplit,
                             minbucket = round(max_minsplit / 3),
                             # maxdepth = max_maxdepth,
                             cp = 0.01)
    
    tune_fit <- rpart(cod2_summ~., data = data_train, method = 'class', control = control)
    
    #############################
    ## Store and graph importance
    #############################
    
    imp <- as.data.frame(varImp(tune_fit))
    # Remove 0 importance variables
    imp <- subset(imp, Overall > importance.min)
  
    total_imp = sum(imp)
    print(total_imp)
    for (imp_row in 1:nrow(imp)){
      res_row = which(fimp.matrix$feature == rownames(imp)[imp_row])
      fimp.matrix[res_row, stage.num + 2] <- (imp[imp_row,1] / total_imp) * 100
      imp[imp_row,1] <- (imp[imp_row,1] / total_imp) * 100
    }
    
    # Remove less import features for clarity
    imp <- subset(imp, Overall > importance.min)
    
    imp$varnames <- rownames(imp) # row names to column
    rownames(imp) <- NULL  
    imp$var_categ <- rep(1, nrow(imp)) # random var category
    
    plot.title = paste0("Relative Feature Importance - Model: ",model.name,", Stage: ",stage)
    
    p <- ggplot(imp, aes(x=reorder(varnames, Overall), y=Overall))
    p <- p + geom_point()
    p <- p + geom_segment(aes(x=varnames,xend=varnames,y=0,yend=Overall))
    p <- p + ggtitle(plot.title)
    p <- p + ylab("Relative Importance")
    p <- p + xlab("Feature")
    p <- p + coord_flip()
    p <- p + theme_classic()
    
    print(p)
    
    ggsave(paste0(results.sub.dir, "/", model.abv, "_feature_importance_",stage, file.suffix,".png"))
    
    #############################
    ## Plot Tree
    #############################
    
    # dev.new(width=10, height=10, unit="in")
    rpart.plot(tune_fit)
    title(main=paste0("Tree - Model: ",model.name,", Stage: ",stage), col.main="red", font.main=4)
    
    # These results need storing - not quite sure how!
    # tune_fit
    # width=1000, height=750, unit="px"
    dev.copy(png,filename=paste0(results.sub.dir, "/", model.abv, "_tree_",stage, file.suffix,".png"),width=900, height=540, unit="px"); 
    dev.off ();
    
    # Split criteria
    # rows in this node
    # Misclassified
    # Predicted Class
    # % of rows in predicted class for this node.
    
    sink(paste0(results.sub.dir, "/", model.abv, "_tree_fit_",stage, file.suffix,".txt"))
    print(tune_fit)
    sink()
    
    sink(paste0(results.sub.dir, "/", model.abv, "_tree_cp_",stage, file.suffix,".txt"))
    printcp(tune_fit)
    sink()

    # plotcp(tune_fit)
    
    # CP Table
    # Variable Importance
    # Description of the Node and Split (including # going left or right and even surrogate splits.
                                       
    # summary(tune_fit)
    
    #############################
    ## Store confusion matrix
    #############################
    
    accuract.fit <- accuracy_fit(tune_fit, data_test)
    
    print(accuract.fit)
    
    results.matrix[stage.num,rm.col] = accuract.fit
    rm.col = rm.col + 1
    
    table_mat <- cmatrix_fit(tune_fit, data_test)
    
    # Loop over my_matrix
    for(row in 1:nrow(table_mat)) {
      for(col in 1:ncol(table_mat)) {
        results.matrix[stage.num,rm.col] = table_mat[row, col]
        rm.col = rm.col + 1
      }
    }
    
  } ## end of For loop
  
  #############################
  ## graph combined importance
  #############################
  
  data <- fimp.matrix
  # Order results
  data$feature <- with(data, reorder(feature, ext + int1 + int2 + int3))
  # Remove 0 values and create structure to plot
  data.m.ss <- subset(melt(data), value > importance.min)
  # Create plot
  plot.title = paste0("Relative Feature Importance Heatmap - Model: ",model.name)
  p <- ggplot(data.m.ss, aes(x=variable, y=feature)) 
  p <- p + geom_tile(aes(fill = value))
  p <- p + scale_fill_viridis_c(direction = -1, begin = .3, end = 1)
  p <- p + geom_text(aes(label = round(value, 1)))
  p <- p + xlab("Stage")
  p <- p + ggtitle(plot.title)
  p <- p + theme_classic()
  
  print(p)
  
  ggsave(paste0(results.sub.dir, "/", model.abv, "_feature_importance_hm", file.suffix, ".png"))
  
  #############################
  ## output results CSV files
  #############################
  
  write.csv(results.matrix, file = paste0(results.sub.dir, "/", model.abv, "_results_matrix", file.suffix, ".csv"),row.names=FALSE, na="")
  write.csv(fimp.matrix, file = paste0(results.sub.dir, "/", model.abv, "_feature_importance_matrix", file.suffix, ".csv"),row.names=FALSE, na="")
  
  #################################

}