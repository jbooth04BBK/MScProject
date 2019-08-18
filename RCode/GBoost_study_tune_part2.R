# References
#
# https://shirinsplayground.netlify.com/2018/11/ml_basics_gbm
# https://rpubs.com/dalekube/XGBoost-Iris-Classification-Example-in-R
#
# Tuning:
# https://datascienceplus.com/extreme-gradient-boosting-with-r/
# https://insightr.wordpress.com/2018/05/17/tuning-xgboost-in-r-part-i/
#
# More tuning:
# https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/
# https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/
# 
#----- Initialise model

# Load libraries
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(lubridate)
library(reshape2)
# xgboost added:
library(xgboost)
library('DiagrammeR') # NB installed package
library('rsvg') # NB installed package
library('DiagrammeRsvg') # NB installed package

# Clear work space
rm(list = ls())

source("study_functions.R")

source.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/DataExtraction/CSVs"
results.dir <- "I:/DRE/Projects/Research/0004-Post mortem-AccessDB/Results"

study.prefix <- "run_12_"

now <- Sys.time()
sub.dir <- paste0(study.prefix,format(now, "%Y%m%d_%H%M"))
results.sub.dir <- file.path(results.dir, sub.dir)

if (!dir.exists(results.sub.dir)) {
  dir.create(results.sub.dir)
}

# Adjusted data or not for this study
data.adjusted <- TRUE
if (data.adjusted) {
  rdv.type = "_adj"
} else {
  rdv.type = ""
}

importance.min <- 1.0

model.list = c("dt","rf","xgb")
# stage.list = c("ext","int1","int2","int3")
stage.list = c("ext")

run.num <- 1

file.suffix <- sprintf("_%02d", run.num)

now <- Sys.time()
run.seed <- as.integer((second(now) - as.integer(second(now))) * 1000)


#----- End initialise

# RunXGBModel <- function(run.seed, rdv.type, importance.min, source.dir, results.sub.dir, file.suffix, stage.list) {
  
  set.seed(run.seed)
  
  model.name = "XGBoost Tree"
  model.abv = "xgb"

  run.str <- substr(file.suffix, nchar(file.suffix) - 1, nchar(file.suffix))
  
  fimp.matrix <- setup.fimp.matrix(rdv.type, source.dir, run.str)
  
  results.matrix <- setup.results.matrix(model.abv,length(stage.list))
  
  stage.num <- 1
  
  # for(stage.num in 1:length(stage.list)) {
    
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
    
    RDVData <- read.csv(file=paste0(source.dir, "\\rdv_study_", stage, rdv.type, ".csv"), header=TRUE, sep=",")
    
    #Remove unwanted columns - gestation_at_delivery_in_days
    if (stage == "ext") { 
      clean_RDVData <- RDVData %>%
        select(-c(event_id, event_start_date, age_category, case_id, gestation_at_delivery_in_days, include_in_study)) %>%
        na.omit()
    } else {
      clean_RDVData <- RDVData %>%
        select(-c(event_id, event_start_date, age_category, case_id, gestation_at_delivery_in_days, include_in_study, foot_length, crown_rump_length)) %>%
        na.omit()
    }  
    
    results.matrix[stage.num,rm.col] = nrow(clean_RDVData)
    rm.col = rm.col + 1
    
    clean_RDVData$cod2_summ <- as.factor(clean_RDVData$cod2_summ)
    
    
    #########################################################
    # Further Data adjustments for XGBoost
    
    # Do it once with other paramters set to defaults
    # gradually chnage them to chosen values
    # repeat process with parameters set to chosen values and see if they change.
    
    # = parameters = #
    # = eta candidates = #
    # eta=c(0.05,0.1,0.2,0.3,0.5,1)
    eta=c(0.05,0.1,0.2,0.25,0.3,0.35)
    # = max_depth candidates = #
    md=c(2,4,6,8)
    # = sub_sample candidates = #
    ss=c(0.25,0.5,0.75,1)
    # = colsample_bytree candidates = #
    ct=c(0.25,0.5,0.75,1)
    # = gamma candidates = #
    gamma=c(0.1,1,5,10)
    # = min_child_weight candidates = #
    mcw=c(1,4,7,10)
    
    all=c(1,2,3,4)
    
    random.seeds = sample(1:500, 5)
    
    # pred_eta <- matrix(NA, length(eta) * length(random.seeds),3)
    params.list <- all
    param.name = "All Parameters Set"
    pred.param <- matrix(NA, length(params.list) * length(random.seeds),3)
    
    for(seed.num in 1:length(random.seeds)){
      
      random.seed = random.seeds[seed.num]

      xgb.data <- clean_RDVData
      
      num_class = length(levels(xgb.data$cod2_summ))
      cod2_summ = clean_RDVData$cod2_summ
      
      # Convert from class to numeric
      label <- as.integer(xgb.data$cod2_summ) - 1
      ## label <- as.factor(label) 
      xgb.data$cod2_summ = NULL
      
      n = nrow(xgb.data)
      train.index = sample(n,floor(0.80 * n))
      train.data = as.matrix(xgb.data[train.index,])
      train.label = label[train.index]
      test.data = as.matrix(xgb.data[-train.index,])
      test.label = label[-train.index]
      
      # Transform the two data sets into xgb.Matrix
      xgb.train = xgb.DMatrix(data=train.data,label=train.label)
      xgb.test = xgb.DMatrix(data=test.data,label=test.label)
      
      # i = 1
      for(i in 1:length(params.list)){
        
        # params.list[i]
        
        params=list(
          booster="gbtree",
          max_depth=6, 
          gamma=5,
          min_child_weight=4,
          subsample=0.75,
          colsample_bytree=0.5,
          objective="multi:softprob",
          eval_metric="mlogloss",
          num_class=num_class 
        )
        
        
        xgb.fit=xgb.train(
                    params=params,
                    data=xgb.train,
                    nrounds=1000,
                    nthreads=1,
                    early_stopping_rounds=10,
                    watchlist=list(val1=xgb.train,val2=xgb.test),
                    verbose=0
        )
        
        # Predict outcomes with the test data - match objective and eval_metric above
        xgb.pred = predict(xgb.fit,test.data,reshape=T)
        xgb.pred = as.data.frame(xgb.pred)
        colnames(xgb.pred) = levels(cod2_summ)
        
        # Use the predicted label with the highest probability
        xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
        xgb.pred$label = levels(cod2_summ)[test.label+1]
        
        # Calculate the final accuracy
        result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
        pred.param[i + ((seed.num - 1) * length(params.list)),1] = params.list[i]
        pred.param[i + ((seed.num - 1) * length(params.list)),2] = result
        pred.param[i + ((seed.num - 1) * length(params.list)),3] = seed.num
      }
      
    }
      
    pred.param = data.frame(pred.param)
    pred.param$X1 <- factor(pred.param$X1)
    pred.param$X3 <- factor(pred.param$X3)
    
    p <- ggplot(pred.param, aes(x = X1, y = X2, group = X3)) 
    p <- p + geom_line(aes(color=X3))
    p <- p + geom_point()
    p <- p + ylim(0.5, 0.8)
    p <- p + ggtitle(paste0(param.name," Accuracy"))
    p <- p + ylab("Accuracy")
    p <- p + xlab(param.name)

    print(p)
      
    # eta = 0.25 OK on pass 2
    # max_depth = 6 OK on Pass 2
    # subsamplle = .75
    # colsample_bytree = 0.50
    # gamma = 5
    # min_child_weight = 4
    

    # Original work
    
    xgb_trcontrol = trainControl(
      method = "cv",
      number = 5,  
      allowParallel = TRUE,
      verboseIter = FALSE,
      returnData = FALSE
    )
    
    # = parameters = #
    # = nrounds candidates = #
    # nr.list <- c(100,200,300)
    nr.list <- c(1000)
    # = eta candidates = #
    eta.list <- c(0.05,0.1,0.2,0.3)
    # = colsample_bylevel candidates = #
    cs.list <- c(1/3,2/3,1)
    # = max_depth candidates = #
    md.list <- c(5,6)
    # = sub_sample candidates = #
    ss.list <- c(0.5)
    # = min_child_weights candidates = #
    mcw.list <- c(1)
    # = gamma candidates = #
    gamma.list <- c(0,0.1,1)
    
    # 3 x 5 x 3 x 3 x 4 x 4 x 4 = 8,640 combinations 1 second each = 2.4 hours, took 3.5 hours
    
    xgbGrid <- expand.grid(nrounds = nr.list,  
                           max_depth = md.list,
                           colsample_bytree = cs.list,
                           eta = eta.list,
                           gamma=gamma.list,
                           min_child_weight = mcw.list,
                           subsample = ss.list
    )
    
    start.time <- proc.time()
    print(Sys.time())

    train.label.factor <- as.factor(train.label)
    
    xgb_model = train(
      xgb.train, 
      train.label.factor,  
      trControl = xgb_trcontrol,
      tuneGrid = xgbGrid,
      method = "xgbTree"
    )
    
    end.time <- proc.time()
    print(end.time - start.time)
    print(Sys.time())
    
    xgb_model$bestTune
    
    #Store best values
    tune.nrounds = xgb_model$bestTune["nrounds"][1,1]
    results.matrix[stage.num,rm.col] = tune.nrounds
    rm.col = rm.col + 1
    tune.max_depth = xgb_model$bestTune["max_depth"][1,1]
    results.matrix[stage.num,rm.col] = tune.max_depth
    rm.col = rm.col + 1
    tune.eta = xgb_model$bestTune["eta"][1,1]
    results.matrix[stage.num,rm.col] = tune.eta
    rm.col = rm.col + 1
    tune.gamma = xgb_model$bestTune["gamma"][1,1]
    results.matrix[stage.num,rm.col] = tune.gamma
    rm.col = rm.col + 1
    tune.colsample_bytree = xgb_model$bestTune["colsample_bytree"][1,1]
    results.matrix[stage.num,rm.col] = tune.colsample_bytree
    rm.col = rm.col + 1
    tune.subsample = xgb_model$bestTune["subsample"][1,1]
    results.matrix[stage.num,rm.col] = tune.subsample
    rm.col = rm.col + 1
    
    # End tuning
       
    params = list(
      booster="gbtree",
      eta=tune.eta,
      max_depth=tune.max_depth,
      gamma=tune.gamma,
      subsample=tune.subsample,
      colsample_bytree=tune.colsample_bytree,
      objective="multi:softprob",
      eval_metric="mlogloss",
      num_class=num_class
    )
    
    # Train the XGBoost classifer
    xgb.fit=xgb.train(
      params=params,
      data=xgb.train,
      nrounds=tune.nrounds,
      nthreads=1,
      early_stopping_rounds=10,
      watchlist=list(val1=xgb.train,val2=xgb.test),
      verbose=0
    )

    # Review the final model and results
    # xgb.fit
    
    # Predict outcomes with the test data
    xgb.pred = predict(xgb.fit,test.data,reshape=T)
    xgb.pred = as.data.frame(xgb.pred)
    colnames(xgb.pred) = levels(cod2_summ)
    
    # Use the predicted label with the highest probability
    xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
    xgb.pred$label = levels(cod2_summ)[test.label + 1]
    
    # Calculate the final accuracy
    result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
  
    print(result)
    
    results.matrix[stage.num,rm.col] = result
    rm.col = rm.col + 1
    
    # Create confusion matrix
    table_mat <- table(xgb.pred$label, xgb.pred$prediction)
  
    # Loop over my_matrix
    for(row in 1:nrow(table_mat)) {
      for(col in 1:ncol(table_mat)) {
        results.matrix[stage.num,rm.col] = table_mat[row, col]
        rm.col = rm.col + 1
      }
    }
    
    importance <- xgb.importance(model = xgb.fit)
    imp <- as.data.frame(importance)
  
    total_imp = sum(imp$Gain)
    
    for (imp_row in 1:nrow(imp)){
      res_row = which(fimp.matrix$feature == imp[imp_row,1])
      fimp.matrix[res_row, stage.num + 2] <- (imp[imp_row,2] / total_imp) * 100
    }
    
    plot.title = paste0("Feature Importance - Model: ",model.name,", Stage: ",stage)
    
    p <- ggplot(imp, aes(x=reorder(Feature, Gain), y=Gain))
    p <- p + geom_point()
    p <- p + geom_segment(aes(x=Feature,xend=Feature,y=0,yend=Gain))
    p <- p + ggtitle(plot.title)
    p <- p + ylab("Relative Importance")
    p <- p + xlab("Feature")
    p <- p + coord_flip()
    
    print(p)
    
    ggsave(paste0(results.sub.dir, "/", model.abv, "_feature_importance_",stage, file.suffix,".png"))
    
    # Single tree plot
    
    p <- xgb.plot.tree(model = xgb.fit, trees = 0, show_node_id = TRUE)
    
    print(p)
  
    gr <- xgb.plot.tree(model=xgb.fit, trees=0, show_node_id = TRUE, render=FALSE) 
    export_graph(gr, paste0(results.sub.dir, "/", model.abv, "_tree_",stage, file.suffix,".png"), width=1500, height=1900)

  # }
  
  #############################
  ## graph combined importance
  #############################
  
  data <- fimp.matrix
  # Order results
  data$feature <- with(data, reorder(feature, ext + int1 + int2 + int3))
  # Remove 0 values and create structure to plot
  data.m.ss <- subset(melt(data), value > importance.min)
  # Create plot
  plot.title = paste0("Feature Importance Heatmap - Model: ",model.name)
  p <- ggplot(data.m.ss, aes(x=variable, y=feature)) 
  p <- p + ggtitle(plot.title)
  p <- p + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "green", high = "red")
  p <- p + geom_text(aes(label = round(value, 1)))
  # p <- p + theme(axis.text=element_text(size=6))
  
  print(p)
  
  ggsave(paste0(results.sub.dir, "/", model.abv, "_feature_importance_hm", file.suffix, ".png"))

  #############################
  ## output results CSV files
  #############################
  
  write.csv(results.matrix, file = paste0(results.sub.dir, "/", model.abv, "_results_matrix", file.suffix, ".csv"),row.names=FALSE, na="")
  write.csv(fimp.matrix, file = paste0(results.sub.dir, "/", model.abv, "_feature_importance_matrix", file.suffix, ".csv"),row.names=FALSE, na="")
  
  #################################

# }
  
  