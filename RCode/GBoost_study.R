# References
#
# https://shirinsplayground.netlify.com/2018/11/ml_basics_gbm
# https://rpubs.com/dalekube/XGBoost-Iris-Classification-Example-in-R
#

RunXGBModel <- function(run.seed, rdv.type, importance.min, source.dir, results.sub.dir, file.suffix, stage.list) {
  
  set.seed(run.seed)
  
  model.name = "XGBoost Tree"
  model.abv = "xgb"

  run.str <- substr(file.suffix, nchar(file.suffix) - 1, nchar(file.suffix))
  
  fimp.matrix <- setup.fimp.matrix(rdv.type, source.dir, run.str)
  
  results.matrix <- setup.results.matrix(model.abv,length(stage.list))
  
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
    # XGBoost
    
    xgb.data <- clean_RDVData
    
    num_class = length(levels(xgb.data$cod2_summ))
    cod2_summ = clean_RDVData$cod2_summ
    
    # Convert from class to numeric
    label <- as.integer(xgb.data$cod2_summ) - 1
    xgb.data$cod2_summ = NULL
    
    n = nrow(xgb.data)
    train.index = sample(n,floor(0.80 * n))
    train.data = as.matrix(xgb.data[train.index,])
    train.label = label[train.index]
    test.data = as.matrix(xgb.data[-train.index,])
    test.label = label[-train.index]
    
    # Store proportional split of COD2_SUMM for this run    
    results.matrix[stage.num,rm.col] = prop.table(table(train.label))[1]
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] = prop.table(table(train.label))[2]
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] = prop.table(table(test.label))[1]
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] = prop.table(table(test.label))[2]
    rm.col = rm.col + 1
    
    # Transform the two data sets into xgb.Matrix
    xgb.train = xgb.DMatrix(data=train.data,label=train.label)
    xgb.test = xgb.DMatrix(data=test.data,label=test.label)

    # Original values    
    # params = list(
    #   booster="gbtree",
    #   eta=0.3,
    #   max_depth=6,## NB Was 9
    #   gamma=0,
    #   subsample=1,
    #   colsample_bytree=1,
    #   objective="multi:softprob",
    #   eval_metric="mlogloss",
    #   num_class=num_class
    # )
    
    if (stage == "ext") { 
      eta.value <- 0.3
      max_depth.value <- 6
      gamma.value <- 5
      min_child_weight.value <- 4
      subsample.value <- 0.75
      colsample_bytree.value <- 0.50
    } else if (stage == "int1") {
      eta.value <- 0.185
      max_depth.value <- 6
      gamma.value <- 2.65
      min_child_weight.value <- 8
      subsample.value <- 0.725
      colsample_bytree.value <- 0.50
    } else if (stage == "int2") {
      eta.value <- 0.2
      max_depth.value <- 5
      gamma.value <- 1
      min_child_weight.value <- 5
      subsample.value <- 0.775
      colsample_bytree.value <- 0.5
    } else if (stage == "int3") {
      eta.value <- 0.17
      max_depth.value <- 5
      gamma.value <- 3.2
      min_child_weight.value <- 4
      subsample.value <- 0.65
      colsample_bytree.value <- 0.525
    } else {
      # default values    
      eta.value <- 0.3
      max_depth.value <- 6 
      gamma.value <- 0
      min_child_weight.value <- 1
      subsample.value <- 1
      colsample_bytree.value <- 1
    }  
    
    #Store best values
    results.matrix[stage.num,rm.col] = eta.value
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] = max_depth.value
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] = gamma.value
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] = min_child_weight.value
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] = subsample.value
    rm.col = rm.col + 1
    results.matrix[stage.num,rm.col] = colsample_bytree.value
    rm.col = rm.col + 1
    
    params=list(
      booster="gbtree",
      eta=eta.value,
      max_depth=max_depth.value, 
      gamma=gamma.value,
      min_child_weight=min_child_weight.value,
      subsample=subsample.value,
      colsample_bytree=colsample_bytree.value,
      objective="multi:softprob",
      eval_metric="mlogloss",
      num_class=num_class 
    )
    
    # Train the XGBoost classifer
    xgb.fit=xgb.train(
      params=params,
      data=xgb.train,
      nrounds=1000,
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

  }
  
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

}