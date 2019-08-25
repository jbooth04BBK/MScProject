# References
#
# https://www.guru99.com/r-random-forest-tutorial.html
#


RunRFModel <- function(run.seed, 
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
  
  model.name = "Random Forest"
  model.abv = "rf"

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
    
    # trainControl(method = "cv", number = n, search ="grid")
    # arguments
    # - method = "cv": The method used to resample the dataset. 
    # - number = n: Number of folders to create
    # - search = "grid": Use the search grid method. For randomized method, use "grid"
    # Note: You can refer to the vignette to see the other arguments of the function.
    
    # Define the control
    trControl <- trainControl(method = "cv",
                              number = 10,
                              search = "grid")
    
    # train(formula, df, method = "rf", metric= "Accuracy", trControl = trainControl(), tuneGrid = NULL)
    # argument
    # - `formula`: Define the formula of the algorithm
    # - `method`: Define which model to train. 
    # - `metric` = "Accuracy": Define how to select the optimal model
    # - `trControl = trainControl()`: Define the control parameters
    # - `tuneGrid = NULL`: Return a data frame with all the possible combination
    
    run.tune <- FALSE  
    
    if (run.tune) {
      
      # Run the model using defaults
      rf_default <- train(cod2_summ~.,
                          data = data_train,
                          method = "rf",
                          metric = "Accuracy",
                          trControl = trControl)
      
      # Print the results
      
      # mtry = 2 with accuracy of 0.679
      # adj mtry = 24 with accuracy of 0.673
      
      best.mtry <- rf_default$bestTune$mtry
      
      print(best.mtry)
      print(max(rf_default$results$Accuracy))
      
      results.matrix[stage.num,rm.col] = best.mtry
      rm.col = rm.col + 1
      results.matrix[stage.num,rm.col] = max(rf_default$results$Accuracy)
      rm.col = rm.col + 1
      
      min.mtry <- best.mtry - 5
      max.mtry <- best.mtry + 5
      
      # Try and find a better mtry
      tuneGrid <- expand.grid(.mtry = c(min.mtry : max.mtry))
      rf_mtry <- train(cod2_summ~.,
                       data = data_train,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       ntree = 300)
      
      # Max accuracy was 0.694 with mtry = 8
      # adj Max accuracy was 0.700 with mtry = 12
      
      best.mtry <- rf_mtry$bestTune$mtry
      
      print(best.mtry)
      print(max(rf_mtry$results$Accuracy))
      
      results.matrix[stage.num,rm.col] = best.mtry
      rm.col = rm.col + 1
      results.matrix[stage.num,rm.col] = max(rf_mtry$results$Accuracy)
      rm.col = rm.col + 1
      
      # Find best.maxnodes
      
      nodes.seq <- seq(4,30,by=2)
      
      rf.matrix = matrix(nrow = length(nodes.seq),ncol = 2)
      rfm.row <- 1
      
      accuracy.max = 0
      best.maxnodes = 0
      
      tuneGrid <- expand.grid(.mtry = best.mtry)
      
      for (maxnodes in nodes.seq) {
      
        rf_maxnode <- train(cod2_summ~.,
                            data = data_train,
                            method = "rf",
                            metric = "Accuracy",
                            tuneGrid = tuneGrid,
                            trControl = trControl,
                            importance = TRUE,
                            nodesize = 14,
                            maxnodes = maxnodes,
                            ntree = 300)
          
        rf.matrix[rfm.row,1] <- maxnodes
        rf.matrix[rfm.row,2] <- max(rf_maxnode$results$Accuracy)
        
        if (accuracy.max < max(rf_maxnode$results$Accuracy)) {
          accuracy.max <- max(rf_maxnode$results$Accuracy)
          best.maxnodes <- maxnodes
        }
        
        rfm.row <- rfm.row + 1
        
      }
      
      print(best.maxnodes)
      print(accuracy.max)
      
      results.matrix[stage.num,rm.col] = best.maxnodes
      rm.col = rm.col + 1
      results.matrix[stage.num,rm.col] = accuracy.max
      rm.col = rm.col + 1
      
      # Max accuracy was 0.740 with maxnodes = 16
      # adj Max accuracy was 0.751 with maxnodes = 16
      # best_maxnodes = 16
      
      # Now find best number of trees
      
      tree.seq <- c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 800, 1000)
      
      rf.matrix = matrix(nrow = length(tree.seq),ncol = 2)
      rfm.row <- 1
      
      accuracy.max = 0
      best.ntree = 0
      
      for (ntree in tree.seq) {
      
          rf_maxtrees <- train(cod2_summ~.,
                             data = data_train,
                             method = "rf",
                             metric = "Accuracy",
                             tuneGrid = tuneGrid,
                             trControl = trControl,
                             importance = TRUE,
                             nodesize = 14,
                             maxnodes = best.maxnodes,
                             ntree = ntree)
      
          rf.matrix[rfm.row,1] <- ntree
          rf.matrix[rfm.row,2] <- max(rf_maxtrees$results$Accuracy)
          
          if (accuracy.max < max(rf_maxtrees$results$Accuracy)) {
            accuracy.max <- max(rf_maxtrees$results$Accuracy)
            best.ntree <- ntree
          }
          
          rfm.row <- rfm.row + 1
          
      }
      
      print(best.ntree)
      print(accuracy.max)
      
      results.matrix[stage.num,rm.col] = best.ntree
      rm.col = rm.col + 1
      results.matrix[stage.num,rm.col] = accuracy.max
      rm.col = rm.col + 1
      
      # Max accuracy of 0.7457 with 30 trees
      # adj 1 Max accuracy of 0.7457 with 45 trees
      # adj 2 Max accuracy of 0.7514 with 300 trees
      # best_maxtrees = 300
      
    } else {
      #Default results
      results.matrix[stage.num,rm.col] = 0
      rm.col = rm.col + 1
      results.matrix[stage.num,rm.col] = 0
      rm.col = rm.col + 1
      
      best.mtry <- 45
      
      results.matrix[stage.num,rm.col] = best.mtry
      rm.col = rm.col + 1
      results.matrix[stage.num,rm.col] = 0
      rm.col = rm.col + 1
      
      best.maxnodes <- 23
      
      results.matrix[stage.num,rm.col] = best.maxnodes
      rm.col = rm.col + 1
      results.matrix[stage.num,rm.col] = 0
      rm.col = rm.col + 1
      
      best.ntree <- 550

      results.matrix[stage.num,rm.col] = best.ntree
      rm.col = rm.col + 1
      results.matrix[stage.num,rm.col] = 0
      rm.col = rm.col + 1
      
      tuneGrid <- expand.grid(.mtry = best.mtry)
    }
    
    # The best model
    fit_rf <- train(cod2_summ~.,
                    data = data_train,
                    method = "rf",
                    metric = "Accuracy",
                    tuneGrid = tuneGrid,
                    trControl = trControl,
                    importance = TRUE,
                    nodesize = 14,
                    maxnodes = best.maxnodes,
                    ntree = best.ntree)
    
    results.matrix[stage.num,rm.col] = max(fit_rf$results$Accuracy)
    rm.col = rm.col + 1
    
    #############################
    ## Store and graph importance
    #############################
    
    imp <- as.data.frame(varImp(fit_rf)$importance)
    imp$value <- rep(0, nrow(imp)) 
    
    total_imp = sum(imp$`0`)
    
    for (imp_row in 1:nrow(imp)){
      res_row = which(fimp.matrix$feature == rownames(imp)[imp_row])
      fimp.matrix[res_row, stage.num + 2] <- (imp[imp_row,1] / total_imp) * 100
      imp[imp_row,3]  <- (imp[imp_row,1] / total_imp) * 100 
    }
    
    # Remove less import features for clarity
    imp <- subset(imp, value > importance.min)
    
    imp$varnames <- rownames(imp) # row names to column
    rownames(imp) <- NULL  
    
    plot.title = paste0("Relative Feature Importance - Model: ",model.name,", Stage: ",stage)
    
    p <- ggplot(imp, aes(x=reorder(varnames, value), y=value))
    p <- p + geom_point()
    p <- p + geom_segment(aes(x=varnames,xend=varnames,y=0,yend=value))
    p <- p + ggtitle(plot.title)
    p <- p + ylab("Relative Importance")
    p <- p + xlab("Feature")
    p <- p + coord_flip()
    p <- p + theme_classic()
    
    print(p)
    
    ggsave(paste0(results.sub.dir, "/", model.abv, "_feature_importance_",stage, file.suffix,".png"))
    
    #############################
    ## Store confusion matrix
    #############################
    
    results.matrix[stage.num,rm.col] = accuracy_fit(fit_rf, data_test, "raw")
    rm.col = rm.col + 1
    
    print(accuracy_fit(fit_rf, data_test, "raw"))
    
    table_mat <- cmatrix_fit(fit_rf, data_test, "raw")
    
    # Loop over my_matrix
    for(row in 1:nrow(table_mat)) {
      for(col in 1:ncol(table_mat)) {
        results.matrix[stage.num,rm.col] = table_mat[row, col]
        rm.col = rm.col + 1
      }
    }
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
  plot.title = paste0("Relative Feature Importance Heatmap - Model: ",model.name)
  p <- ggplot(data.m.ss, aes(x=variable, y=feature)) 
  p <- p + ggtitle(plot.title)
  p <- p + geom_tile(aes(fill = value))
  p <- p + scale_fill_viridis_c(direction = -1, begin = .3, end = 1)
  p <- p + geom_text(aes(label = round(value, 1)))
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