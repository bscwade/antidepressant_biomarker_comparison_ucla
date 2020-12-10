# Notes: 
# May want to include demographic predictors too!
# why was bootstrapping in trCtrl Rsquared so low compared to 10fold CV?

predict_co <- function(y, df, nboot){
  
  # y = np outcome data
  # X = set of predictors
  
  require(caret)
  require(parallel)
  require(doParallel)
  cl2 <- makeCluster(25)
  doParallel::registerDoParallel(cl2)
  
  df <- df[, !names(df) %in% names(df)[caret::nzv(df)]]
  
  # Clean and format data
  X <- df[, !names(df) %in% c(y, 'group', 'screen_id', 'timepoint', 'uid')]
  
  # Designate model outcome
  outcome <- y
  
  # for now, keep only complete
  regdat <- df[, c(outcome, names(X))]
  
  ## Create model
  trCtrl <- trainControl(method = 'cv', 
                         number = 10,
                         allowParallel = TRUE)
  
  fit_list <- lapply(1:nboot, function(boot_index){
    
    print(sprintf('Bootstrap Repetition %s / %s', boot_index, nboot))
    
    # Partition train and testing data
    set.seed(boot_index)
    
    ind <- sample(1:nrow(df), size = round(nrow(df)*.8), replace = FALSE)
    
    train_data <- regdat[1:nrow(df) %in% ind, ]
    test_data <- regdat[!1:nrow(df) %in% ind, ]
    
    print(dim(train_data))
    print(dim(test_data))
    
    # Fit model to training data
    model <- as.formula(sprintf('%s ~ .', outcome))
    
    train_fit <- train(model, 
                       data = train_data, 
                       method = 'rf', 
                       metric = 'RMSE', 
                       trControl = trCtrl,
                       ntree = 1000,
                       tuneLength=5)
    
    pred <- predict(object = train_fit, newdata = test_data)
    
    actual <- test_data[[outcome]]
    
    # Get ordering of variable importance
    vimp <- train_fit$finalModel$importance
    vimp <- data.frame(vois=rownames(vimp), vimp)
    rownames(vimp) <- NULL
    vimp_df <- vimp[order(vimp$IncNodePurity, decreasing = T),]
    
    return(list(
      predicted = pred,
      actual = actual,
      #features = selected_variables,
      N = nrow(df),
      nTrain = nrow(train_data),
      test_index=ind,
      variable_importance=vimp_df
    ))
    
  })
  
  return(fit_list)
  
  stopCluster(cl2)
  
}


