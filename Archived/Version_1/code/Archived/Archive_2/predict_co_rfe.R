# Notes: 
# May want to include demographic predictors too!
# why was bootstrapping in trCtrl Rsquared so low compared to 10fold CV?

predict_co_rfe <- function(y, df, nboot, quantile_threshold, ntree=1000, rfetree=500, nrfe=5, var_freq=8, custom_of=FALSE, RFE=TRUE, cbf=TRUE, clf){

  require(caret)
  require(parallel)
  require(doParallel)
  cl2 <- makeCluster(25)
  doParallel::registerDoParallel(cl2)
  
  df <- df[, !names(df) %in% names(df)[caret::nzv(df)]]
  
  # Clean and format data
  X <- df[, !names(df) %in% c(y, 'group', 'screen_id', 'timepoint', 'uid', 'redcap_event_name')]
  
  # optional correlation-based filtering: not w.r.t. outcome
  if(cbf==TRUE){
    
    idx_drop <- findCorrelation(X, names = TRUE)
    X <- X[, !names(X) %in% idx_drop]

  }
  
  # Designate model outcome
  outcome <- y
  
  # for now, keep only complete
  regdat <- df[, c(outcome, names(X))]

  
  ## Custom summary function ##
  corrmsesummary <- function(data,lev=NULL,model=NULL){
    out <- (cor(data$obs, data$pred)*10 - caret::RMSE(data$obs, data$pred)) # correlation - rmse
    names(out) <- 'CorRMSE'
    out
  }
  
  if(custom_of==TRUE){
    ## Create model
    trCtrl <- trainControl(method = 'cv', 
                           number = 10,
                           summaryFunction = corrmsesummary,
                           savePredictions = TRUE,
                           allowParallel = TRUE)
  }else{
    ## Create model
    trCtrl <- trainControl(method = 'cv', 
                           number = 10,
                           savePredictions = TRUE,
                           allowParallel = TRUE)
  }
  
  fit_list <- lapply(1:nboot, function(boot_index){
    
    print(sprintf('Bootstrap Repetition %s / %s', boot_index, nboot))
    
    # Partition train and testing data
    set.seed(boot_index)
    
    ind <- sample(1:nrow(df), size = round(nrow(df)*.9), replace = FALSE)
    
    train_data <- regdat[1:nrow(df) %in% ind, ]
    test_data <- regdat[!1:nrow(df) %in% ind, ]
    
    # RFE feature selection
    if(RFE==TRUE){
    
      rfeVars <- lapply(1:nrfe, function(r){
        
        set.seed(r)
        inRFE <- createFolds(train_data[[outcome]], k = 10, list = FALSE)
        
        print(sprintf('RFE Iteration %s', r))
        rfe_dat <- train_data[inRFE != r, ]
        rfFuncs$fit <- function(x,y,first,last,...) {library(randomForest); randomForest(x, y, importance = first, ntree = 200, ...) } # 1K trees for RFE
        
        if(custom_of==TRUE){
          print("Using Custom Objective Function for RFE")
          rfFuncs$summary <- corrmsesummary
          control <- rfeControl(functions=rfFuncs, method="cv", number=10, allowParallel = TRUE)
          # results <- rfe(subset(rfe_dat, select = -get(outcome)), rfe_dat[[outcome]],
          #                sizes = c( round(seq(from=1, to = ncol(rfe_dat), length.out = 20)) ),
          #                rfeControl = control, metric = 'CorRMSE', maximize = TRUE)
          results <- rfe(x=subset(rfe_dat, select = -outcome), y=rfe_dat[['outcome']], 
                         sizes = c( round(seq(from=1, to = ncol(rfe_dat), length.out = 20)) ), 
                         rfeControl = control, metric = 'CorRMSE', maximize = TRUE)
        }else{
          control <- rfeControl(functions=rfFuncs, method="cv", number=10, allowParallel = TRUE)
          # results <- rfe(subset(rfe_dat, select = -get(outcome)), rfe_dat[[outcome]], 
          #                sizes = c( round(seq(from=1, to = ncol(rfe_dat), length.out = 20)) ), 
          #                rfeControl = control, metric = 'RMSE') 
          results <- rfe(x=subset(rfe_dat, select = -outcome), y=rfe_dat[['outcome']], 
                         sizes = c( round(seq(from=1, to = ncol(rfe_dat), length.out = 20)) ), 
                         rfeControl = control, metric = 'CorRMSE', maximize = TRUE)
        }
        return(results$optVariables)
        
      })
      
      tab <- table(unlist(rfeVars))
      #selected_variables <- names(tab)[tab>=quantile(tab, quantile_threshold)] 
      selected_variables <- names(tab)[tab>=var_freq]
      
    }else{
      selected_variables <- colnames(train_data)[!colnames(train_data) %in% outcome]
    }
    
    while( length(selected_variables) < 3 ){
      var_freq <- var_freq-1
      selected_variables <- names(tab)[tab>=var_freq]
      print(sprintf('Too few variables; reducing var_freq to %s', var_freq))
      
      if(var_freq < 1){
        print('returning minimum feature set')
        break
      }
      
    }
    
    # Fit model to training data
    model <- as.formula(sprintf('%s ~ .', outcome))
    
    if(custom_of==TRUE){
      print("Training with custom objective function...")
      train_fit <- train(model, 
                         data = train_data[, c(outcome, selected_variables)], 
                         method = clf, 
                         metric = 'CorRMSE', 
                         maximize = TRUE,
                         trControl = trCtrl,
                         ntree = ntree,
                         tuneLength=5)
      
    }else{
      train_fit <- train(model, 
                         data = train_data[, c(outcome, selected_variables)], 
                         method = clf, 
                         metric = 'RMSE', 
                         trControl = trCtrl,
                         ntree = ntree,
                         tuneLength=5) 
    }
    
    pred <- predict(object = train_fit, newdata = test_data[, c(outcome, selected_variables)])
    
    actual <- test_data[[outcome]]
    
    # get training performance
    train_param <- train_fit$bestTune[[1]]
    train_actual <- train_fit$pred[train_fit$pred$mtry==train_param, 'obs']
    train_predicted <- train_fit$pred[train_fit$pred$mtry==train_param, 'pred']
    
    return(list(
      predicted = pred,
      actual = actual,
      train_actual = train_actual,
      train_predicted = train_predicted,
      features = selected_variables,
      N = nrow(df),
      nTrain = nrow(train_data),
      test_index=ind,
      var_freq=var_freq,
      varimp=train_fit$finalModel$importance
    ))
    
  })
  
  return(fit_list)
  
  stopCluster(cl2)
  
}


