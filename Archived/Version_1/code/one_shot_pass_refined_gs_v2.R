# mostly the same as one_shot_pass but used when we need to refine the grid search and find a more finalized model
# expanded parameter space
# use 10-repeated 10-fold cv instead of 1 x 10
# Updated to load compiled datasets according to hyperparameters in /ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/hyper_param_specific/

# # for testing ##
# clf <- 'xgbTree'
# cutoff <- 0.2
# ndthresh <- 0.7
# baseline <- FALSE
# y <- 'rrsr'
# rmterms <- FALSE
# nrounds <- 100
# eta <- 0.025
# max_depth <- 2
# gamma <- 0
# colsample_bytree <- 0.4
# min_child_weight <- 2
# subsample <- 0.1

one_shot_pass_refined_gs_v2 <- function(clf, cutoff, ndthresh, baseline, y, rmterms, nrounds, eta, max_depth, gamma, colsample_bytree, min_child_weight, subsample){
  
  ## One Shot Pass ##
  require(caret)
  require(parallel)
  require(doParallel)
  require(randomForest)
  require(pdp)
  require(xgboost)
  
  #Set up grid parameters
  tune_grid <- expand.grid(
    nrounds = nrounds,
    eta = eta,
    max_depth = max_depth,
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    min_child_weight = min_child_weight,
    subsample = subsample
  )
  
  print(tune_grid)
  
  print('Iterating Over Treatments...')
  fitted <- lapply(c('e', 'k', 's'), function(g){
    
    print(g)
    
    print('Loading Data...')
    fid <- dir('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/hyper_param_specific/V2_Data/', pattern='Rdata', full.names = TRUE)
    data_to_load <- grep(paste0('cutoff-', cutoff), grep(paste0('ndthresh-', ndthresh), grep(paste0('group-', g), grep(y, fid, value = TRUE), value = TRUE), value = TRUE), value = TRUE)
    
    load(data_to_load)
    
    if(baseline==FALSE){
      cdf <- subset(cdf, select = -outcome_baseline)
    }
      
    print('Processing XGB...')
    trCtrl <- trainControl(method = 'repeatedcv', 
                           number = 10,
                           repeats = 10,
                           savePredictions = 'final', 
                           verboseIter = TRUE,
                           allowParallel = TRUE)
    
    set.seed(32134)
    train_fit <- train(outcome ~ .,
                       data = cdf,
                       method = 'xgbTree',
                       trControl = trCtrl,
                       tuneGrid = tune_grid,
                       nthread=1)
      
    # space saver
    test_cor <- cor(train_fit$pred$pred, train_fit$pred$obs)
    if(test_cor<.1){ 
        train_fit$terms <- NULL
      }
      
    return(list(
      mod=train_fit,
      #data=cdf
      data=c('precompiled')
    ))
    
  })
  
  return(fitted)
  
}


