## Updated to load pre-compiled datasets to avoid filtering online

# clf <- 'rf'
# cutoff <- 0.5
# ndthresh <- 0.5
# baseline <- TRUE
# diff_drop <- FALSE
# rmterms <- TRUE
# y <- 'rrsr'

one_shot_pass_v2 <- function(clf, cutoff, ndthresh, baseline, y, diff_drop, rmterms){
  
  ## One Shot Pass ##
  require(caret)
  require(parallel)
  require(doParallel)
  require(randomForest)
  require(pdp)
  require(xgboost)
  
  print('Iterating Over Treatments...')
  fitted <- lapply(c('e', 'k', 's'), function(g){
    
    print(g)
    
    print('Loading Data...')
    fid <- dir('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/hyper_param_specific/V2_Data/', pattern='Rdata', full.names = TRUE)
    #fid <- grep('diffdrop-FALSE', fid, value = TRUE)
    
    data_to_load <- grep(paste0('ndthresh-', ndthresh), grep(paste0('group-', g), grep(paste0('y-', y), grep(paste0('cutoff-', cutoff), fid, value = T), value = TRUE), value = TRUE), value = TRUE)
    
    load(data_to_load)
    
    if(baseline==FALSE){
      cdf <- subset(cdf, select = -outcome_baseline)
    }
      
    if(clf=='xgbTree'){
      print('Processing XGB...')
      trCtrl <- trainControl(method = 'repeatedcv', 
                             number = 10,
                             repeats = 1,
                             savePredictions = 'final',
                             allowParallel = FALSE)
      
      train_fit <- train(outcome ~ .,
                         data = cdf,
                         method = clf,
                         trControl = trCtrl,
                         nthread=1,
                         ntree = 1000,
                         tuneLength=5)
      
    }else{
      print('Non XGB Model...')
      trCtrl <- trainControl(method = 'repeatedcv', 
                             number = 10,
                             repeats = 1,
                             savePredictions = 'final',
                             allowParallel = TRUE)
      
      train_fit <- train(outcome ~ .,
                         data = cdf,
                         method = clf,
                         trControl = trCtrl,
                         ntree = 1000,
                         tuneLength=5)
    }
      
      if(rmterms==TRUE){ # space saver
        train_fit$terms <- NULL
      }
      
      return(list(
        mod=train_fit,
        data=cdf
      ))
      
    
  })
  
  return(fitted)
  
}


