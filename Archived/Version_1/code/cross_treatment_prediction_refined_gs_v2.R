## DEPRICATED VERSION???

## Testing params
# clf <- 'xgbTree'
# cutoff <- .3 
# ndthresh <- .5 
# baseline <- FALSE 
# y <- 'rrsr'
# nrounds <- 100 
# eta <- .3 
# max_depth <- 4
# gamma <- 0
# colsample_bytree <- 1 
# min_child_weight <- 2
# subsample <- .4

cross_treatment_prediction_refined_gs_v2 <- function(clf, cutoff, ndthresh, baseline, y, rmterms, nrounds, eta, max_depth, gamma, colsample_bytree, min_child_weight, subsample){
  
  ## One Shot Pass ##
  require(caret)
  require(parallel)
  require(doParallel)
  require(randomForest)
  require(pdp)
  require(xgboost)
  require(plyr)
  
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
    trCtrl <- trainControl(method = 'none', 
                           allowParallel = TRUE)
    
    train_fit <- train(outcome ~ .,
                       data = cdf,
                       method = 'xgbTree',
                       trControl = trCtrl,
                       tuneGrid = tune_grid,
                       nthread=1)
      
      
    odir <- '/ifshome/bwade/NARSAD/Aim_1/results/cross_treatment_grid_search/'
    
    # load full dataset for cross-tx validation
    cfid <- dir('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/', pattern = 'Rdata', full.names = TRUE)
    c_to_load <- grep(y, cfid, value = TRUE)
    load(c_to_load)
    
    # fix mismatch with 3rd and 4th ventricle names
    df <- plyr::rename(df, replace=c('3rd_Ventricle'='X3rd_Ventricle', '4th_Ventricle'='X4th_Ventricle'))
    
    if(g=='e'){
      
      ket <- df[df$group=='k',]
      tsd <- df[df$group=='s',]
      
      k_pred <- list(obs=ket$outcome, pred=predict(object = train_fit, newdata=ket))
      s_pred <- list(obs=tsd$outcome, pred=predict(object = train_fit, newdata=tsd))
      
      # ECT -> Ketamine
      save(k_pred, file=sprintf('%s%s_basemodel-%s_predicting-k_cutoff-%s_ndthresh-%s_baseline-%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                                      odir, y, g, cutoff, ndthresh, baseline, nrounds, eta, max_depth, gamma, colsample_bytree, min_child_weight, subsample))
      # ECT -> TSD
      save(s_pred, file=sprintf('%s%s_basemodel-%s_predicting-s_cutoff-%s_ndthresh-%s_baseline-%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                                odir, y, g, cutoff, ndthresh, baseline, nrounds, eta, max_depth, gamma, colsample_bytree, min_child_weight, subsample))
      
    }else if(g=='k'){
      
      ect <- df[df$group=='e',]
      tsd <- df[df$group=='s',]
      
      e_pred <- list(obs=ect$outcome, pred=predict(object = train_fit, newdata=ect))
      s_pred <- list(obs=tsd$outcome, pred=predict(object = train_fit, newdata=tsd))
      
      # Ketamine -> ECT
      save(e_pred, file=sprintf('%s%s_basemodel-%s_predicting-e_cutoff-%s_ndthresh-%s_baseline-%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                                odir, y, g, cutoff, ndthresh, baseline, nrounds, eta, max_depth, gamma, colsample_bytree, min_child_weight, subsample))
      # Ketamine -> TSD
      save(s_pred, file=sprintf('%s%s_basemodel-%s_predicting-s_cutoff-%s_ndthresh-%s_baseline-%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                                odir, y, g, cutoff, ndthresh, baseline, nrounds, eta, max_depth, gamma, colsample_bytree, min_child_weight, subsample))
      
    }else if(g=='s'){
      
      ket <- df[df$group=='k',]
      ect <- df[df$group=='e',]
      
      k_pred <- list(obs=ket$outcome, pred=predict(object = train_fit, newdata=ket))
      e_pred <- list(obs=ect$outcome, pred=predict(object = train_fit, newdata=ect))
      
      # TSD -> Ketamine
      save(k_pred, file=sprintf('%s%s_basemodel-%s_predicting-k_cutoff-%s_ndthresh-%s_baseline-%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                                odir, y, g, cutoff, ndthresh, baseline, nrounds, eta, max_depth, gamma, colsample_bytree, min_child_weight, subsample))
      # TSD -> ECT
      save(e_pred, file=sprintf('%s%s_basemodel-%s_predicting-e_cutoff-%s_ndthresh-%s_baseline-%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                                odir, y, g, cutoff, ndthresh, baseline, nrounds, eta, max_depth, gamma, colsample_bytree, min_child_weight, subsample))
      
    }
    
  })
  
}


