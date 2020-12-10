## Updated cross-treatment prediction function to handle parameter identifiers in fid string and write/save with same format

# fid <- dir('/ifshome/bwade/NARSAD/Aim_1/results/refinement/', full.names = TRUE)[352]
# load(fid)
# cor(fits[[1]]$mod$pred$pred, fits[[1]]$mod$pred$obs)


cross_treatment_prediction <- function(fid){
  
  ## One Shot Pass ##
  require(caret)
  require(parallel)
  require(doParallel)
  require(randomForest)
  require(pdp)
  require(xgboost)
  require(plyr)
  
  # pull parameters from fid string
  ss <- strsplit(fid, '_')[[1]]
  
  tune_grid <- expand.grid(
    nrounds = gsub('nrounds-', '', ss[[grep('nrounds-', ss)]]),
    eta = gsub('eta-', '', ss[[grep('eta-', ss)]]),
    max_depth = gsub('depth-', '', ss[[grep('depth-', ss)]]),
    gamma = gsub('gamma-', '', ss[[grep('gamma-', ss)]]),
    colsample_bytree = gsub('colsamp-', '', ss[[grep('colsamp-', ss)]]),
    min_child_weight = gsub('childwei-', '', ss[[grep('childwei-', ss)]]),
    subsample = gsub('subsamp-|.Rdata', '', ss[[grep('subsamp-', ss)]])
  )
  print(tune_grid)
  
  # load all data
  y <- ss[[grep('hdrs6|rrsr|rrsb|tcqr', ss)]]
  print('Loading Data...')
  fid_data <- dir('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/', pattern='Rdata', full.names = TRUE)
  data_to_load <- grep(y, fid_data, value = T)
  load(data_to_load)
  
  # baseline
  baseline <- gsub('baseline-', '', ss[[grep('baseline-', ss)]])
  
  # load fitted models
  load(fid)
  
  print('Iterating Over Treatments...')
  fitted <- lapply(c('e', 'k', 's'), function(g){
    
    print(g)
    
    odir <- '/ifshome/bwade/NARSAD/Aim_1/results/cross_treatment_grid_search/'
    out_fid <- gsub('/ifshome/bwade/NARSAD/Aim_1/results/refinement//', odir, fid)
    
    # fix mismatch with 3rd and 4th ventricle names
    df <- plyr::rename(df, replace=c('3rd_Ventricle'='X3rd_Ventricle', '4th_Ventricle'='X4th_Ventricle'))
    
    if(g=='e'){
      
      train_fit <- fits[[1]]$mod
      
      cmtest <- cor(train_fit$pred$pred, train_fit$pred$obs)
      
      ket <- df[df$group=='k',]
      tsd <- df[df$group=='s',]
      
      if(cmtest > .1){
        k_pred <- list(obs=ket$outcome, pred=predict(object = train_fit, newdata=ket))
        s_pred <- list(obs=tsd$outcome, pred=predict(object = train_fit, newdata=tsd))
      }else{
        k_pred <- NA
        s_pred <- NA
      }
      
      # ECT -> Ketamine
      save(k_pred, file=gsub('xgbTree_', 'xgbTree_e_predicting_k_', out_fid))
  
      # ECT -> TSD
      save(s_pred, file=gsub('xgbTree_', 'xgbTree_e_predicting_s_', out_fid))
      
    }else if(g=='k'){
      
      train_fit <- fits[[2]]$mod
      
      cmtest <- cor(train_fit$pred$pred, train_fit$pred$obs)
      
      ect <- df[df$group=='e',]
      tsd <- df[df$group=='s',]
      
      if(cmtest > .1){
        e_pred <- list(obs=ect$outcome, pred=predict(object = train_fit, newdata=ect))
        s_pred <- list(obs=tsd$outcome, pred=predict(object = train_fit, newdata=tsd))  
      }else{
        e_pred <- NA
        s_pred <- NA
      }
      
      # Ketamine -> ECT
      save(e_pred, file=gsub('xgbTree_', 'xgbTree_k_predicting_e_', out_fid))
      
      # Ketamine -> TSD
      save(s_pred, file=gsub('xgbTree_', 'xgbTree_k_predicting_s_', out_fid))
      
    }else if(g=='s'){
      
      train_fit <- fits[[3]]$mod
      
      cmtest <- cor(train_fit$pred$pred, train_fit$pred$obs)
      
      ket <- df[df$group=='k',]
      ect <- df[df$group=='e',]
      
      if(cmtest > .1){
        k_pred <- list(obs=ket$outcome, pred=predict(object = train_fit, newdata=ket))
        e_pred <- list(obs=ect$outcome, pred=predict(object = train_fit, newdata=ect))  
      }else{
        k_pred <- NA
        e_pred <- NA
      }
      
      
      # TSD -> Ketamine
      save(k_pred, file=gsub('xgbTree_', 'xgbTree_s_predicting_k_', out_fid))
      
      # TSD -> ECT
      save(e_pred, file=gsub('xgbTree_', 'xgbTree_s_predicting_e_', out_fid))
      
    }
    
  })
  
}

# Run function
model_fid <- dir('/ifshome/bwade/NARSAD/Aim_1/results/refinement/', full.names=T, pattern = 'Rdata')

for(m in 551:length(model_fid)){
  
  print(m)
  
  # if this was already done skip to next
  check <- grep(strsplit(model_fid[m], '_cutoff')[[1]][2], dir('/ifshome/bwade/NARSAD/Aim_1/results/cross_treatment_grid_search/'), value = T)
  
  if(length(check) > 0){
    next
  }else{
    tryCatch(cross_treatment_prediction(fid = model_fid[m]), error = function(e) print('error: skipping to next'))
  }
  
}



