# mostly the same as one_shot_pass but used when we need to refine the grid search and find a more finalized model
# expanded parameter space
# use 10-repeated 10-fold cv instead of 1 x 10

cross_treatment_prediction_refined_gs <- function(clf, cutoff, baseline, y, diff_drop, rmterms, nrounds, eta, max_depth, gamma, colsample_bytree, min_child_weight, subsample){
  
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
  
  print('Loading Data...')
  
  datasets <- dir('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/', full.names = TRUE, pattern = 'Rdata')
  data_to_load <- grep(y, datasets, value = T)
  
  load(data_to_load)
  
  if(diff_drop==TRUE){
    df <- df[, !colnames(df) %in% grep('_AD|_MD|_RD|_kurt|_vessel|5th_Ventricle|CSF|VentralDC|CSF|Optic_', colnames(df), value = T)]
  }else{
    df <- df[, !colnames(df) %in% grep('_vessel|5th_Ventricle|CSF|VentralDC|CSF|Optic_', colnames(df), value = T)]
  }
  
  print('Iterating Over Treatments...')
  fitted <- lapply(c('e', 'k', 's'), function(g){
    
    print(g)
    
    dtmp <- df[df$group==g, ]
    
    nzvrm <- nzv(dtmp, names = TRUE)
    gconn_rm <- names(which(apply(dtmp[, grep("Degree", names(dtmp), value = T)], 2, median)==0)) # a lot of global connectivity measures useless, remove these with medians == 0
    to_exclude <- unique(c(nzvrm, gconn_rm))
    to_exclude <- to_exclude[!to_exclude %in% c('cont_age', 'cont_sex', 'outcome','outcome_baseline','group')]
    
    # remove nzvrm and gconn_rm from dtmp
    dtmp <- dtmp[, !names(dtmp) %in% to_exclude]
    
    ## Filter within feature type
    featurelist <- list(
      grep('lh_|rh_', names(df), value = T),
      grep('Right_|Left_', names(df), value = T),
      #grep('_FA', names(df), value = T),
      grep('_FA|_AD|_MD|_RD|_kurt', names(df), value = T),
      grep('Degree', names(df), value = T),
      grep('BnConn', names(df), value = T)
    )
    
    print('Filtering Features...')
    filtered <- lapply(featurelist, function(f){
      
      tmp <- dtmp[, f]
      fc <- findCorrelation(cor(tmp), names=T, cutoff=cutoff)
      return(names(tmp)[!names(tmp) %in% fc])
      
    })
    
    if(baseline==TRUE){
      cdf <- df[df$group==g, c('outcome', 'outcome_baseline', 'cont_age', 'cont_sex', unlist(filtered))]
    }else{
      cdf <- df[df$group==g, c('outcome', 'cont_age', 'cont_sex', unlist(filtered))]
    }
    
    if(ncol(cdf)<3){
      
      return(list())
      
    }else{
      
      print('Processing XGB...')
      # trCtrl <- trainControl(method = 'none', 
      #                        number = 10,
      #                        repeats = 10,
      #                        savePredictions = 'final',
      #                        allowParallel = TRUE)
      
      trCtrl <- trainControl(method = 'none', 
                             allowParallel = TRUE)
      
      train_fit <- train(outcome ~ .,
                         data = cdf,
                         method = 'xgbTree',
                         trControl = trCtrl,
                         tuneGrid = tune_grid,
                         nthread=1)
      
      
      odir <- '/ifshome/bwade/NARSAD/Aim_1/results/cross_treatment_grid_search/'
      
      if(g=='e'){
        
        ket <- df[df$group=='k',]
        tsd <- df[df$group=='s',]
        
        k_pred <- list(obs=ket$outcome, pred=predict(object = train_fit, newdata=ket))
        s_pred <- list(obs=tsd$outcome, pred=predict(object = train_fit, newdata=tsd))
        
        
        # ECT -> Ketamine
        save(k_pred, file=sprintf('%sbasemodel-%s_predicting-k_%s_cutoff-%s_baseline-%s_%s_diffdrop-%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                                        odir, g, grid$classifier[i], grid$cutoff[i], grid$baseline[i], grid$y[i], grid$diff_drop[i],
                                        grid$nrounds[i], grid$eta[i], grid$max_depth[i], grid$gamma[i], grid$colsample_bytree[i], 
                                        grid$min_child_weight[i], grid$subsample[i]))
        # ECT -> TSD
        save(s_pred, file=sprintf('%sbasemodel-%s_predicting-s_%s_cutoff-%s_baseline-%s_%s_diffdrop-%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                                  odir, g, grid$classifier[i], grid$cutoff[i], grid$baseline[i], grid$y[i], grid$diff_drop[i],
                                  grid$nrounds[i], grid$eta[i], grid$max_depth[i], grid$gamma[i], grid$colsample_bytree[i], 
                                  grid$min_child_weight[i], grid$subsample[i]))
        
      }else if(g=='k'){
        
        ect <- df[df$group=='e',]
        tsd <- df[df$group=='s',]
        
        e_pred <- list(obs=ect$outcome, pred=predict(object = train_fit, newdata=ect))
        s_pred <- list(obs=tsd$outcome, pred=predict(object = train_fit, newdata=tsd))
        
        # Ketamine -> ECT
        save(e_pred, file=sprintf('%sbasemodel-%s_predicting-e_%s_cutoff-%s_baseline-%s_%s_diffdrop-%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                                  odir, g, grid$classifier[i], grid$cutoff[i], grid$baseline[i], grid$y[i], grid$diff_drop[i],
                                  grid$nrounds[i], grid$eta[i], grid$max_depth[i], grid$gamma[i], grid$colsample_bytree[i], 
                                  grid$min_child_weight[i], grid$subsample[i]))
        # Ketamine -> TSD
        save(s_pred, file=sprintf('%sbasemodel-%s_predicting-s_%s_cutoff-%s_baseline-%s_%s_diffdrop-%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                                  odir, g, grid$classifier[i], grid$cutoff[i], grid$baseline[i], grid$y[i], grid$diff_drop[i],
                                  grid$nrounds[i], grid$eta[i], grid$max_depth[i], grid$gamma[i], grid$colsample_bytree[i], 
                                  grid$min_child_weight[i], grid$subsample[i]))
        
      }else if(g=='s'){
        
        ket <- df[df$group=='k',]
        ect <- df[df$group=='e',]
        
        k_pred <- list(obs=ket$outcome, pred=predict(object = train_fit, newdata=ket))
        e_pred <- list(obs=ect$outcome, pred=predict(object = train_fit, newdata=ect))
        
        # TSD -> Ketamine
        save(k_pred, file=sprintf('%sbasemodel-%s_predicting-k_%s_cutoff-%s_baseline-%s_%s_diffdrop-%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                                  odir, g, grid$classifier[i], grid$cutoff[i], grid$baseline[i], grid$y[i], grid$diff_drop[i],
                                  grid$nrounds[i], grid$eta[i], grid$max_depth[i], grid$gamma[i], grid$colsample_bytree[i], 
                                  grid$min_child_weight[i], grid$subsample[i]))
        # TSD -> ECT
        save(e_pred, file=sprintf('%sbasemodel-%s_predicting-e_%s_cutoff-%s_baseline-%s_%s_diffdrop-%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                                  odir, g, grid$classifier[i], grid$cutoff[i], grid$baseline[i], grid$y[i], grid$diff_drop[i],
                                  grid$nrounds[i], grid$eta[i], grid$max_depth[i], grid$gamma[i], grid$colsample_bytree[i], 
                                  grid$min_child_weight[i], grid$subsample[i]))
        
      }
      
    }
    
  })
  
}


