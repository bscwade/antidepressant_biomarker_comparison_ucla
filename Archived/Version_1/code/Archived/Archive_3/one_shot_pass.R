one_shot_pass <- function(clf, cutoff, baseline, y, diff_drop, rmterms){
  
  ## One Shot Pass ##
  require(caret)
  require(parallel)
  require(doParallel)
  require(randomForest)
  require(pdp)
  require(xgboost)
  
  # cl2 <- makeCluster(25)
  # doParallel::registerDoParallel(cl2)
  # cl <- makePSOCKcluster(20)
  # registerDoParallel(cl)
  
  print('Loading Data...')
  
  datasets <- dir('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/', full.names = TRUE, pattern = 'Rdata')
  data_to_load <- grep(y, datasets, value = T)
  
  load(data_to_load)
  
  # drop tsd control participants 
  
  
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
      grep('lh_|rh_', names(dtmp), value = T),
      grep('Right_|Left_', names(dtmp), value = T),
      #grep('_FA', names(dtmp), value = T),
      grep('_FA|_AD|_MD|_RD|_kurt', names(dtmp), value = T),
      grep('Degree', names(dtmp), value = T),
      grep('BnConn', names(dtmp), value = T)
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
      
    }
    
  })
  
  # stopCluster(cl)
  # registerDoSEQ()
  
  return(fitted)
  
}


