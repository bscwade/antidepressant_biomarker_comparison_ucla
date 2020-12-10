
performance_by_feature_and_n <- function(feature, n, group, y){
  
  diff_drop <- TRUE
  
  datasets <- dir('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/', full.names = TRUE, pattern = 'Rdata')
  data_to_load <- grep(y, datasets, value = T)
  
  load(data_to_load)
  
  if(diff_drop==TRUE){
    df <- df[, !colnames(df) %in% grep('_AD|_MD|_RD|_kurt|_vessel|5th_Ventricle|CSF|VentralDC|CSF|Optic_', colnames(df), value = T)]
  }else{
    df <- df[, !colnames(df) %in% grep('_vessel|5th_Ventricle|CSF|VentralDC|CSF|Optic_', colnames(df), value = T)]
  }  
  
  dtmp <- df[df$group==group, ]
  
  if(feature=='diffusion'){
    vois <- grep('_FA', names(df), value = T)
  }else if(feature=='asegaparc'){
    vois <- grep('lh_|rh_|Right_|Left_', names(df), value = T)
  }else if(feature=='globalconn'){
    vois <- grep('Degree', names(df), value = T)
  }else if(feature=='betweennetwork'){
    vois <- grep('BnConn', names(df), value = T)
  }
  
  if(length(vois) < n){
    return(NA)
  }else{
    
    data_to_reduce <- dtmp[, vois]
    
    cutoff=.99
    
    while(ncol(data_to_reduce) >= n){
      
      cutoff <- cutoff - 0.05
      print(cutoff)
      
      drop <- findCorrelation(cor(data_to_reduce), cutoff, names = TRUE)
      
      data_to_reduce <- data_to_reduce[, !names(data_to_reduce) %in% drop]
      
    }
    
    cdf <- data.frame(outcome=dtmp$outcome, data_to_reduce)
    
    trCtrl <- trainControl(method = 'repeatedcv', 
                           number = 10,
                           repeats = 1,
                           savePredictions = 'final',
                           allowParallel = FALSE)
    
    fit <- train(outcome ~ .,
                       data = cdf,
                       method = 'xgbTree',
                       trControl = trCtrl,
                       nthread=1,
                       ntree = 1000,
                       tuneLength=5)
    
    
    return(cor(fit$pred$obs, fit$pred$pred))
    
  } # if statement
  
  
}










