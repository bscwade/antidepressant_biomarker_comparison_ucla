
## Collect PvAc pairs
fid_cross <- dir('/ifshome/bwade/NARSAD/Aim_1/results/cross_treatment_grid_search/', full.names = TRUE, pattern='Rdata')

cpairs <- lapply(1:length(fid_cross), function(f){
  
  print(f)
  
  rm(e_pred)
  rm(k_pred)
  rm(s_pred)
  rm(idx)
  
  # load results of cross-treatment prediction
  load(fid_cross[f])
  
  # use cross-treatment fid string to find corresponding base model from refined grid search
  fid_base_model <- gsub('/ifshome/bwade/NARSAD/Aim_1/results/cross_treatment_grid_search//xgbTree_[eks]_predicting_[eks]_', '', fid_cross[f])
  
  check <- length(grep(fid_base_model, dir('/ifshome/bwade/NARSAD/Aim_1/results/refinement/', full.names=TRUE), value = TRUE))
  
  if(check>0){
    
    load(grep(fid_base_model, dir('/ifshome/bwade/NARSAD/Aim_1/results/refinement/', full.names=TRUE), value = TRUE))
    
    # find base model treatment arm
    ss <- strsplit(fid_cross[f], '_')[[1]]
    base_model_arm <- ss[[grep('xgbTree', ss)+1]]
    
    if(base_model_arm=='e'){
      idx <- 1
    }else if(base_model_arm=='k'){
      idx <- 2
    }else if(base_model_arm=='s'){
      idx <- 3
    }
    
    if(sum(grepl('e_pred', ls()))==1){
      cm <- list(
        base=cor(fits[[idx]]$mod$pred$pred, fits[[idx]]$mod$pred$obs),
        cross=tryCatch(cor(e_pred$pred, e_pred$obs), error=function(e) NA)
      )
    }else if(sum(grepl('k_pred', ls()))==1){
      cm <- list(
        base=cor(fits[[idx]]$mod$pred$pred, fits[[idx]]$mod$pred$obs),
        cross=tryCatch(cor(k_pred$pred, k_pred$obs), error=function(e) NA)
      )
    }else if(sum(grepl('s_pred', ls()))==1){
      cm <- list(
        base=cor(fits[[idx]]$mod$pred$pred, fits[[idx]]$mod$pred$obs),
        cross=tryCatch(cor(s_pred$pred, s_pred$obs), error=function(e) NA)
      )
    }
    
    }else{
      
      cm <- list(
        base=NA,
        cross=NA
      )
      
    }
  
  return(cm)
  
})

save(cpairs, file = '/ifshome/bwade/NARSAD/Aim_1/results/correlation_pairs_v2.Rdata')


## Collect associated parameters
param_list <- lapply(fid_cross, function(f){
  
  ss <- strsplit(f, '_')[[1]]
  
  tmp <- data.frame(
    y=ss[grep('hdrs6|rrsr', ss)],
    baseline=gsub('baseline-','',ss[grep('baseline', ss)]),
    cutoff=gsub('cutoff-','',ss[grep('cutoff-', ss)]),
    ndthresh=gsub('ndthresh-','',ss[grep('ndthresh-', ss)]),
    nrounds=gsub('nrounds-','',ss[grep('nrounds-', ss)]),
    eta=gsub('eta-','',ss[grep('eta-', ss)]),
    maxdepth=gsub('depth-','',ss[grep('depth-', ss)]),
    gamma=gsub('gamma-','',ss[grep('gamma-', ss)]),
    colsamp=gsub('colsamp-','',ss[grep('colsamp-', ss)]),
    childweight=gsub('childwei-','',ss[grep('childwei-', ss)]),
    subsample=gsub('subsamp-|.Rdata','',ss[grep('subsamp-', ss)]),
    basemodel=ss[[grep('xgbTree', ss)+1]], 
    crosstx=ss[[grep('predicting', ss)+1]]
  )
  
})

param_list <- do.call(rbind, param_list)

df <- data.frame(param_list, base=sapply(cpairs, function(x) x$base), cross=sapply(cpairs, function(x) x$cross))
df$baseline <- as.factor(df$baseline)
df$y <- as.factor(df$y)
df$crosstx <- factor(df$crosstx, levels = c('e', 'k', 's'))
df$basemodel <- factor(df$basemodel, levels = c('e', 'k', 's'))

ggplot(df, aes(base, cross, fill=baseline)) + 
  geom_point(aes(colour=baseline)) + 
  facet_grid(basemodel~crosstx) + 
  geom_hline(yintercept = 0, col='black', lty=2) + 
  geom_vline(xintercept = 0, col='black', lty=2) + 
  theme_bw()

#save(df, file = '/ifshome/bwade/NARSAD/Aim_1/results/cross_correlation_results_with_parameters_v2.Rdata')







