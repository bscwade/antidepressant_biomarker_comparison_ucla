## Script to fit final models from refined grid search

# Load parameterizations and pefromances of best models
#write.table(performance, file = '/ifshome/bwade/NARSAD/Aim_1/results/final_models/best_fits_all.txt')
model_grid <- read.table('/ifshome/bwade/NARSAD/Aim_1/results/final_models/best_fits_all.txt', header=T)

odir <- '/ifshome/bwade/NARSAD/Aim_1/results/final_models/'

source('/ifshome/bwade/NARSAD/Aim_1/code/one_shot_pass_refined_gs.R')

cl <- makePSOCKcluster(20)
registerDoParallel(cl)

for(i in 1:nrow(model_grid)){
  
  print(sprintf('model_grid row %s of %s', i, nrow(model_grid)))
  
  print('Loading Data...')
  datasets <- dir('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/', full.names = TRUE, pattern = 'Rdata')
  data_to_load <- grep(model_grid$y[i], datasets, value = T)
  
  load(data_to_load)
  
  df <- df[, !colnames(df) %in% grep('_AD|_MD|_RD|_kurt|_vessel|5th_Ventricle|CSF|VentralDC|CSF|Optic_', colnames(df), value = T)]
  
  
  dtmp <- df[df$group==substr(as.character(model_grid$variable[i]), 6,6), ]
  
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
    fc <- findCorrelation(cor(tmp), names=T, cutoff=model_grid$cutoff[i])
    return(names(tmp)[!names(tmp) %in% fc])
    
  })
  
  if(model_grid$baseline[i]==TRUE){
    cdf <- df[df$group==substr(as.character(model_grid$variable[i]), 6,6), c('outcome', 'outcome_baseline', 'cont_age', 'cont_sex', unlist(filtered))]
  }else{
    cdf <- df[df$group==substr(as.character(model_grid$variable[i]), 6,6), c('outcome', 'cont_age', 'cont_sex', unlist(filtered))]
  }
  
  # make tuning grid
  tune_grid <- data.frame(
    nrounds=model_grid$nrounds[i],
    eta=model_grid$eta[i],
    max_depth=model_grid$maxdepth[i],
    gamma=model_grid$gamma[i],
    colsample_bytree=model_grid$colsamp[i],
    min_child_weight=model_grid$childweight[i],
    subsample=model_grid$subsamp[i]
  )
  
    
  trCtrl <- trainControl(method = 'repeatedcv', 
                         number = 10,
                         repeats = 10,
                         savePredictions = 'final',
                         allowParallel = TRUE)
  
  set.seed(i)
  fit <- train(outcome ~ .,
                    data = cdf,
                    method = 'xgbTree',
                    trControl = trCtrl,
                    tuneGrid = tune_grid,
                    nthread=1)
  
  
  save(fit, file=sprintf('%s%s_cutoff-%s_baseline-%s_%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                               odir, model_grid$variable[i], model_grid$cutoff[i], model_grid$baseline[i], model_grid$y[i],model_grid$nrounds[i], model_grid$eta[i],
                               model_grid$maxdepth[i], model_grid$gamma[i], model_grid$colsamp[i], model_grid$childweight[i], model_grid$subsamp[i]))
  
  
}

stopCluster(cl)
registerDoSEQ()


# check consistency with older models since randomness is a factor here
fid <- dir(odir, pattern='.Rdata', full.names = TRUE)

for(i in 1:length(fid)){
  
  load(fid[i])
  
  print(fid[i])
  print(cor(fit$pred$obs, fit$pred$pred))
  
}

require(xgboost)
xgbimp <- xgb.importance(feature_names=fit$finalModel$feature_names, model=fit$finalModel)
plot(varImp(fit))
print(cor(fit$pred$obs, fit$pred$pred))

roi_probe <- 'V34_V143_BnConn'
partial(fit, pred.var = roi_probe, ice = TRUE, center = TRUE, 
        plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "ggplot2", 
        train = fit$trainingData)











