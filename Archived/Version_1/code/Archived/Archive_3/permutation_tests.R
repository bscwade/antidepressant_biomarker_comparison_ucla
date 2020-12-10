## script to use permutation testing to test how significant PvAc's are for optimized models
## jointly shuffle outcome and predictors
## Some considerations: 1. Do I need to permute the whole of the process instead? Shuffling data before feature selection may be needed since selection first then reshuffling suggests info leakage.

# Load parameterizations and pefromances of best models
model_grid <- read.table('/ifshome/bwade/NARSAD/Aim_1/results/final_models/best_fits.txt')

odir <- '/ifshome/bwade/NARSAD/Aim_1/results/permutation_tests/'

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
  
  perm_dist <- list()
  
  for(p in 1:1000){
    
    print(p)
    
    set.seed(p)
    permuted_data <- sapply(1:ncol(cdf), function(c){
      sample(cdf[, c], size = nrow(cdf), replace = FALSE)
    })
    colnames(permuted_data) <- colnames(cdf)
  
    trCtrl <- trainControl(method = 'repeatedcv', 
                           number = 10,
                           repeats = 10,
                           savePredictions = 'final',
                           allowParallel = TRUE)
    
    perm_fit <- train(outcome ~ .,
                       data = permuted_data,
                       method = 'xgbTree',
                       trControl = trCtrl,
                       tuneGrid = tune_grid,
                       nthread=1)
  
    perm_dist[[p]] <- cor(perm_fit$pred$obs, perm_fit$pred$pred)
    
  }
  
  
  save(perm_dist, file=sprintf('%spermutation_distribution_cutoff-%s_baseline-%s_%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                 odir, model_grid$cutoff[i], model_grid$baseline[i], model_grid$y[i],model_grid$nrounds[i], model_grid$eta[i],
                 model_grid$maxdepth[i], model_grid$gamma[i], model_grid$colsamp[i], model_grid$childweight[i], model_grid$subsamp[i]))
       
  
}

stopCluster(cl)
registerDoSEQ()




dir(odir)

model_grid


load('/ifshome/bwade/NARSAD/Aim_1/results/permutation_tests/permutation_distribution_cutoff-0.1_baseline-TRUE_hdrs6_nrounds-500_eta-0.025_max_depth-6_gamma-0.3_colsamp-0.4_childwei-1_subsamp-1.Rdata')

hist(unlist(perm_dist))




