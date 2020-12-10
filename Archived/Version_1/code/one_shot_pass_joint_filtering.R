# Simplified appraoch to do joint feature selection. Compare this to more extensive grid search; it may do better!
# Can also include adaptive grid search here to further simplify things: https://topepo.github.io/caret/adaptive-resampling.html

# Evaluation method below
require(caret)
require(xgboost)

grid <- expand.grid(
  classifier='xgbTree',
  group = c('e', 'k', 's'),
  cutoff = c(.1, .2, .3),
  y = c('hdrs6', 'rrsr'),
  baseline = c(TRUE)
)

odir <- '/ifshome/bwade/NARSAD/Aim_1/results/refinement/adaptive_gridsearch/'

for(i in 1:nrow(grid)){
  
  print(i)
  
  data_to_load <- grep(grid[i, 'y'], dir('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/', pattern = 'Rdata', full.names = TRUE), value = TRUE)
  
  load(data_to_load)
  
  dat <- df[df$group==grid$group[i], ]
  
  nzvdrop <- nzv(dat, names=TRUE)
  otherdrop <- grep('_vessel|5th_Ventricle|CSF|VentralDC|CSF|Optic_', names(dat), value = TRUE)
  
  dat <- dat[, !names(dat) %in% c(nzvdrop, otherdrop, 'screen_id')]
  
  outcome_baseline <- dat$outcome_baseline
  
  tmpdat <- dat[, !names(dat) %in% c('outcome', 'outcome_baseline')]
  
  cordrop <- findCorrelation(cor(tmpdat), cutoff = grid$cutoff[i], names = TRUE)
  
  cdf <- dat[, !names(dat) %in% cordrop]
  
  if(grid$baseline[i]==TRUE){
    if(sum(grepl('outcome_baseline', colnames(cdf)))==0){
      cdf <- data.frame(cdf, outcome_baseline=outcome_baseline) 
    }
  }else{
    cdf <- cdf[, !names(cdf) %in% c('outcome_baseline')]
  }
  
  adaptControl <- trainControl(method = "adaptive_cv",
                               number = 10, repeats = 10,
                               adaptive = list(min = 9, alpha = 0.05, 
                                               method = "gls", complete = TRUE),
                               savePredictions = 'final',
                               verboseIter = TRUE,
                               allowParallel = TRUE,
                               search = "random")
  
  fit <- train(outcome ~ .,
                     data = cdf,
                     #method = grid$classifier[i],
                     method = 'xgbTree',
                     trControl = adaptControl,
                     tuneLength = 20,
                     nthread=1)
  
  
  save(fit, file = sprintf('%s%s_group-%s_y-%s_cutoff-%s_baseline-%s.Rdata', odir, grid$classifier[i], grid$group[i], grid$y[i], grid$cutoff[i], grid$baseline[i]))
  
  rm(fit)
  
}
  

# Evaluate
fid <- dir('/ifshome/bwade/NARSAD/Aim_1/results/refinement/adaptive_gridsearch/', full.names = TRUE)
fid <- grep('baseline-FALSE', fid, value = TRUE)

sapply(fid, function(f){
  load(f)
  cm <- cor(fit$pred$pred, fit$pred$obs)
  return(cm)
})



