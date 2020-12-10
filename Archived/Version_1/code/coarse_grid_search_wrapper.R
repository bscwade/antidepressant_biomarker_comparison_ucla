## Coarse grid search to identify hyperparameters: models, ndthresh, correlation cutoff
require(parallel)
require(doParallel)
require(caret)

# Coarse tune grid
grid <- expand.grid(classifier=c('rf', 'xgbTree', 'svmRadial'),
                    cutoff=c(0.1, 0.2, 0.3, 0.4, 0.5),
                    ndthresh=c(0.5, 0.6, 0.7, 0.8, 0.9),
                    baseline=c(TRUE, FALSE),
                    y=c('rrsr', 'rrsb', 'hdrs6'),
                    diff_drop=FALSE,
                    rmterms=TRUE)

source('/ifshome/bwade/NARSAD/Aim_1/code/one_shot_pass_v2.R')

odir <- '/ifshome/bwade/NARSAD/Aim_1/results/gridsearch/'

cl <- makePSOCKcluster(20)
registerDoParallel(cl)

for(i in 1:nrow(grid)){
  
  print(i)
  
  if(file.exists(sprintf('%s%s_cutoff-%s_ndthresh-%s_baseline-%s_%s_diffdrop-%s.Rdata', odir, grid$classifier[i], grid$cutoff[i], grid$ndthresh[i], grid$baseline[i], grid$y[i], grid$diff_drop[i]))){
    
    next
    
  }else{
    
    fits <- one_shot_pass_v2(clf = grid$classifier[i], cutoff = grid$cutoff[i], ndthresh = grid$ndthresh[i], baseline = grid$baseline[i], y = grid$y[i], diff_drop = grid$diff_drop[i], rmterms=grid$rmterms[i])
    
    save(fits, file=sprintf('%s%s_cutoff-%s_ndthresh-%s_baseline-%s_%s_diffdrop-%s.Rdata', odir, grid$classifier[i], grid$cutoff[i], grid$ndthresh[i], grid$baseline[i], grid$y[i], grid$diff_drop[i])) 
    
  }
  
}

stopCluster(cl)
registerDoSEQ()
