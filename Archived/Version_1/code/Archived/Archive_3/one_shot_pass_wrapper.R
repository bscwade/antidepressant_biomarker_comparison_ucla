## outcomes
# "hdrs6"
#"rumq_rrsb_total_st" 
#"rumq_rrsr_total_st" 
#"rumq_tcqr_total_st"

# # Coarse tune grid
grid <- expand.grid(classifier=c('rf', 'xgbTree', 'svmRadial'),
                    cutoff=c(.1, .3, .5, .7),
                    baseline=c(TRUE, FALSE),
                    y=c('rrsr', 'hdrs6'),
                    diff_drop=FALSE,
                    rmterms=TRUE)

source('/ifshome/bwade/NARSAD/Aim_1/code/one_shot_pass_v2.R')

odir <- '/ifshome/bwade/NARSAD/Aim_1/results/gridsearch/'

# cl <- makePSOCKcluster(20)
# registerDoParallel(cl)

for(i in 1:nrow(grid)){
  
  print(i)
  
  if(file.exists(sprintf('%s%s_cutoff-%s_baseline-%s_%s_diffdrop-%s.Rdata', odir, grid$classifier[i], grid$cutoff[i], grid$baseline[i], grid$y[i], grid$diff_drop[i]))){
    
    next
    
  }else{
    
    fits <- one_shot_pass_v2(clf = grid$classifier[i], cutoff = grid$cutoff[i], baseline = grid$baseline[i], y = grid$y[i], diff_drop = grid$diff_drop[i], rmterms=grid$rmterms[i])
    
    save(fits, file=sprintf('%s%s_cutoff-%s_baseline-%s_%s_diffdrop-%s.Rdata', odir, grid$classifier[i], grid$cutoff[i], grid$baseline[i], grid$y[i], grid$diff_drop[i])) 
    
  }
  
}

stopCluster(cl)
registerDoSEQ()




## Refine Grid Search ##: Updated to draw from data generator for faster fitting
grid <- expand.grid(classifier=c('xgbTree'),
                    #cutoff=c(.1, .2, .3, .4),
                    cutoff=c(.1, .2, .3),
                    #ndthresh=c(.5, .6, .7, .8, .9), # global conn measures with n proportion == 0 are excluded
                    ndthresh=c(.5, .6, .7, .8), # global conn measures with n proportion == 0 are excluded
                    baseline=c(FALSE, TRUE),
                    y=c('rrsr', 'hdrs6'),
                    rmterms=TRUE,
                    nrounds = c(50, 100, 500),
                    eta=c(0.025, 0.3),
                    max_depth=c(2, 4, 6),
                    gamma = 0,
                    #colsample_bytree=c(0.2, 0.4),
                    colsample_bytree=c(0.4),
                    min_child_weight = c(1,2),
                    subsample = c(.5, 1))
                    

odir <- '/ifshome/bwade/NARSAD/Aim_1/results/refinement/redo/'

source('/ifshome/bwade/NARSAD/Aim_1/code/one_shot_pass_refined_gs_v2.R')

cl <- makePSOCKcluster(20)
registerDoParallel(cl)

for(i in 1:nrow(grid)){
  
  print(i)
  
  if(file.exists(sprintf('%s%s_cutoff-%s_ndthresh-%s_baseline-%s_%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                         odir, grid$classifier[i], grid$cutoff[i], grid$ndthresh[i],
                         grid$baseline[i], grid$y[i],
                         grid$nrounds[i], grid$eta[i],
                         grid$max_depth[i], grid$gamma[i], grid$colsample_bytree[i], grid$min_child_weight[i], grid$subsample[i]))){
    
    next
    
  }else{
    
    fits <- one_shot_pass_refined_gs_v2(clf = grid$classifier[i], 
                                     cutoff = grid$cutoff[i], 
                                     ndthresh = grid$ndthresh[i],
                                     baseline = grid$baseline[i], 
                                     y = grid$y[i], 
                                     rmterms=grid$rmterms[i],
                                     nrounds = grid$nrounds[i],
                                     eta = grid$eta[i],
                                     max_depth = grid$max_depth[i],
                                     gamma = grid$gamma[i],
                                     colsample_bytree = grid$colsample_bytree[i],
                                     min_child_weight = grid$min_child_weight[i],
                                     subsample = grid$subsample[i])
    
    save(fits, file=sprintf('%s%s_cutoff-%s_ndthresh-%s_baseline-%s_%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                             odir, grid$classifier[i], grid$cutoff[i], grid$ndthresh[i],
                             grid$baseline[i], grid$y[i],
                             grid$nrounds[i], grid$eta[i],
                             grid$max_depth[i], grid$gamma[i], grid$colsample_bytree[i], grid$min_child_weight[i], grid$subsample[i]))
    
  }
  
}

stopCluster(cl)
registerDoSEQ()


## Cross-treatment Prediction Refined Grid Search ##
grid <- expand.grid(classifier=c('xgbTree'),
                    cutoff=c(.1, .2, .3, .4),
                    ndthresh=c(.5, .6, .7, .8, .9), # global conn measures with n proportion == 0 are excluded
                    baseline=c(FALSE, TRUE),
                    y=c('rrsr', 'hdrs6'),
                    rmterms=TRUE,
                    nrounds = c(50, 100, 500),
                    eta=c(0.025, 0.3),
                    max_depth=c(2, 4, 6),
                    gamma = 0,
                    colsample_bytree=c(0.2, 0.4),
                    min_child_weight = c(1,2),
                    subsample = c(.5, 1))


source('/ifshome/bwade/NARSAD/Aim_1/code/cross_treatment_prediction_refined_gs_v2.R')

#cl <- makePSOCKcluster(20)
#registerDoParallel(cl)

for(i in 6904:nrow(grid)){
  
  print(i)
  
    
  cross_treatment_prediction_refined_gs_v2(clf = grid$classifier[i], 
                                       cutoff = grid$cutoff[i], 
                                       ndthresh = grid$ndthresh[i],
                                       baseline = grid$baseline[i], 
                                       y = grid$y[i], 
                                       rmterms=grid$rmterms[i],
                                       nrounds = grid$nrounds[i],
                                       eta = grid$eta[i],
                                       max_depth = grid$max_depth[i],
                                       gamma = grid$gamma[i],
                                       colsample_bytree = grid$colsample_bytree[i],
                                       min_child_weight = grid$min_child_weight[i],
                                       subsample = grid$subsample[i])
  
}

stopCluster(cl)
registerDoSEQ()





## Refit Final Models and Save Full Fits ##
grid <- read.table('/ifshome/bwade/NARSAD/Aim_1/results/final_models/model_grid.txt', header = TRUE)

odir <- '/ifshome/bwade/NARSAD/Aim_1/results/final_models/'

source('/ifshome/bwade/NARSAD/Aim_1/code/one_shot_pass_refined_gs_v2.R')

cl <- makePSOCKcluster(20)
registerDoParallel(cl)

#for(i in 1:nrow(grid)){
for(i in 1:2){
  
  print(i)

    
  fits <- one_shot_pass_refined_gs_v2(clf = 'xgbTree', 
                                      cutoff = grid$cutoff[i], 
                                      ndthresh = grid$ndthresh[i],
                                      baseline = grid$baseline[i], 
                                      y = grid$y[i], 
                                      rmterms=FALSE,
                                      nrounds = grid$nrounds[i],
                                      eta = grid$eta[i],
                                      max_depth = grid$maxdepth[i],
                                      gamma = grid$gamma[i],
                                      colsample_bytree = grid$colsamp[i],
                                      min_child_weight = grid$childweight[i],
                                      subsample = grid$subsample[i])
  
  group <- gsub('perf_', '', grid[i, 'variable'])
  
  if(group=='e'){
    fits <- fits[[1]]
  }else if(group=='k'){
    fits <- fits[[2]]
  }else if(group=='s'){
    fits <- fits[[3]]
  }
  
  save(fits, file=sprintf('%sxgbTree_group-%s_cutoff-%s_ndthresh-%s_baseline-%s_%s_nrounds-%s_eta-%s_max_depth-%s_gamma-%s_colsamp-%s_childwei-%s_subsamp-%s.Rdata', 
                          odir, group, grid$cutoff[i], grid$ndthresh[i],
                          grid$baseline[i], grid$y[i],
                          grid$nrounds[i], grid$eta[i],
                          grid$maxdepth[i], grid$gamma[i], grid$colsamp[i], grid$childweight[i], grid$subsample[i]))
    
  
}

stopCluster(cl)
registerDoSEQ()




