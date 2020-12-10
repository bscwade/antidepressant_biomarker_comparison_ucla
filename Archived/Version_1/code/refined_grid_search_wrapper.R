## Refine Grid Search ##: Updated to draw from data generator for faster fitting
grid <- expand.grid(classifier=c('xgbTree'),
                    cutoff=c(.1, .2, .3),
                    ndthresh=c(.5, .7), # global conn measures with n proportion == 0 are excluded
                    baseline=c(FALSE, TRUE),
                    #y=c('rrsr', 'hdrs6'),
                    y=c('hdrs6'),
                    rmterms=TRUE,
                    nrounds = c(50, 100, 500, 1000),
                    eta=c(.025, .3),
                    max_depth=c(2, 4, 6, 12),
                    gamma = 0,
                    colsample_bytree=c(.5, 1),
                    min_child_weight = c(1,2,4),
                    subsample = c(.5, 1))


odir <- '/ifshome/bwade/NARSAD/Aim_1/results/refinement/'

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
