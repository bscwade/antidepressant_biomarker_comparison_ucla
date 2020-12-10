fid <- dir('/ifshome/bwade/NARSAD/Aim_1/results/cross_treatment_grid_search/', full.names = TRUE)

df <- lapply(fid, function(f){
  
  load(f)
  
  if(grepl('predicting-e', f)){
    cm <- cor(e_pred$obs, e_pred$pred)
    rm(e_pred)
  }else if(grepl('predicting-k', f)){
    cm <- cor(k_pred$obs, k_pred$pred)
    rm(k_pred)
  }else if(grepl('predicting-s', f)){
    cm <- cor(s_pred$obs, s_pred$pred)
    rm(s_pred)
  }
  
  ss <- strsplit(f, '/|_')[[1]]
  
  tmp <- data.frame(
    basemodel=ss[grep('basemodel-', ss)],
    predicted=ss[grep('predicting-', ss)],
    cutoff=gsub('cutoff-','', ss[grep('cutoff-', ss)]),
    baseline=gsub('baseline-','', ss[grep('baseline-', ss)]),
    y=grep('hdrs|rrsr|rrsb|tcqr', ss, value=TRUE),
    nrounds=gsub('nrounds-', '', ss[grep('nrounds-', ss)]),
    eta=gsub('eta-','', ss[grep('eta-', ss)]),
    max_depth=gsub('depth-','',ss[grep('depth-', ss)]),
    gamma=gsub('gamma-','',ss[grep('gamma-', ss)]),
    colsamp=gsub('colsamp-','',ss[grep('colsamp-', ss)]),
    chidweight=gsub('childwei-','',ss[grep('childwei-', ss)]),
    subsample=gsub('subsamp-|.Rdata','', ss[grep('subsamp-', ss)]),
    perf=cm
  )
  
})


df <- do.call(rbind, df)

df$predicted <- factor(df$predicted, levels = c('predicting-e', 'predicting-k', 'predicting-s'))
df$nrounds <- factor(df$nrounds, levels = c(50, 100, 500))

# highlight best models within arm
model_list <- read.table('/ifshome/bwade/NARSAD/Aim_1/results/final_models/best_fits_all.txt')
df$best_within_arm_model <- rep(0, nrow(df))

# ect models
df[df$basemodel=='basemodel-e' & df$cutoff==0.3 & df$baseline==FALSE & df$y=='hdrs6' & df$nrounds==100 & df$eta==0.025 & df$max_depth==6 & df$gamma==0.3 & df$chidweight==1 & df$subsample==1, 'best_within_arm_model'] <- 1
df[df$basemodel=='basemodel-e' & df$cutoff==0.1 & df$baseline==FALSE & df$y=='rrsb' & df$nrounds==100 & df$eta==0.025 & df$max_depth==2 & df$gamma==0.3 & df$chidweight==1 & df$subsample==1, 'best_within_arm_model'] <- 1
df[df$basemodel=='basemodel-e' & df$cutoff==0.1 & df$baseline==FALSE & df$y=='rrsr' & df$nrounds==500 & df$eta==0.025 & df$max_depth==2 & df$gamma==0.3 & df$chidweight==2 & df$subsample==1, 'best_within_arm_model'] <- 1
df[df$basemodel=='basemodel-e' & df$cutoff==0.1 & df$baseline==FALSE & df$y=='tcqr' & df$nrounds==500 & df$eta==0.025 & df$max_depth==6 & df$gamma==0.3 & df$chidweight==2 & df$subsample==0.5, 'best_within_arm_model'] <- 1

# ketamine models
df[df$basemodel=='basemodel-k' & df$cutoff==0.3 & df$baseline==FALSE & df$y=='hdrs6' & df$nrounds==500 & df$eta==0.025 & df$max_depth==2 & df$gamma==0.3 & df$chidweight==2 & df$subsample==0.5, 'best_within_arm_model'] <- 1
df[df$basemodel=='basemodel-k' & df$cutoff==0.1 & df$baseline==FALSE & df$y=='rrsb' & df$nrounds==100 & df$eta==0.025 & df$max_depth==4 & df$gamma==0.3 & df$chidweight==2 & df$subsample==1, 'best_within_arm_model'] <- 1
df[df$basemodel=='basemodel-k' & df$cutoff==0.3 & df$baseline==FALSE & df$y=='rrsr' & df$nrounds==100 & df$eta==0.025 & df$max_depth==2 & df$gamma==0.3 & df$chidweight==2 & df$subsample==1, 'best_within_arm_model'] <- 1
df[df$basemodel=='basemodel-k' & df$cutoff==0.1 & df$baseline==FALSE & df$y=='tcqr' & df$nrounds==100 & df$eta==0.025 & df$max_depth==2 & df$gamma==0.3 & df$chidweight==2 & df$subsample==1, 'best_within_arm_model'] <- 1

# sleep deprivation
df[df$basemodel=='basemodel-s' & df$cutoff==0.1 & df$baseline==FALSE & df$y=='hdrs6' & df$nrounds==100 & df$eta==0.025 & df$max_depth==6 & df$gamma==0.3 & df$chidweight==2 & df$subsample==1, 'best_within_arm_model'] <- 1
df[df$basemodel=='basemodel-s' & df$cutoff==0.3 & df$baseline==FALSE & df$y=='rrsb' & df$nrounds==500 & df$eta==0.025 & df$max_depth==2 & df$gamma==0.3 & df$chidweight==1 & df$subsample==1, 'best_within_arm_model'] <- 1
df[df$basemodel=='basemodel-s' & df$cutoff==0.3 & df$baseline==FALSE & df$y=='rrsr' & df$nrounds==100 & df$eta==0.025 & df$max_depth==4 & df$gamma==0.3 & df$chidweight==2 & df$subsample==1, 'best_within_arm_model'] <- 1
df[df$basemodel=='basemodel-s' & df$cutoff==0.1 & df$baseline==FALSE & df$y=='tcqr' & df$nrounds==500 & df$eta==0.025 & df$max_depth==2 & df$gamma==0.3 & df$chidweight==2 & df$subsample==0.5, 'best_within_arm_model'] <- 1


df$best_within_arm_model <- as.factor(df$best_within_arm_model)

ggplot(df, aes(y, perf, fill=baseline)) + 
  geom_boxplot() + 
  facet_grid(basemodel~predicted) +
  geom_hline(yintercept=0, color='red') + 
  theme_bw() + 
  geom_point(data=df[df$best_within_arm_model==1,], aes(y, perf), color='green', shape=18, size=4) + 
  ggtitle('Cross Treatment Prediction Performance', subtitle = 'Results from refined grid search') + 
  ylab('Predicted Versus Actual Symptom Change Correlation') + 
  xlab('Symptom Dimension')









