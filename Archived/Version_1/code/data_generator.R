# Function to generate set of hyperparameter specific datasets so modeling can be done more quickly without the need for recurrent preprocessing

require(caret)

dg_grid <- expand.grid(
  group = c('e', 'k', 's'),
  y = c('hdrs6', 'rrsr', 'rrsb'),
  ndthresh = c(.5, .6, .7, .8, .9),
  cutoff = c(.1, .2, .3, .4, .5)
)

odir <- '/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/hyper_param_specific/V2_Data/'

fid <- dir('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/', full.names = TRUE, pattern = 'Rdata')

for(g in 1:nrow(dg_grid)){
  
  print(g)
  
  # find and load base dataset
  data_to_load <- grep(dg_grid$y[g], fid, value = TRUE)
  load(data_to_load)
  
  # subset by group
  tmp <- df[df$group==dg_grid$group[g], ]
  
  # save outcome baseline for later; don't want it getting mixed up with other variables
  outcome_baseline <- tmp$outcome_baseline
  tmp <- subset(tmp, select = -outcome_baseline)
  
  # remove nzv variables
  drop <- nzv(tmp, names = TRUE)
  clfdata <- tmp[, !names(tmp) %in% c(drop, 'screen_id')]
  
  # separate out data types for independent treatments
  bnconn <- clfdata[, grep('BnConn', names(clfdata), value = TRUE)]
  diff <- clfdata[, grep('_FA|_AD|_RD|_MD|_kurt', names(clfdata), value = TRUE)]
  ndegree <- clfdata[, grep('Node_Degree', names(clfdata), value = T)]
  
  # filter data types independently
  gconndrop <- names(ndegree)[sapply(1:ncol(ndegree), function(x) sum(ndegree[, x]==0)/nrow(ndegree)) > dg_grid$ndthresh[g]]
  bnconndrop <- findCorrelation(cor(bnconn), cutoff = dg_grid$cutoff[g], names = TRUE)
  diffdrop <- findCorrelation(cor(diff), cutoff = dg_grid$cutoff[g], names = TRUE)
  otherdrop <- grep('_vessel|5th_Ventricle|CSF|VentralDC|CSF|Optic_', names(clfdata), value = TRUE)
  
  # drop variables and reintroduce baseline symptoms
  cdf <- clfdata[, !names(clfdata) %in% c(diffdrop, bnconndrop, otherdrop, gconndrop)]
  cdf <- data.frame(cdf, outcome_baseline=outcome_baseline)
  
  # save data
  save(cdf, file = sprintf('%sgroup-%s_y-%s_ndthresh-%s_cutoff-%s.Rdata', odir, dg_grid$group[g],dg_grid$y[g], dg_grid$ndthresh[g], dg_grid$cutoff[g]))
  
}
  