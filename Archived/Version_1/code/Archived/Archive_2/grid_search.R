## Load imaging data

setwd('/ifshome/bwade/NARSAD/Aim_1/')

source('~/NARSAD/Aim_1/code/load_connectivity_data_noresid_graphtheory.R')
#conn <- load_connectivity_data_noresid_graphtheory() # node-degree distribution 
load('/ifshome/bwade/NARSAD/Aim_1/data/conn_ndd.Rdata')

# Diffusion data
source('~/NARSAD/Aim_1/code/load_dti_data.R')
dti <- load_dti_data()
dti <- dti[dti$timepoint == '01', ]

# FreeSurfer
source('~/NARSAD/Aim_1/code/load_aparc.R')
source('~/NARSAD/Aim_1/code/load_aseg.R')
aparc <- load_aparc()
aseg <- load_aseg()


## Create tuning grid
params <- expand.grid(ntree=c(500, 1000, 5000), nrfe = c(3, 5, 10), var_freq = c(1, 3), custom_of = c(TRUE, FALSE), RFE = c(TRUE, FALSE), clf=c('rf'), outcome=c('rumq_rrsr_total_st'))#'rumq_rrsr_total_st', 'dass_anxiety_total', 'dass_stress_total'))

#for(i in 9:nrow(params)){
for(i in 1:9){
  
  print(sprintf('Parameter Combination %s', i))
  
  source('~/NARSAD/Aim_1/code/load_clinical_demographic_data_variable_outcome.R')
  dem <- load_clinical_demographic_data_variable_outcome(outcome = as.character(params$outcome[i]), percent = FALSE)
  # source('~/NARSAD/Aim_1/code/load_clinical_demographic_data_v112219.R')
  # dem <- load_clinical_demographic_data()
  # dem <- plyr::rename(dem, replace = c('qids_percent_change'='outcome_percent_change'))
  
  df <- Reduce(function(...) merge(..., by='screen_id'), list(dem, aparc, aseg, conn))
  df <- df[, !names(df) %in% c(grep('hypo', names(df), value = T), 'timepoint', 'uid', 'redcap_event_name', 'screen_id')]
  
  source('/ifshome/bwade/NARSAD/Aim_1/code/predict_co_rfe.R')
  results <- lapply(levels(df$group), function(x){
    
    print(x)
    
    regdat <- df[df$group == x, ]
    
    print(dim(regdat))
    
    regdat <- regdat[, !names(regdat) %in% c('group')]
    
    res <- predict_co_rfe(y='outcome_percent_change', 
                          df=regdat, nboot = 50, 
                          quantile_threshold=.9, 
                          ntree=params$ntree[i], 
                          rfetree=200, 
                          nrfe=params$nrfe[i], 
                          var_freq=params$var_freq[i], 
                          custom_of = params$custom_of[i], 
                          RFE = params$RFE[i], 
                          clf = params$clf[i])
    
  })
  
    
  save(results, file=sprintf('/ifshome/bwade/NARSAD/Aim_1/results/Gridsearch_CLF-%s_RFE-%s_OF-%s_varfreq-%s_NRFE-%s_ntree-%s_outcome-%s.Rdata', params$clf[i], params$RFE[i], params$custom_of[i], params$var_freq[i], params$nrfe[i], params$ntree[i], params$outcome[i]))
  
}




## Evaluate Grid Search Results ##
fid <- dir('/ifshome/bwade/NARSAD/Aim_1/results/rumq_rrsr_total_st/', full.names = TRUE)
idx <- 2

c <- sapply(fid, function(f){
  load(f)
  pred <- c(sapply(results[[idx]], function(x) x$predicted))
  actual <- c(sapply(results[[idx]], function(x) x$actual))
  c <- cor(pred, actual)
  return(c)
  
})

which.max(c)
max(c)

load(names(which.max(c))[1])





