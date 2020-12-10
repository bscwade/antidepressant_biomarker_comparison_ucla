# Updated workflow to accomodate new data: 4/14/2020
setwd('/ifshome/bwade/NARSAD/Aim_1/')

end_tp <- 'end_of_treatment' 
outcome <- 'hdrs6'

load_clf_data <- function(end_tp){
  
  setwd('/ifshome/bwade/NARSAD/Aim_1/')
  
  # Demographic/Clinical data: Post treatment
  if(end_tp=='end_of_treatment'){
    source('code/load_clinical_demographic_data_v04142020.R')
    dem <- load_clinical_demographic_data(outcome = outcome)
  }else if(end_tp=='followup'){
    source('code/load_clinical_demographic_data_followup_v04142020.R')
    dem <- load_clinical_demographic_data_followup(outcome = outcome)
  }
  
  # load node-degree connectivity measures
  source('~/NARSAD/Aim_1/code/load_connectivity_data_noresid_graphtheory.R')
  #conn <- load_connectivity_data_noresid_graphtheory() # node-degree distribution 
  load('/ifshome/bwade/NARSAD/Aim_1/data/conn_ndd.Rdata')
  
  # load between-network connectivity measures
  source('~/NARSAD/Aim_1/code/load_connectivity_data_noresid_betwen_network_conn.R')
  load('/ifshome/bwade/NARSAD/Aim_1/data/conn_bnc.Rdata')
  
  # Diffusion data
  source('~/NARSAD/Aim_1/code/load_dti_data.R')
  dti <- load_dti_data()
  dti <- dti[dti$timepoint == '01', ]
  
  # FreeSurfer
  source('~/NARSAD/Aim_1/code/load_aparc.R')
  source('~/NARSAD/Aim_1/code/load_aseg.R')
  aparc <- load_aparc()
  aseg <- load_aseg()
  
  df <- Reduce(function(...) merge(..., by='screen_id'), list(dem, aparc, aseg, conn, conndat_bnc, dti))
  table(df$group)
  
  df <- df[, !names(df) %in% c(grep('hypo', names(df), value = T), 'timepoint', 'uid', 'redcap_event_name', 'screen_id')]
  
  return(df)
  
}

df <- load_clf_data(end_tp = 'end_of_treatment')

# save(df, file = sprintf('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/%s_%s.Rdata', outcome, end_tp))
# 
# o <- lapply(dir('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/', full.names = T), function(f){
#   load(f)
#   return(df$outcome/df$outcome_baseline)
# })




