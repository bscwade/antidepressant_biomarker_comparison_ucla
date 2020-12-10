# Updated workflow to accomodate new data: 4/14/2020
setwd('/ifshome/bwade/NARSAD/Aim_1/')

end_tp <- 'end_of_treatment' 

load_clf_data <- function(end_tp, outcome){
  
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
  
  df <- df[, !names(df) %in% c(grep('hypo', names(df), value = T), 'timepoint', 'uid', 'redcap_event_name')]
  
  # drop sleep deprivation controls labeled with an 's'; they have baseline hdrs scores = 0-1. 
  controls_to_drop <- c('s0006', 's0007', 's0008', 's0010', 's0011', 's0013', 's0014', 's0015', 's0016', 's0019', 's0020', 's0021', 's0023', 's0024', 's0027', 's0028')
  
  problematic_scans_drop <- c('k0051', 'e0024')
  
  df <- df[!df$screen_id %in% c(controls_to_drop, problematic_scans_drop), ]
  
  return(df)
  
}

nplist <- c('rumq_rrsr_total_st', 'hdrs6', 'rumq_rrsb_total_st', 'rumq_tcqr_total_st')

for(n in 1:length(nplist)){
  
  df <- load_clf_data(end_tp = 'end_of_treatment', outcome=nplist[[n]])
  
  save(df, file = sprintf('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/%s_end_of_treatment.Rdata', gsub('rumq_|_total_st','', nplist[n])))
  
}

#df <- load_clf_data(end_tp = 'end_of_treatment', outcome='rumq_rrsr_total_st')
#save(df, file = '/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/rrsr_end_of_treatment.Rdata')




