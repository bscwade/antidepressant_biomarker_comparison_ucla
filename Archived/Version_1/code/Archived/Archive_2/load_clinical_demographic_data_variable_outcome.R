load_clinical_demographic_data_variable_outcome <- function(outcome='rumq_tcqr_total_st', percent=TRUE){
  
  require(gdata)
  prl <- '/usr/local/ActivePerl-5.18.4.1804/bin/perl'
  cdat <- read.xls('/ifshome/bwade/NARSAD/Aim_1/data/Narr_Clinical_Data_Export_02-06-19.xlsx', perl=prl)
  cdat$hdrs6 <- rowSums(sapply(1:6, function(x){as.numeric(as.character(cdat[, c('hamd_depressed_mood', 'hamd_guilt', 'hamd_activities', 'hamd_retardation', 'hamd_anxiety_psychic', 'hamd_somsxs_general')][, x]))}))
  
  # Get/format ketamine qids scores
  qids_ket_dat <- read.csv('/ifshome/bwade/NARSAD/Aim_1/data/DGCSerialKetamineStu-NarrClinicalDataExpo_DATA_2019-11-22_0907.csv')
  qids_ket_dat$hdrs6 <- rowSums(sapply(1:6, function(x){as.numeric(as.character(qids_ket_dat[, c('hamd_depressed_mood', 'hamd_guilt', 'hamd_activities', 'hamd_retardation', 'hamd_anxiety_psychic', 'hamd_somsxs_general')][, x]))}))
  qids_ket <- qids_ket_dat[qids_ket_dat$redcap_event_name == 'postinfusion_asses_arm_2' | qids_ket_dat$redcap_event_name == 'baseline_arm_2', c('screen_id', 'redcap_event_name', outcome)]
  
  # Get/format tsd qids scores
  qids_tsd_dat <- read.xls('/ifshome/bwade/NARSAD/Aim_1/data/TSD_QIDS_071819.xlsx', perl=prl)
  qids_tsd_dat <- merge(qids_tsd_dat, cdat[, c('screen_id', 'redcap_event_name', outcome)])
  qids_tsd <- qids_tsd_dat[qids_tsd_dat$redcap_event_name == 'posttsd_assessment_arm_2' | qids_tsd_dat$redcap_event_name == 'pretsd_assessment_arm_2', c('screen_id', 'redcap_event_name', outcome)]
  qids_tsd$redcap_event_name <- droplevels(qids_tsd$redcap_event_name)
  qids_tsd <- qids_tsd[qids_tsd[[outcome]]!='N/A', ]
  
  # Get/format ect qids scores
  #qids_ect_dat <- read.xls('/ifshome/bwade/NARSAD/Aim_1/data/ECT_QIDS_071819.xlsx', perl=prl)
  qids_ect_dat <- read.csv('/ifshome/bwade/NARSAD/Aim_1/data/DGCECT-NarrClinicalDataExpo_DATA_2019-11-22_0909.csv')
  qids_ect_dat$hdrs6 <- rowSums(sapply(1:6, function(x){as.numeric(as.character(qids_ect_dat[, c('hamd_depressed_mood', 'hamd_guilt', 'hamd_activities', 'hamd_retardation', 'hamd_anxiety_psychic', 'hamd_somsxs_general')][, x]))}))
  qids_ect <- qids_ect_dat[qids_ect_dat$redcap_event_name == 'followup_assessmen_arm_2' | qids_ect_dat$redcap_event_name == 'baseline_arm_2', c('screen_id', 'redcap_event_name', outcome)]
  qids_ect$redcap_event_name <- droplevels(qids_ect$redcap_event_name)
  
  qids <- rbind(qids_ect, qids_tsd, qids_ket)
  qids$redcap_event_name <- droplevels(qids$redcap_event_name)
  qids[[outcome]] <- as.numeric(as.character(qids[[outcome]]))
  
  ## split data by time point
  bl <- qids[qids$redcap_event_name == 'baseline_arm_2' | qids$redcap_event_name == 'pretsd_assessment_arm_2', ]
  fu <- qids[qids$redcap_event_name == 'followup_assessmen_arm_2' | qids$redcap_event_name == 'followup_arm_2' | qids$redcap_event_name == 'postinfusion_asses_arm_2' | qids$redcap_event_name=='posttsd_assessment_arm_2', ]
  
  bl <- bl[!is.na(bl[[outcome]]), ]
  fu <- fu[!is.na(fu[[outcome]]), ]
  
  id_keep <- intersect(fu$screen_id, bl$screen_id)
  id_keep <- id_keep[!grepl('s_00[0-2]', id_keep)]
  
  bl$screen_id <- as.character(bl$screen_id)
  fu$screen_id <- as.character(fu$screen_id)
  
  bl <- bl[bl$screen_id %in% id_keep, ]
  fu <- fu[fu$screen_id %in% id_keep, ]
  
  ## compute percent hdrs change
  # sanity check
  sum(bl$screen_id != fu$screen_id)
  
  if(percent==TRUE){
    df <- data.frame(
      bl[, c('screen_id', 'redcap_event_name')],
      outcome_percent_change = (fu[[outcome]] - bl[[outcome]])/bl[[outcome]],
      group = as.factor(sapply(bl$screen_id, function(i){strsplit(as.character(i), '')[[1]][1]}))
    )
  }else{
    df <- data.frame(
      bl[, c('screen_id', 'redcap_event_name')],
      outcome_percent_change = (fu[[outcome]] - bl[[outcome]]), # compute raw change instead
      group = as.factor(sapply(bl$screen_id, function(i){strsplit(as.character(i), '')[[1]][1]}))
    )
  }
    
  # # add age
  # age <- cdat[, c('screen_id', 'cont_age')]
  # age <- age[age$cont_age != 'N/A',]
  # age$cont_age <- as.numeric(as.character(age$cont_age))
  # 
  # # sex
  # sex <- cdat[, c('screen_id', 'demog_gender')]
  # sex <- sex[sex$demog_gender != 'N/A',]
  # 
  # df <- Reduce(function(...) merge(..., by='screen_id'), list(df, sex, age, bl[, c('screen_id', 'qids_total')])) # age and sex drop a lot of subjects
  # 
  
  #bl <- plyr::rename(bl, replace = c('qids_total'='qids_total_baseline'))
  
  df <- merge(df, bl[, c('screen_id', outcome)], by = 'screen_id')
  
  df$screen_id <- gsub('_', '', as.character(df$screen_id))
  
  return(df)
  
}

