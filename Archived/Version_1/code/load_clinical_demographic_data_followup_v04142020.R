
load_clinical_demographic_data_followup <- function(outcome){
  
  ## outcomes ##
  # "hdrs6"
  #"rumq_rrsb_total_st" 
  #"rumq_rrsr_total_st" 
  #"rumq_tcqr_total_st"
  
  ## read data 
  kdat <- read.csv('data/neuropsych/Ket.csv')
  edat <- read.csv('data/neuropsych/ECT.csv')
  tdat <- read.csv('data/neuropsych/TSD.csv')
  
  ## format ketamine data
  # T1 = baseline_arm_2; T3 = postinfusion_asses_arm_2
  # keep columns with hamd, not hamd17
  # demographics have to be taken separately from consult
  
  kdem <- kdat[kdat$redcap_event_name=='register_arm_2', c('screen_id', 'cont_age', 'cont_sex', 'dabst_duration', 'dabst_num_episodes')]
  
  if(outcome=='hdrs6'){
    tmp <- kdat[kdat$redcap_event_name == 'baseline_arm_2' | kdat$redcap_event_name == 'followup_assessmen_arm_2', c('screen_id', 'redcap_event_name', 'hamd_depressed_mood', 'hamd_guilt', 'hamd_activities', 'hamd_retardation', 'hamd_anxiety_psychic', 'hamd_somsxs_general')]
    kclin <- data.frame(screen_id=tmp$screen_id, redcap_event_name=tmp$redcap_event_name, hdrs6=rowSums(tmp[, grep('hamd', names(tmp), value = T)]))
    kclin$redcap_event_name <- droplevels(kclin$redcap_event_name)
  }else{
    kclin <- kdat[kdat$redcap_event_name == 'baseline_arm_2' | kdat$redcap_event_name == 'followup_assessmen_arm_2', c('screen_id', 'redcap_event_name', grep(outcome, names(kdat), value = T))]
  }
  
  kclin <- kclin[complete.cases(kclin), ]
  
  tab <- table(kclin$screen_id)
  id_keep <- names(tab)[tab==2]
  
  kclin <- kclin[kclin$screen_id %in% id_keep, ]
  
  
  ## format ECT data
  # T1 = baseline_arm_2; T3 = posttreatment_asse_arm_2 ??
  
  edem <- edat[edat$redcap_event_name=='register_arm_2', c('screen_id', 'cont_age', 'cont_sex', 'dabst_duration', 'dabst_num_episodes')]
  
  if(outcome=='hdrs6'){
    tmp <- edat[edat$redcap_event_name == 'baseline_arm_2' | edat$redcap_event_name == 'followup_assessmen_arm_2', c('screen_id', 'redcap_event_name', 'hamd_depressed_mood', 'hamd_guilt', 'hamd_activities', 'hamd_retardation', 'hamd_anxiety_psychic', 'hamd_somsxs_general')]
    eclin <- data.frame(screen_id=tmp$screen_id, redcap_event_name=tmp$redcap_event_name, hdrs6=rowSums(tmp[, grep('hamd', names(tmp), value = T)]))
    eclin$redcap_event_name <- droplevels(eclin$redcap_event_name)
  }else{
    eclin <- edat[edat$redcap_event_name == 'baseline_arm_2' | edat$redcap_event_name == 'followup_assessmen_arm_2', c('screen_id', 'redcap_event_name', grep(outcome, names(edat), value = T))]
  }
  
  eclin <- eclin[complete.cases(eclin), ]
  
  tab <- table(eclin$screen_id)
  id_keep <- names(tab)[tab==2]
  
  eclin <- eclin[eclin$screen_id %in% id_keep, ]
  

  ## format TSD data
  # T1 = baseline_arm_2; T3 = posttreatment_asse_arm_2 ??
  
  tdem <- tdat[tdat$redcap_event_name=='register_arm_2', c('screen_id', 'cont_age', 'cont_sex', 'dabst_duration', 'dabst_num_episodes')]
  
  if(outcome=='hdrs6'){
    tmp <- tdat[tdat$redcap_event_name == 'pretsd_assessment_arm_2' | tdat$redcap_event_name == 'followup_arm_2', c('screen_id', 'redcap_event_name', 'hamd_depressed_mood', 'hamd_guilt', 'hamd_activities', 'hamd_retardation', 'hamd_anxiety_psychic', 'hamd_somsxs_general')]
    tclin <- data.frame(screen_id=tmp$screen_id, redcap_event_name=tmp$redcap_event_name, hdrs6=rowSums(tmp[, grep('hamd', names(tmp), value = T)]))
    tclin$redcap_event_name <- droplevels(tclin$redcap_event_name)
  }else{
    tclin <- tdat[tdat$redcap_event_name == 'pretsd_assessment_arm_2' | tdat$redcap_event_name == 'followup_arm_2', c('screen_id', 'redcap_event_name', grep(outcome, names(tdat), value = T))]
  }
  
  tclin <- tclin[complete.cases(tclin), ]
  
  tab <- table(tclin$screen_id)
  id_keep <- names(tab)[tab==2]
  
  tclin <- tclin[tclin$screen_id %in% id_keep, ]
  
  
  ## calculate symptom change
  # merge data by arm
  merged_clinical_data <- rbind(kclin, eclin, tclin)
  
  # add unified time point
  merged_clinical_data$time_point <- rep(NA, nrow(merged_clinical_data))
  merged_clinical_data$time_point[grep('baseline|pretsd_assessment_arm_2', merged_clinical_data$redcap_event_name)] <- 'Baseline'
  merged_clinical_data$time_point[grep('followup', merged_clinical_data$redcap_event_name)] <- 'Follow-up'
  merged_clinical_data$time_point <- as.factor(merged_clinical_data$time_point)
  
  # split by time point
  merged_clinical_data_by_time <- split(merged_clinical_data, f=merged_clinical_data$time_point)
  assertthat::are_equal(merged_clinical_data_by_time[[1]]$screen_id, merged_clinical_data_by_time[[2]]$screen_id)
  
  clinical_change <- data.frame(screen_id=merged_clinical_data_by_time[[1]]$screen_id,
                                redcap_event_name=merged_clinical_data_by_time[[1]]$redcap_event_name,
                                outcome=(merged_clinical_data_by_time[[2]][[outcome]] - merged_clinical_data_by_time[[1]][[outcome]]),
                                outcome_baseline=merged_clinical_data_by_time[[1]][[outcome]])
  
  
  # merge with demographic data 
  dem <- data.frame(rbind(kdem, edem, tdem))
  
  data <- merge(dem, clinical_change, by = 'screen_id')
  
  data$group <- as.factor(substr(data$screen_id, 1, 1))
  
  data$screen_id <- gsub('_', '', as.character(data$screen_id))
  
  return(data)
  
}



