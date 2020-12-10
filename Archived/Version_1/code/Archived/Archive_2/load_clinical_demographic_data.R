load_clinical_demographic_data_old <- function(){

  require(gdata)
  prl <- '/usr/local/ActivePerl-5.18.4.1804/bin/perl'
  cdat <- read.xls('/ifshome/bwade/NARSAD/Aim_1/data/Narr_Clinical_Data_Export_02-06-19.xlsx', perl=prl)
  
  # Get/format ketamine qids scores
  qids_ket_dat <- read.xls('/ifshome/bwade/NARSAD/Aim_1/data/NIHT_KET_Export_053019.xlsx', perl=prl)
  qids_ket <- qids_ket_dat[qids_ket_dat$redcap_event_name == 'postinfusion_asses_arm_2' | qids_ket_dat$redcap_event_name == 'baseline_arm_2', c('screen_id', 'redcap_event_name', 'qids1_total_score', 'qids3_total_score')]
  qids_ket$qids_total <- qids_ket$qids1_total_score
  qids_ket$qids_total[is.na(qids_ket$qids1_total_score)] <- qids_ket$qids3_total_score[is.na(qids_ket$qids1_total_score)]
  qids_ket <- qids_ket[, !names(qids_ket) %in% c('qids1_total_score', 'qids3_total_score')]
  
  # Get/format tsd qids scores
  qids_tsd_dat <- read.xls('/ifshome/bwade/NARSAD/Aim_1/data/TSD_QIDS_071819.xlsx', perl=prl)
  qids_tsd <- qids_tsd_dat[qids_tsd_dat$redcap_event_name == 'followup_arm_2' | qids_tsd_dat$redcap_event_name == 'pretsd_assessment_arm_2', c('screen_id', 'redcap_event_name', 'qids1_total_score')]
  qids_tsd$redcap_event_name <- droplevels(qids_tsd$redcap_event_name)
  qids_tsd <- plyr::rename(qids_tsd, replace = c('qids1_total_score'='qids_total'))
  
  # Get/format ect qids scores
  qids_ect_dat <- read.xls('/ifshome/bwade/NARSAD/Aim_1/data/ECT_QIDS_071819.xlsx', perl=prl)
  qids_ect <- qids_ect_dat[qids_ect_dat$redcap_event_name == 'followup_assessmen_arm_2' | qids_ect_dat$redcap_event_name == 'baseline_arm_2', c('screen_id', 'redcap_event_name', 'qids1_total_score')]
  qids_ect$redcap_event_name <- droplevels(qids_ect$redcap_event_name)
  qids_ect <- plyr::rename(qids_ect, replace = c('qids1_total_score'='qids_total'))
  
  qids <- rbind(qids_ect, qids_tsd, qids_ket)
  qids$redcap_event_name <- droplevels(qids$redcap_event_name)
  
  ## split data by time point
  bl <- qids[qids$redcap_event_name == 'baseline_arm_2' | qids$redcap_event_name == 'pretsd_assessment_arm_2', ]
  fu <- qids[qids$redcap_event_name == 'followup_assessmen_arm_2' | qids$redcap_event_name == 'followup_arm_2' | qids$redcap_event_name == 'postinfusion_asses_arm_2', ]
  
  bl <- bl[!is.na(bl$qids_total), ]
  fu <- fu[!is.na(fu$qids_total), ]
  
  id_keep <- intersect(fu$screen_id, bl$screen_id)
  id_keep <- id_keep[!grepl('s_00[0-2]', id_keep)]
  
  bl$screen_id <- as.character(bl$screen_id)
  fu$screen_id <- as.character(fu$screen_id)
  
  bl <- bl[bl$screen_id %in% id_keep, ]
  fu <- fu[fu$screen_id %in% id_keep, ]
  
  ## compute percent hdrs change
  # sanity check
  sum(bl$screen_id != fu$screen_id)
  
  df <- data.frame(
    subset(bl, select = -qids_total),
    qids_percent_change = (fu$qids_total - bl$qids_total)/bl$qids_total,
    group = as.factor(sapply(bl$screen_id, function(i){strsplit(as.character(i), '')[[1]][1]}))
  )
  
  # add age
  age <- cdat[, c('screen_id', 'cont_age')]
  age <- age[age$cont_age != 'N/A',]
  age$cont_age <- as.numeric(as.character(age$cont_age))
  
  # sex
  sex <- cdat[, c('screen_id', 'demog_gender')]
  sex <- sex[sex$demog_gender != 'N/A',]
  
  df <- Reduce(function(...) merge(..., by='screen_id'), list(df, sex, age, bl[, c('screen_id', 'qids_total')])) # age and sex drop a lot of subjects
  
  df <- plyr::rename(df, replace = c('qids_total'='qids_total_baseline'))
  
  df$screen_id <- gsub('_', '', as.character(df$screen_id))
  
  return(df)

}

