load_connectivity_data_noresid_between_network_conn <- function(){
  
  #flist <- dir('~/GoogleDrive/BMAP/Studies/Depression_Studies/NARSAD_Aim_1/ACNP_2019/Data/dualreg_txt_200.dr/', full.names = TRUE)
  flist <- dir('/ifshome/bwade/NARSAD/Aim_1/data/dr_stage_1/', full.names=TRUE)
  dat_list <- lapply(flist, function(f) read.table(f))
  
  # identify differing lengths
  drop_id <- which(sapply(dat_list, function(x) dim(x)[1]) != 478)
  
  dat <- do.call(rbind, dat_list[-drop_id])
  
  # subset good components
  #dat <- dat[, c(1:16,18:20,25:27,34:35,72)] # Old usable components
  dat <- dat[, c(1:73,75:79,81:85,89:90,93:94,98:100,102:104,106,111,114:115,119:122,125:131,133:141,143:144,146:149,151:154,156:200)] # new good components
  
  #subj <- as.factor(rep(1:634, each = 488))
  subj <- as.factor(rep(1:(length(flist) - length(drop_id)), each = 478))
  dat_split <- split(dat, f = subj)
  
  #idtmp <- read.xls('~/GoogleDrive/BMAP/Studies/Depression_Studies/NARSAD_Aim_1/ACNP_2019/Data/634rfmri_sessions.xlsx', header=F)
  idtmp <- read.table('/ifshome/bwade/NARSAD/Aim_1/data/dualreg_subs.txt')
  idtmp <- data.frame(V1=idtmp$V1[-drop_id])
  
  # get ID
  id <- sapply(idtmp$V1, function(x){
    strsplit(as.character(x), '/')[[1]][7]
  })
  
  # get time point
  timepoint <- sapply(id, function(x){
    strsplit(x, '')[[1]][length(strsplit(x, '')[[1]])]
  })
  
  # get group 
  group <- sapply(id, function(x){
    strsplit(x, '')[[1]][1]
  })
  
  # get AP/PA
  ap_pa <- sapply(idtmp$V1, function(x){
    ifelse(grepl('AP_run', x), 'AP', 'PA')
  })
  
  # merge info
  df <- data.frame(id, timepoint, group, ap_pa)
  
  # boolean subset by group and timepoint 
  #ss <- df$timepoint==1 & df$group!='h'
  
  #df_ss <- df[ss, ]
  #dat_split_ss <- dat_split[ss]
  df_ss <- df
  dat_split_ss <- dat_split 
  
  # Average AP and PA runs (?)
  #uid <- unique(df_ss$id)
  uid <- unique(df$id)
  
  ictcave <- lapply(uid, function(u){
    
    print(u)
    
    apid <- which(df_ss$id == u & df_ss$ap_pa == 'AP')
    paid <- which(df_ss$id == u & df_ss$ap_pa == 'PA')
    
    if(length(apid)>1){ # not sure why some have multiple ap/pa runs?
      apid <- apid[1]
      paid <- paid[1]
    }
    
    ave <- (dat_split_ss[[apid]] + dat_split_ss[[paid]]) / 2
    
    return(ave)
    
  })
  
  # Correlation or partial correlation?
  cm <- lapply(ictcave, function(m) cor(m))
  cm <- lapply(cm, function(x){
    diag(x) <- 0.0
    return(x)
  })
  
  tdf <- lapply(cm, function(c){
    tmp <- c
    tmp[lower.tri(tmp, diag = TRUE)] <- NA
    tm <- melt(tmp)
    tm <- tm[complete.cases(tm), ]
    tdf <- data.frame(t(tm$value))
    colnames(tdf) <- t(paste0(tm$Var1,'_',tm$Var2,'_BnConn'))
    return(tdf)
  })
  
  tmp <- do.call(rbind, tdf)
  
  conndat_bnc <- data.frame(uid=uid, tmp)
  
  write.csv(conndat_bnc, file = '/ifshome/bwade/NARSAD/Aim_1/data/Between_Network_Connectivity_Alex.csv')
  
  raw_connectivity_data <- list(uid=uid, correlation_matrices=cm)
  save(raw_connectivity_data, file = '/ifshome/bwade/NARSAD/Aim_1/data/Raw_Connectivity_Data_Alex.Rdata')
  
}









