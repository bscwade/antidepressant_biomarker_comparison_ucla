load_connectivity_data_noresid <- function(x){
  
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
  ss <- df$timepoint==1 & df$group!='h'
  
  df_ss <- df[ss, ]
  dat_split_ss <- dat_split[ss]
  
  # Average AP and PA runs (?)
  uid <- unique(df_ss$id)
  
  ictcave <- lapply(uid, function(u){
    
    apid <- which(df_ss$id == u & df_ss$ap_pa == 'AP')
    paid <- which(df_ss$id == u & df_ss$ap_pa == 'PA')
    
    ave <- (dat_split_ss[[apid]] + dat_split_ss[[paid]]) / 2
    
    return(ave)
    
  })
  
  # Correlation or partial correlation?
  cm <- lapply(ictcave, function(m) cor(m))
  pcm <- lapply(ictcave, function(m) ppcor::pcor(m))
  
  tmp <- t(sapply(cm, function(m){
    m[upper.tri(m, diag = FALSE)]
  }))
  
  conndat <- data.frame(screen_id=sapply(uid, function(u){paste(strsplit(as.character(u), '')[[1]][1:5], collapse = '')}),
                        tmp)
  
  ## Create reference map for correlation of ICA components
  # refmat <- sapply(1:nrow(cm[[1]]), function(x){
  #   sapply(1:ncol(cm[[1]]), function(y){
  #     paste0(rownames(cm[[1]])[x], '-', colnames(cm[[1]])[y])
  #   })
  # })
  # 
  # refvec <- data.frame(refmat[upper.tri(refmat, diag = FALSE)])
  # refvec$XName <- paste0("X",rownames(refvec))
  # rownames(refvec) <- NULL
  # names(refvec) <- c('Comps','XName')
  # save(refvec, file = '~/Desktop/ICA_Correlation_Mapping.Rdata')
  
  
  return(conndat)
  
}






