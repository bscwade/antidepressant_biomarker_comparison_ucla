load_connectivity_data <- function(x){

  require(gdata)
  
  dat <- data.frame(read.delim('~/GoogleDrive/BMAP/Studies/Depression_Studies/ACNP_2019/Data/dualreg_634.txt', sep=",", header = FALSE))
  subj <- as.factor(rep(1:634, each = 488))
  dat_split <- split(dat, f = subj)
  
  idtmp <- read.xls('~/GoogleDrive/BMAP/Studies/Depression_Studies/ACNP_2019/Data/634rfmri_sessions.xlsx', header=F)
  
  # get ID
  id <- sapply(idtmp$V1, function(x){
    strsplit(as.character(x), split = '/')[[1]][5]
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
  pcm <- lapply(ictcave, function(m) pcor(m))
  
  tmp <- t(sapply(cm, function(m){
    m[upper.tri(m, diag = FALSE)]
  }))
  
  conndat <- data.frame(screen_id=sapply(uid, function(u){paste(strsplit(as.character(u), '')[[1]][1:5], collapse = '')}),
                        tmp)
  
  
  
  return(conndat)

}






