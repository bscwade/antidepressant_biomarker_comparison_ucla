load_aparc <- function(){  
  
  load_lh <- function(){
    
    #fid_aparc <- grep('_lh', dir('~/Desktop/FS_Output/', full.names = TRUE, pattern = 'aparc'), value = T)
    #fid_aparc <- grep('_lh', dir('~/GoogleDrive/BMAP/Studies/Depression_Studies/U01_Data/FS_Output/', full.names = TRUE, pattern = 'aparc'), value = T)
    fid_aparc <- grep('_lh', dir('/ifshome/bwade/NARSAD/Aim_1/data/FS_Output_112119/', full.names = TRUE, pattern = 'aparc'), value = T)
    
    id <- sapply(fid_aparc, function(x){
      strsplit(strsplit(x, '//')[[1]], '_')[[2]][1]
    })
    
    df <- data.frame(t(sapply(fid_aparc, function(x){
      tmp <- read.table(x ,header = FALSE)
      return(tmp$V5)
    })))
    
    ROI <- read.table(fid_aparc[[1]])
    ROI <- paste0('lh_', ROI$V1)
    
    colnames(df) <- ROI
    df$screen_id <- id
    
    
    df <- df[grepl('1$', df$screen_id), ]
    df <- df[grepl('^[esk]', df$screen_id), ]
    
    return(df)
    
  }
  
  
  load_rh <- function(){
    
    #fid_aparc <- grep('_rh', dir('~/Desktop/FS_Output/', full.names = TRUE, pattern = 'aparc'), value = T)
    #fid_aparc <- grep('_rh', dir('~/GoogleDrive/BMAP/Studies/Depression_Studies/U01_Data/FS_Output/', full.names = TRUE, pattern = 'aparc'), value = T)
    fid_aparc <- grep('_rh', dir('/ifshome/bwade/NARSAD/Aim_1/data/FS_Output_112119/', full.names = TRUE, pattern = 'aparc'), value = T)
    
    id <- sapply(fid_aparc, function(x){
      strsplit(strsplit(x, '//')[[1]], '_')[[2]][1]
    })
    
    df <- data.frame(t(sapply(fid_aparc, function(x){
      tmp <- read.table(x ,header = FALSE)
      return(tmp$V5)
    })))
    
    ROI <- read.table(fid_aparc[[1]])
    ROI <- paste0('rh_', ROI$V1)
    
    colnames(df) <- ROI
    df$screen_id <- id
    
    
    df <- df[grepl('1$', df$screen_id), ]
    df <- df[grepl('^[esk]', df$screen_id), ]
    
    return(df)
    
  }
  
  lh_dat <- load_lh()
  rh_dat <- load_rh()
  
  ct_dat <- merge(lh_dat, rh_dat, by='screen_id')
  
  ct_dat$screen_id <- sapply(ct_dat$screen_id, function(x){
    paste(strsplit(x, '')[[1]][1:5], collapse = '')
  })
    
  return(ct_dat)

}

