load_aseg <- function(){
  
  #fid_aseg <- dir('~/GoogleDrive/BMAP/Studies/Depression_Studies/Neuropsych_CCA/Data/FS_Output/', full.names = TRUE, pattern = 'aseg')
  fid_aseg <- dir('/ifshome/bwade/NARSAD/Aim_1/data/FS_Output_112119/', full.names = TRUE, pattern = 'aseg')
  
  id <- sapply(fid_aseg, function(x){
    strsplit(strsplit(x, '//')[[1]], '_')[[2]][1]
  })
  
  df <- data.frame(t(sapply(fid_aseg, function(x){
    tmp <- read.table(x ,header = FALSE)
    return(tmp$V4)
  })))
  
  ROI <- read.table(fid_aseg[[1]])
  ROI <- ROI$V5
  
  colnames(df) <- gsub('-', '_', ROI)
  df$screen_id <- id
  
  
  df <- df[grepl('1$', df$screen_id), ]
  df <- df[grepl('^[esk]', df$screen_id), ]
  
  df$screen_id <- sapply(df$screen_id, function(x){
    paste(strsplit(x, '')[[1]][1:5], collapse = '')
  })

  return(df)
  
}







