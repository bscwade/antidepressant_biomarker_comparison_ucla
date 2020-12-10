# Note: baseline RSFC matrices generated with script: /ifshome/bwade/NARSAD/Aim_1/code/make_baseline_rsfc_narsad_aim_1.ipynb 

load_atlas_rsfc <- function(atlas){
  
  fid <- dir('/ifshome/bwade/NARSAD/Aim_1/data/atlas_rsfc/', pattern = atlas, full.names = TRUE)
  fid <- grep('labels.txt', fid, value = TRUE, invert = TRUE)
  
  rsfc <- lapply(fid, function(x){
    rsfc_tmp <- read.csv(x, header=FALSE)
    rsfc_tmp_ut <- rsfc_tmp[upper.tri(rsfc_tmp)]
    return(rsfc_tmp_ut)
  })
  
  sid <- lapply(fid, function(x){
    substr(strsplit(x, 'rsfc//')[[1]][2], 1,5)
  })
  
  rsfc_df <- data.frame(do.call(rbind, sid),
                        do.call(rbind, rsfc))
  
  # map atlas label pairs
  if(atlas=='yeo17'){
    labs <- read.table('/ifshome/bwade/NARSAD/Aim_1/data/atlas_rsfc/yeo17_labels.txt')  
  }else if(atlas=='yeo7'){
    labs <- read.table('/ifshome/bwade/NARSAD/Aim_1/data/atlas_rsfc/yeo7_labels.txt')
  }else if(atlas=='mmp'){
    labs <- read.table('/ifshome/bwade/NARSAD/Aim_1/data/atlas_rsfc/mmp_labels.txt')
  }
  
  m <- matrix(NA, nrow=length(labs$V1), ncol=length(labs$V1))
  
  for(i in 1:nrow(m)){
    for(j in 1:ncol(m)){
      m[i,j] <- paste0(labs$V1[i], '_', labs$V1[j])
    }
  }
  
  colnames(rsfc_df) <- c('screen_id', m[upper.tri(m)])
  
  return(rsfc_df)
  
}
