# Function to identify most frequent edges in ICA node degrees
# For testing:
#group='k'
#node='V111'

connectivity_pairs <- function(node, group){
  
  # load subjectwise correlation matrices with uid pairs for subsetting
  load('/ifshome/bwade/NARSAD/Aim_1/data/connectivity_mapping_data.Rdata')
  
  # subset according to group and time point
  cm_ss <- conn_info$cm[grepl(sprintf('^%s', group), conn_info$id)]
  
  # get indices of all paired nodes surviving thresholding
  indices <- lapply(cm_ss, function(c){
    which(abs(c)>.3, arr.ind=T)
  })
  
  pairs <- lapply(indices, function(x){
    x[rownames(x)==node, 'col']
  })
  
  # do something about zero-indexing of figure titles
  
  tab <- table(unlist(pairs))
  
  out <- names(tab)[tab>quantile(tab, .99)]
 
  qt <- .99
  while(length(out)<3){
    qt <- qt - 0.01
    out <- names(tab)[tab>quantile(tab, qt)]
    
    if(qt < 0.5){
      break
    }
    
  }
  
  out <- as.numeric(out)
  
  print('Adjusting for zero indexing')
  out <- out - 1
  
  return(out)
  
}

connectivity_pairs(node = 'V3', group = 'k')




