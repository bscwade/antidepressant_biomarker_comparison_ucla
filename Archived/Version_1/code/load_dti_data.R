load_dti_data <- function(){
  
  #fid <- dir('~/GoogleDrive/BMAP/Studies/Depression_Studies/Ketamine_Studies/HBM_SOBP_2019/Data/Ketamine_DTI/', full.names = TRUE)
  fid <- dir('/ifshome/bwade/NARSAD/Aim_1/data/dti/')
  
  #roi_order <- read.table('~/GoogleDrive/BMAP/Studies/Depression_Studies/Ketamine_Studies/HBM_SOBP_2019/Data/ketamine_roi_labels.txt')
  roi_order <- read.table('/ifshome/bwade/NARSAD/Aim_1/data/ketamine_roi_labels.txt')
  
  # split by diffusion metric
  fid_by_metric <- lapply(c('FA', 'AD', 'MD', 'RD', 'kurt'), function(d){return(grep(d, fid, value = T))})
  
  diffusion_data_list <- lapply(fid_by_metric, function(z){
    
    sublist <- lapply(z, function(x){
      # read data in 
      tmp <- tryCatch(read.table(paste0('/ifshome/bwade/NARSAD/Aim_1/data/dti/', x)), error = function(e) NA)
      
      # make dataframe 
      dftmp <- data.frame(roi_order=roi_order, tmp)
      
      # add column names
      colnames(dftmp) <- c('ROI', 'value')
      
      # reshape dataframe
      dfreshape <- data.frame(t(dftmp$value))
      cnames <- gsub('.txt', '', dftmp$ROI)
      cnames <- gsub('-','_', cnames)
      colnames(dfreshape) <- cnames
      
      # get ID and metric
      id <- strsplit(x, '_')[[1]][[2]]
      #diff <- gsub('DTI//', '', strsplit(x, '_')[[1]][[6]])
      diff <- strsplit(x, '_')[[1]][[1]]
      
      return(list(data=dfreshape,
                  id=id,
                  metric=diff)
      )
      
    })
    
  })
  
  # Format as list of dataframes with ids and metrics
  prelist <- lapply(diffusion_data_list, function(z){
    tmplist <- do.call(rbind, lapply(z, function(y) y$data))
    id <- sapply(z, function(y) y$id)
    timepoint <- as.factor(substr(id, 6,7))
    #timepoint <- ordered(timepoint, c('01', '02', '03', '04'))
    id <- as.factor(substr(id, 1, 5))
    metric <- sapply(z, function(y) y$metric)
    return(data.frame(id, timepoint, metric, tmplist))
  })
  
  prelist2 <- lapply(prelist, function(x){
    
    # append metric
    colnames(x) <- c(colnames(x)[1:3], paste0(colnames(x[, 4:ncol(x)]), '_', x$metric[1])) # some hard coding
    
    # make uid
    x$uid <- paste0(x$id, '_', x$timepoint)
    
    # drop metric column
    x <- subset(x, select = -metric)
    
    return(x)
    
  })
  
  # merge data
  df <- Reduce(function(x,y) merge(x,y, by = c('uid', 'timepoint', 'id')), prelist2)
  
  # rename id
  df <- plyr::rename(df, replace = c('id'='screen_id'))
  
  # remove duplicated observations
  df <- df[-which(duplicated(df$uid)), ]
  df$timepoint <- as.factor(df$timepoint)
  
  return(df)
  
  # m <- melt(prelist[[3]], id.vars = c('id', 'metric', 'timepoint'))
  # ml <- m[grep('_R', m$variable), ]
  # 
  # ggplot(ml, aes(timepoint, value, fill = timepoint)) + geom_boxplot() + facet_wrap(~variable, scales = 'free')
  # 
  # # quick stats 
  # dlist <- split(m, f = m$variable)
  # 
  # fitlist <- lapply(dlist, function(x){
  #   fit <- lme(value~timepoint, random=~1|id, data=x[complete.cases(x), ])
  #   return(summary(fit))
  # })
  
  
}




