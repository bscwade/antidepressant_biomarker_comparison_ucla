
#### Hyperparameter Selection ####

grid <- expand.grid(classifier=c('rf', 'xgbTree', 'svmRadial'),
                    cutoff=c(0.1, 0.2, 0.3, 0.4, 0.5),
                    ndthresh=c(0.5, 0.6, 0.7, 0.8, 0.9),
                    baseline=c(TRUE, FALSE),
                    y=c('rrsr', 'hdrs6'),
                    diff_drop=FALSE,
                    rmterms=TRUE)


perf_coarse <- lapply(1:nrow(grid), function(r){
  
  flist <- dir('/ifshome/bwade/NARSAD/Aim_1/results/gridsearch/', full.names = TRUE)
  
  f <- grep(paste0('ndthresh-', grid[r, 'ndthresh']), grep(grid[r, 'y'], grep(paste0('baseline-', grid[r, 'baseline']), grep(paste0('cutoff-', grid[r, 'cutoff']), grep(grid[r, 'classifier'], flist, value = T), value = T), value = T), value = T), value = T)
  
  if(length(f)==0){
    return(NA)
  }else{
    
    
    load(f)
    
    cm <- list()
    
    for(a in 1:3){
      
      pred <- fits[[a]]$mod$pred
      
      cm[[a]] <- tryCatch(cor(pred$pred, pred$obs), error = function(e) NA)
      
    }
    
    return(cm)
  }
  
})

grid$performance_ect <- sapply(perf_coarse, function(x) tryCatch(x[[1]], error=function(e) NA))
grid$performance_ski <- sapply(perf_coarse, function(x) tryCatch(x[[2]], error=function(e) NA))
grid$performance_tsd <- sapply(perf_coarse, function(x) tryCatch(x[[3]], error=function(e) NA))

m <- melt(grid, id.vars=c('classifier', 'cutoff', 'ndthresh', 'baseline','y','diff_drop', 'rmterms'))

m <- m[m$y=='hdrs6',]

m$cutoff <- factor(m$cutoff, levels=c(0.1, 0.2, 0.3, 0.4, 0.5))
m$cutoff <- as.factor(m$cutoff)
m$ndthresh <- as.factor(m$ndthresh)

p1 <- ggplot(m, aes(classifier, value, fill=classifier)) + 
  geom_boxplot() + facet_grid(baseline~y) + theme_bw() + 
  ylab('Predicted vs. Actual Change Correlation') + xlab('Classifier') + 
  ggtitle('Hyperparameter Tuning: Classifier Selection') + 
  theme_bw()

p2 <- ggplot(m, aes(cutoff, value, fill=cutoff)) + 
  geom_boxplot() + facet_grid(baseline~y) + theme_bw() + 
  ylab('Predicted vs. Actual Change Correlation') + xlab('Correlation Cutoff') + 
  ggtitle('Hyperparameter Tuning: Correlation Threshold') + 
  theme_bw()

p3 <- ggplot(m, aes(ndthresh, value, fill=ndthresh)) + 
  geom_boxplot() + facet_grid(baseline~y) + theme_bw() + 
  ylab('Predicted vs. Actual Change Correlation') + xlab('Node Degree Variance Threshold') + 
  ggtitle('Hyperparameter Tuning: Node Degree Exclusion') + 
  theme_bw()


require("gridExtra")
grid.arrange(p1, p2, p3, 
             ncol=1, nrow=3)



#### Refind Grid Search Results ####
fid <- dir("/ifshome/bwade/NARSAD/Aim_1/results/refinement/", pattern = 'Rdata', full.names = TRUE)
fid <- fid[grep('hdrs6', fid)]

perf2 <- lapply(fid, function(f){
  
  load(f)
  
  pvac <- sapply(fits, function(x) cor(x$mod$pred$pred, x$mod$pred$obs))
  
  rm(fits)
  
  ss <- strsplit(f, '_')[[1]]
  
  tmp <- data.frame(
    y=ss[grep('hdrs6|rrsr', ss)],
    baseline=gsub('baseline-','',ss[grep('baseline', ss)]),
    cutoff=gsub('cutoff-','',ss[grep('cutoff-', ss)]),
    ndthresh=gsub('ndthresh-','',ss[grep('ndthresh-', ss)]),
    nrounds=gsub('nrounds-','',ss[grep('nrounds-', ss)]),
    eta=gsub('eta-','',ss[grep('eta-', ss)]),
    maxdepth=gsub('depth-','',ss[grep('depth-', ss)]),
    gamma=gsub('gamma-','',ss[grep('gamma-', ss)]),
    colsamp=gsub('colsamp-','',ss[grep('colsamp-', ss)]),
    childweight=gsub('childwei-','',ss[grep('childwei-', ss)]),
    subsample=gsub('subsamp-|.Rdata','',ss[grep('subsamp-', ss)]),
    perf_e = pvac[[1]],
    perf_k = pvac[[2]],
    perf_s = pvac[[3]]
  )
  
  return(tmp)
  
})

perf <- do.call(rbind, perf2)
m_manual <- melt(perf, id.vars=c('y', 'baseline', 'cutoff', 'ndthresh', 'nrounds', 'eta', 'maxdepth', 'gamma', 'colsamp', 'childweight', 'subsample'))

ggplot(m_manual, aes(x=NULL, y=value, fill=variable)) + 
  geom_boxplot() + 
  facet_grid(y~baseline) + 
  geom_hline(yintercept = 0, col='red') + 
  ylim(-.4, .6) + 
  xlab('Treatment Group') + 
  ylab('Predicted vs. Actual Change Correlation') +
  ggtitle('PvAc by Treatment, Outcome, and Baseline Symptom', subtitle = 'Manual Grid Search') + 
  theme_bw()



#### PvAc Plots ####

pvac_plot <- function(fit){
  
  pred <- fit$pred
  
  pvac <- round(cor(pred$obs, pred$pred), digits = 4)
  cp <- round(cor.test(pred$obs, pred$pred)$p.value, digits = 4)
  xpos <- max(pred$obs) - 2
  ypos <- max(pred$pred)
  
  p <- ggplot(pred, aes(obs, pred)) +
    geom_boxplot(aes(group=rowIndex)) + 
    geom_point(alpha=.5) + 
    geom_smooth(method=lm, col='red') + 
    #geom_text(x=xpos, y=ypos, label=sprintf('r = %s; p = %s', pvac, cp)) + 
    ylab('Predicted Change') + xlab('Actual Change') + 
    ylim(-9, 2) +
    theme_bw() + 
    ggtitle('Predicted Versus Actual Symptom Change')
  
  return(p)
  
}

load('/ifshome/bwade/NARSAD/Aim_1/data/perf_manual_fid.Rdata')

load_best_model <- function(y, arm, baseline, perf_fid){
  
  if(arm=='e'){
    idx <- 1
  }else if(arm=='k'){
    idx <- 2
  }else if(arm=='s'){
    idx <- 3
  }
  
  perf_ss <- perf_fid[perf_fid$y==y & perf_fid$baseline==baseline, ]
  load_idx <- which.max(perf_ss[[paste0('perf_', arm)]])
  load(as.character(perf_ss$fid[load_idx]))
  
  return(fits[[idx]])
  
}

# ECT
fit <- load_best_model(y = 'hdrs6', arm = 'e', baseline = TRUE, perf_fid = perf_fid)
fit <- fit$mod
p1 <-pvac_plot(fit)

# Ketamine
fit <- load_best_model(y = 'hdrs6', arm = 'k', baseline = TRUE, perf_fid = perf_fid)
fit <- fit$mod
p2 <-pvac_plot(fit)

# TSD
fit <- load_best_model(y = 'hdrs6', arm = 's', baseline = TRUE, perf_fid = perf_fid)
fit <- fit$mod
p3 <-pvac_plot(fit)

require("gridExtra")
grid.arrange(p1, p2, p3, 
             ncol=3, nrow=1)
 

#### Radar Chart ####
compare_predictors <- function(y, nstart=1, nend=10, title){
  
  require(fmsb)
  
  load('/ifshome/bwade/NARSAD/Aim_1/data/perf_manual_fid.Rdata')
  
  perf_fid <- perf_fid[perf_fid$baseline==TRUE, ]
  
  m <- melt(perf_fid[, c('y', 'perf_e', 'perf_k', 'perf_s', 'fid')], id.vars=c('y', 'fid'))
  
  ds <- split(m, f=list(m$y, m$variable))
  
  # ordered list of performance by outcome and group
  dso <- lapply(ds, function(x){
    
    tmp <- x[order(x$value, decreasing = T), ]
    return(tmp[1:30, ])
    
  })
  
  # if(y=='hdrs6'){
  #   fidlist <- list(dso[[1]]$fid, dso[[3]]$fid, dso[[5]]$fid) # HDRS Outcomes  
  # }else if(y=='rrsr'){
  #   fidlist <- list(dso[[2]]$fid, dso[[4]]$fid, dso[[6]]$fid) # RRSR Outcomes 
  # }
  
  fidlist <- list(dso[[1]]$fid, dso[[2]]$fid, dso[[3]]$fid)
  
  ilist <- lapply(1:3, function(g){ # iterate over groups
    
    implist <- lapply(fidlist[[g]], function(f){ # iterate over group-specific fids
      
      load(as.character(f))
      
      imp <- xgb.importance(feature_names=fits[[g]]$mod$finalModel$feature_names, model=fits[[g]]$mod$finalModel)
      ret <- data.frame(feature=imp$Feature, gain=scale(imp$Gain, center=F))
      return(ret)
      
    })
    
    test <- Reduce(function(...) merge(..., all=T, by = 'feature'), implist)
    
    d <- data.frame(test$feature, imp=rowMeans(test[, 2:ncol(test)], na.rm=T))
    d <- d[order(d$imp, decreasing = T),]
    
    return(d)
    
  })
  
  features <- unique(c(as.character(ilist[[1]]$test.feature[nstart:nend]), as.character(ilist[[2]]$test.feature[nstart:nend]), as.character(ilist[[3]]$test.feature[nstart:nend])))
  
  data <- as.data.frame(matrix(rep(0, 3*length(features)), nrow=3))
  colnames(data) <- features
  rownames(data) <- c('E', 'K', 'S')
  
  for(r in 1:nrow(data)){
    #print(r)
    
    for(c in 1:ncol(data)){
      #print(c) 
      gain <- ilist[[r]][which(ilist[[r]]$test.feature %in% colnames(data)[c]), 'imp']
      
      if(length(gain)==1){
        data[r, c] <- gain  
      }else{
        data[r, c] <- 0
      }
      
    }
    
  }
  
  #data <- scale(data, center = F)
  
  maxval <- max(data) + sd(unlist(data))
  print(sprintf('max gain: %s', max(data)))
  print(sprintf('ylim: %s', maxval))
  
  data <- rbind(rep(maxval,length(features)) , rep(0,length(features)) , data)
  
  # Color vector
  colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
  colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
  
  # plot with default options:
  radarchart( as.data.frame(data)  , axistype=1 , 
              #custom polygon
              pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", 
              #caxislabels=seq(0,20,5), 
              cglwd=0.8,
              #custom labels
              vlcex=1,
              title=title
  )
  
  # Add a legend
  legend(x=1, y=1.2, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=10)
  
  
}
compare_predictors(y = 'hdrs6', nstart = 1, nend=10, title ='test')




#### Cross Treatment Prediction Matrix ####
load('/ifshome/bwade/NARSAD/Aim_1/results/cross_correlation_results_with_parameters.Rdata')

df$basemodel <- paste0('basemodel_', df$basemodel)
df$crosstx <- paste0('predicting_', df$crosstx)

df_hdrs <- df[df$y=='hdrs6', ]
df_rrsr <- df[df$y=='rrsr', ]

ggplot(df_hdrs, aes(base, cross, fill=baseline)) + 
  geom_point(aes(colour=baseline), shape=1) + 
  facet_grid(basemodel~crosstx) + 
  geom_hline(yintercept = 0, col='black', lty=2) + 
  geom_vline(xintercept = 0, col='black', lty=2) + 
  theme_bw() + 
  xlab('Base Model Performance') + 
  ylab('Cross Treatment Performance') +
  ggtitle('Cross-treatment Symptom Change Predictions', subtitle = 'HDRS-6')




