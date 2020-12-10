

perf <- lapply(1:nrow(grid), function(r){
  
  print(r)
  
  flist <- dir('/ifshome/bwade/NARSAD/Aim_1/results/gridsearch/', full.names = TRUE)
  
  f <- grep(grid[r, 'y'], grep(paste0('baseline-', grid[r, 'baseline']), grep(paste0(grid[r, 'cutoff'],'_'), grep(grid[r, 'classifier'], flist, value = T), value = T), value = T), value = T)
  
  if(length(f)==0){
    return(NA)
  }else{
    
    
    load(f)
    
    cm <- list()
    
    for(a in 1:3){
      
      pred <- fits[[a]]$mod$pred
      
      cm[[a]] <- tryCatch(cor(pred$pred, pred$obs), error = function(e) NA)
      
    }
    
    # idx <- 1 # 1=ECT; 2=Ketamine; 3=TSD
    # 
    # pred <- fits[[idx]]$mod$pred
    # 
    # cm <- tryCatch(cor(pred$pred, pred$obs), error = function(e) NA)
    
    return(cm)
  }
  
})

grid$performance_ect <- sapply(perf, function(x) tryCatch(x[[1]], error=function(e) NA))
grid$performance_ski <- sapply(perf, function(x) tryCatch(x[[2]], error=function(e) NA))
grid$performance_tsd <- sapply(perf, function(x) tryCatch(x[[3]], error=function(e) NA))


m <- melt(grid, id.vars=c('classifier', 'cutoff', 'baseline','y','diff_drop', 'rmterms'))

## Performance by set

# Classifier
ggplot(m, aes(classifier, value, fill=classifier)) + 
  geom_boxplot() + facet_wrap(~y) + theme_bw()

# Correlation cutoff
ggplot(m, aes(as.factor(cutoff), value, fill=as.factor(cutoff))) + 
  geom_boxplot() + facet_wrap(~y) + theme_bw()

# Classifier and treatment arm
ggplot(m, aes(classifier, value, fill=as.factor(variable))) + 
  geom_boxplot() + facet_wrap(~y) + theme_bw()

# Correlation cutoff and treatment arm
ggplot(m, aes(as.factor(cutoff), value, fill=as.factor(variable))) + 
  geom_boxplot() + facet_wrap(~y) + theme_bw()

# Correlation cutoff and treatment arm
ggplot(m, aes(as.factor(cutoff), value, fill=as.factor(variable))) + 
  geom_boxplot() + facet_grid(baseline~y) + theme_bw()


# Classifier by baseline
ggplot(m, aes(classifier, value, fill=variable)) + 
  geom_boxplot() + facet_grid(baseline~y) + theme_bw()


boxplot(unlist(gf$performance) ~ gf$classifier, las=3)
ggplot(gf, aes(classifier, performance, fill=classifier)) + 
  geom_boxplot() + 
  facet_wrap(~y)


m$cutoff <- as.factor(m$cutoff)

# ANOVA test of grid search
a <- aov(value ~ cutoff, data = m[m$baseline==TRUE,])
TukeyHSD(a)

a <- aov(value ~ cutoff, data = m[m$baseline==FALSE & m$classifier=='xgbTree',])
TukeyHSD(a)

tmp <- m[m$baseline==FALSE & m$classifier=='xgbTree',]

ggplot(tmp, aes(cutoff, value, fill=variable)) + geom_boxplot()

idx <- 1
load('/ifshome/bwade/NARSAD/Aim_1/results/gridsearch/xgbTree_cutoff-0.3_baseline-FALSE_rumq_rrsr_total_st_diffdrop-TRUE.Rdata')

pred <- fits[[idx]]$mod$pred

plot(pred$pred, pred$obs)
abline(lm(pred$obs ~ pred$pred))
cor(pred$pred, pred$obs)

plot(varImp(fits[[idx]]$mod))

xgbimp <- xgb.importance(feature_names=fits[[idx]]$mod$finalModel$feature_names, model=fits[[idx]]$mod$finalModel)
#xgb.ggplot.importance(xgbimp)

roi_probe <- 'V24_V133_BnConn'
partial(fits[[idx]]$mod, pred.var = roi_probe, ice = TRUE, center = TRUE, 
        plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "ggplot2", 
        train = fits[[idx]]$data)





sapply(names(fits[[1]]$mod), function(x) format(object.size(fits[[1]]$mod[[x]]), units='MB'))


## Evaluate refined grid search space ##

fid <- dir('/ifshome/bwade/NARSAD/Aim_1/results/refinement/', full.names = TRUE, pattern='Rdata')

perf <- lapply(fid, function(f){
  
    load(f)
    
    cm <- list()
    
    for(a in 1:3){
      
      pred <- fits[[a]]$mod$pred
      
      cm[[a]] <- tryCatch(cor(pred$pred, pred$obs), error = function(e) NA)
      
    }
    
    ss <- strsplit(f, '/|_')[[1]]
    
    if(length(grep('hdrs', f))==0){
      
      tdf <- data.frame(
        cutoff=gsub('cutoff-','',ss[11]),
        baseline=gsub('baseline-','',ss[12]),
        y=ss[14],
        nrounds=gsub('nrounds-','',ss[18]),
        eta=gsub('eta-','',ss[19]),
        maxdepth=gsub('depth-', '', ss[21]),
        gamma=gsub('gamma-', '',ss[22]),
        colsamp=gsub('colsamp-', '', ss[23]),
        childweight=gsub('childwei-', '', ss[24]),
        subsamp=gsub('.Rdata|subsamp-','',ss[25]),
        perf_e=cm[[1]],
        perf_k=cm[[2]],
        perf_s=cm[[3]])
      
      return(tdf)
      
      }else{
        
        tdf <- data.frame(
          cutoff=gsub('cutoff-','',ss[11]),
          baseline=gsub('baseline-','',ss[12]),
          y=ss[13],
          nrounds=gsub('nrounds-','',ss[15]),
          eta=gsub('eta-','',ss[16]),
          maxdepth=gsub('depth-', '', ss[18]),
          gamma=gsub('gamma-', '',ss[19]),
          colsamp=gsub('colsamp-', '', ss[20]),
          childweight=gsub('childwei-', '', ss[21]),
          subsamp=gsub('.Rdata|subsamp-','',ss[22]),
          perf_e=cm[[1]],
          perf_k=cm[[2]],
          perf_s=cm[[3]])
        
        return(tdf)
        
      }
  
})

pdf <- do.call(rbind, perf)
pdf$nrounds <- droplevels(factor(pdf$nrounds, levels = c(5, 10, 25, 50, 100, 500, 1000, 2000, 3000, 4000, 5000)))
pdf$maxdepth <- droplevels(factor(pdf$maxdepth, levels = c(2, 4, 6, 8, 12, 16, 20, 30)))
pdf$eta <- droplevels(factor(pdf$eta, levels= seq(.025, .85, .05)))
pdf$childweight <- droplevels(factor(pdf$childweight, levels=c(1,2,3)))
pdf$colsamp <- droplevels(factor(pdf$colsamp, levels=c(0.2, 0.4, 0.6, 0.8, 1)))
pdf$gamma <- droplevels(factor(pdf$gamma, levels=c(seq(0, 1, .05))))

m <- melt(pdf, id.vars = c('cutoff', 'baseline', 'y', 'nrounds', 'eta', 'maxdepth', 'gamma', 'colsamp', 'childweight', 'subsamp'))

#m <- m[m$y=='rrsr',]
m <- m[m$subsamp==1 & m$childweight==1 & m$maxdepth==4 & m$baseline==FALSE, ]

ggplot(m, aes(nrounds, value, colour=y, group=y)) + geom_point() + geom_line() + facet_grid(cutoff~variable)

ggplot(m[m$cutoff==0.1,], aes(nrounds, value, fill=nrounds)) + geom_point() + geom_boxplot() + facet_grid(baseline~variable)

boxplot(rowMeans(pdf[, grep('perf', names(pdf))]) ~ pdf$gamma)



mf <- m[m$baseline==FALSE, ]
mt <- m[m$baseline==TRUE, ]

best_true <- lapply(split(mt, list(mt$variable, mt$y)), function(x) x[which.max(x$value), ])
best_false <- lapply(split(mf, list(mf$variable, mf$y)), function(x) x[which.max(x$value), ])

best_fits <- rbind(do.call(rbind, best_true), do.call(rbind, best_false))
write.table(best_fits, file = '/ifshome/bwade/NARSAD/Aim_1/results/final_models/best_fits.txt', quote = F)

idx <- 1
load('/ifshome/bwade/NARSAD/Aim_1/results/refinement/xgbTree_cutoff-0.3_baseline-FALSE_hdrs6_diffdrop-TRUE_nrounds-500_eta-0.025_max_depth-6_gamma-0.3_colsamp-0.4_childwei-1_subsamp-1.Rdata')

pred <- fits[[idx]]$mod$pred

plot(pred$pred, pred$obs)
abline(lm(pred$obs ~ pred$pred))
cor(pred$pred, pred$obs)

plot(varImp(fits[[idx]]$mod))

xgbimp <- xgb.importance(feature_names=fits[[idx]]$mod$finalModel$feature_names, model=fits[[idx]]$mod$finalModel)
#xgb.ggplot.importance(xgbimp)

roi_probe <- 'V42_V190_BnConn'
partial(fits[[idx]]$mod, pred.var = roi_probe, ice = TRUE, center = TRUE, 
        plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "ggplot2", 
        train = fits[[idx]]$data)










