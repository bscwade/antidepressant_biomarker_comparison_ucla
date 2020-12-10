
grid <- expand.grid(y=c('rumq_rrsr_total_st', 'hdrs6'),
                    feature=c('diffusion','asegaparc','globalconn','betweennetwork'),
                    n=c(5, 10, 20, 40, 60, 80, 100, 200, 500, 1000, 2000),
                    group=c('e', 'k', 's'),
                    diff_drop=TRUE)

source('~/NARSAD/Aim_1/code/performance_by_feature_and_n.R')

odir <- '/ifshome/bwade/NARSAD/Aim_1/results/by_feature/'

#cl2 <- makeCluster(25)
#doParallel::registerDoParallel(cl2)
cl <- makePSOCKcluster(20)
registerDoParallel(cl)

for(i in 1:nrow(grid)){
  
  print(i)
  
  
  if(file.exists(sprintf('%s%s_%s_%s_%s.Rdata', odir, grid$y[i], grid$feature[i], grid$n[i], grid$group[i]))){
    
    next
    
  }else{
    
    perf <- performance_by_feature_and_n(feature = grid$feature[i], n = grid$n[i], group = grid$group[i], y = grid$y[i])
    
    save(perf, file=sprintf('%s%s_%s_%s_%s.Rdata', odir, grid$y[i], grid$feature[i], grid$n[i], grid$group[i]))
    
  }
  
}

stopCluster(cl)
registerDoSEQ()



fid <- dir(odir, full.names = TRUE, pattern='rrsr')


p <- sapply(fid, function(f){
  load(f)
  return(perf)
})

df <- data.frame(y=sapply(fid, function(f){strsplit(f, '_')[[1]][4]}),
           feature=sapply(fid, function(f){strsplit(f, '_')[[1]][7]}),
           n=sapply(fid, function(f){strsplit(f, '_')[[1]][8]}),
           arm=gsub('.Rdata','', sapply(fid, function(f){strsplit(f, '_')[[1]][9]})),
           perf=p)

rownames(df) <- NULL

df$n <- factor(df$n, levels = c(5, 10, 20, 40, 60, 80, 100, 200, 500, 1000, 2000))

ggplot(df, aes(n, perf, colour = feature, group=as.factor(feature))) + 
  geom_point() + 
  geom_line(size=2) + 
  facet_wrap(~arm)









