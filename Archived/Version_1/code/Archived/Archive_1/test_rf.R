require(caret)
require(parallel)
require(doParallel)
cl2 <- makeCluster(25)
doParallel::registerDoParallel(cl2)

trCtrl <- trainControl(method = 'repeatedcv', 
                       repeats = 10,
                       number = 10,
                       allowParallel = TRUE,
                       savePredictions = TRUE)

tmpresults <- lapply(levels(df$group), function(x){
  
  print(x)
  
  regdat <- df[df$group == x, ]
  #regdat <- df


  #set.seed(1234)
  set.seed(11)
    
  train_fit <- train(qids_percent_change ~ ., 
                     data = regdat[, !names(regdat) %in% c('screen_id', 'redcap_event_name', 'group', 'timepoint', 'uid')],
                     method = 'rf', 
                     metric = 'RMSE', 
                     #preProcess=c('center', 'scale', 'pca'),
                     trControl = trCtrl,
                     impotance=TRUE,
                     ntree = 1000,
                     tuneLength=5)
  
})

train_fit <- tmpresults[[3]]
par(mfrow=c(1,1))

plot(train_fit$finalModel$predicted, train_fit$trainingData$.outcome)
cor(train_fit$finalModel$predicted, train_fit$trainingData$.outcome)
cor.test(train_fit$finalModel$predicted, train_fit$trainingData$.outcome)
randomForest::varImpPlot(train_fit$finalModel)

# Predict cross-treatment responses
ndat <- df[df$group=='e', ]
ndat <- ndat[, !names(ndat) %in% c('screen_id', 'redcap_event_name', 'group', 'timepoint', 'uid')]

predict(object = tmpresults[[2]], newdata = ndat)
plot(ndat$qids_percent_change, predict(object = tmpresults[[2]], newdata = ndat))
cor(ndat$qids_percent_change, predict(object = tmpresults[[2]], newdata = ndat))
cor.test(ndat$qids_percent_change, predict(object = tmpresults[[2]], newdata = ndat))

load('~/GoogleDrive/BMAP/Studies/Depression_Studies/ACNP_2019/Results/ICA_Correlation_Mapping.Rdata')
refvec[which(refvec$XName=='X173'), ]

# Plot important components
comps <- c(173, 85, 149, 104, 182, 62, 273)

for(c in 1:length(comps)){
  
  png(filename = sprintf('~/Desktop/ACNP_Ketamine_IC/Pair_%s_Maps.png', comps[c]))
  source('~/GoogleDrive/BMAP/Studies/Depression_Studies/ACNP_2019/Code/plot_components.R')
  plotComps(x=comps[c], refvec)
  dev.off()
  
  png(filename = sprintf('~/Desktop/ACNP_Ketamine_IC/Pair_%s_Scatters.png', comps[c]))
  plot(df$qids_percent_change[df$group=='k'], 
       df[[paste0('X',comps[[c]])]][df$group=='k'], 
       pch=19,
       ylab='IC Connectivity',
       xlab = 'QIDS % Change')
  abline(lm(df[[paste0('X',comps[[c]])]][df$group=='k'] ~ df$qids_percent_change[df$group=='k']), col = 'red')
  dev.off()
  
  
}

# Features in common?
idf <- lapply(tmpresults, function(r){
  tidf <- data.frame(r$finalModel$importance)
  tidf$varname <- rownames(tidf)
  tidfordered <- tidf[order(-tidf$IncNodePurity), ]
  return(tidfordered)
})

intersect(idf[[3]]$varname[1:100], idf[[2]]$varname[1:100])


common_vars <- intersect(idf[[3]]$varname[1:100], idf[[2]]$varname[1:100])

par(mfrow=c(4,4))
for(i in 1:16){
  
  plot(df[df$group == 's', 'qids_percent_change'], df[df$group == 's', common_vars[[i]]], main = common_vars[[i]])
  
}








