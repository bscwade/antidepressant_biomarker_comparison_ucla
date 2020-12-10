## Script to try using a personalized advantage index
# Should I just load the highest-performing models from Aim 1? 

require(parallel)
require(doParallel)
require(caret)

cl <- makePSOCKcluster(20)
registerDoParallel(cl)

load('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/hdrs6_end_of_treatment.Rdata')

cdf <- df[, -nzv(df)]
cdf <- cdf[, grep('_AD|_MD|_RD|_kurtosis', names(cdf), value = T, invert=T)]

drop <- findCorrelation(cor(cdf[, !names(cdf) %in% c('outcome', 'outcome_baseline', 'screen_id', 'group')]), cutoff = .1, names = TRUE)

cdf <- cdf[, !names(cdf) %in% drop]

fitted <- lapply(c('e', 'k', 's'), function(g){
  
  print(g)
  
  clfdat <- cdf[cdf$group==g, ]
  clfdat <- clfdat[, !names(clfdat) %in% c('screen_id', 'group')]
  
  trCtrl <- trainControl(method = 'repeatedcv', 
                         number = 10,
                         repeats = 1,
                         savePredictions = 'final',
                         allowParallel = TRUE)
  
  train_fit <- train(outcome ~ .,
                     data = clfdat,
                     method = 'xgbTree',
                     trControl = trCtrl,
                     tuneLength=5,
                     nthread=1)
  
  return(train_fit)
  
})


stopCluster(cl)
registerDoSEQ()

par(mfrow=c(1,3))
for(i in 1:3){
  print(cor(fitted[[i]]$pred$obs, fitted[[i]]$pred$pred))
}


# Formulate PAI Scores
actual_change_e <- data.frame(rowidx=fitted[[1]]$pred$rowIndex, obs_change=fitted[[1]]$pred$obs)
actual_change_k <- data.frame(rowidx=fitted[[2]]$pred$rowIndex, obs_change=fitted[[2]]$pred$obs)
actual_change_s <- data.frame(rowidx=fitted[[3]]$pred$rowIndex, obs_change=fitted[[3]]$pred$obs)

xgbimp <- xgb.importance(feature_names=fitted[[1]]$finalModel$feature_names, model=fitted[[1]]$finalModel)

e_to_k <- predict(object = fitted[[1]], cdf[cdf$group=='k',])
e_to_s <- predict(object = fitted[[1]], cdf[cdf$group=='s',])

k_to_e <- predict(object = fitted[[2]], cdf[cdf$group=='e',])
k_to_s <- predict(object = fitted[[2]], cdf[cdf$group=='s',])

s_to_e <- predict(object = fitted[[3]], cdf[cdf$group=='e',])
s_to_k <- predict(object = fitted[[3]], cdf[cdf$group=='k',])

ect <- data.frame(
  actual=cdf[cdf$group=='e', 'outcome'],
  e_pred=fitted[[1]]$pred$pred,
  k_pred=predict(object = fitted[[2]], cdf[cdf$group=='e',]),
  s_pred=predict(object = fitted[[3]], cdf[cdf$group=='e',])
)


ket <- data.frame(
  actual=cdf[cdf$group=='k', 'outcome'],
  e_pred=predict(object = fitted[[1]], cdf[cdf$group=='k',]),
  k_pred=fitted[[2]]$pred$pred,
  s_pred=predict(object = fitted[[3]], cdf[cdf$group=='k',])
)


tsd <- data.frame(
  actual=cdf[cdf$group=='s', 'outcome'],
  e_pred=predict(object = fitted[[1]], cdf[cdf$group=='s',]),
  k_pred=predict(object = fitted[[2]], cdf[cdf$group=='s',]),
  s_pred=fitted[[3]]$pred$pred
)

table(names(sapply(1:nrow(ect), function(x) which.min(ect[x, 2:4]))+1))
table(names(sapply(1:nrow(ket), function(x) which.min(ket[x, 2:4]))+1))
table(names(sapply(1:nrow(tsd), function(x) which.min(tsd[x, 2:4]))+1))


ect_opt <- ect[names(sapply(1:nrow(ect), function(x) which.min(ect[x, 2:4]))+1)=='e_pred', 'actual']
ect_non_opt <- ect[names(sapply(1:nrow(ect), function(x) which.min(ect[x, 2:4]))+1)!='e_pred', 'actual']

ket_opt <- ket[names(sapply(1:nrow(ket), function(x) which.min(ket[x, 2:4]))+1)=='k_pred', 'actual']
ket_non_opt <- ket[names(sapply(1:nrow(ket), function(x) which.min(ket[x, 2:4]))+1)!='k_pred', 'actual']

tsd_opt <- tsd[names(sapply(1:nrow(tsd), function(x) which.min(tsd[x, 2:4]))+1)=='s_pred', 'actual']
tsd_non_opt <- tsd[names(sapply(1:nrow(tsd), function(x) which.min(tsd[x, 2:4]))+1)!='s_pred', 'actual']


opt <- c(ect_opt, ket_opt, tsd_opt)
non_opt <- c(ect_non_opt, ket_non_opt, tsd_non_opt)
boxplot(opt, non_opt, names = c(sprintf('Optimal \n n=%s', length(opt)), sprintf('Non_Optimal \n n=%s', length(non_opt))))


## Excluding ketamine because it's usually picked out as optimal
ect <- data.frame(
  actual=cdf[cdf$group=='e', 'outcome'],
  e_pred=fitted[[1]]$pred$pred,
  s_pred=predict(object = fitted[[3]], cdf[cdf$group=='e',])
)

tsd <- data.frame(
  actual=cdf[cdf$group=='s', 'outcome'],
  e_pred=predict(object = fitted[[1]], cdf[cdf$group=='s',]),
  s_pred=fitted[[3]]$pred$pred
)

table(names(sapply(1:nrow(ect), function(x) which.min(ect[x, 2:3]))+1))
table(names(sapply(1:nrow(tsd), function(x) which.min(tsd[x, 2:3]))+1))

ect_opt <- ect[names(sapply(1:nrow(ect), function(x) which.min(ect[x, 2:3]))+1)=='e_pred', 'actual']
ect_non_opt <- ect[names(sapply(1:nrow(ect), function(x) which.min(ect[x, 2:3]))+1)!='e_pred', 'actual']

tsd_opt <- tsd[names(sapply(1:nrow(tsd), function(x) which.min(tsd[x, 2:3]))+1)=='s_pred', 'actual']
tsd_non_opt <- tsd[names(sapply(1:nrow(tsd), function(x) which.min(tsd[x, 2:3]))+1)!='s_pred', 'actual']

opt <- c(ect_opt, tsd_opt)
non_opt <- c(ect_non_opt, tsd_non_opt)
boxplot(opt, non_opt, names = c(sprintf('Optimal \n n=%s', length(opt)), sprintf('Non_Optimal \n n=%s', length(non_opt))))




## Maybe we need to select subjects across arms with similar baseline scores to emulate randomization? 













