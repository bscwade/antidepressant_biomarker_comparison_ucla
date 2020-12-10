# Updated workflow to accomodate new data: 4/14/2020
setwd('/ifshome/bwade/NARSAD/Aim_1/')

end_tp <- 'end_of_treatment' 

load_clf_data <- function(end_tp, outcome){
  
  setwd('/ifshome/bwade/NARSAD/Aim_1/')
  
  # Demographic/Clinical data: Post treatment
  if(end_tp=='end_of_treatment'){
    source('code/load_clinical_demographic_data_v04142020.R')
    dem <- load_clinical_demographic_data(outcome = outcome)
  }else if(end_tp=='followup'){
    source('code/load_clinical_demographic_data_followup_v04142020.R')
    dem <- load_clinical_demographic_data_followup(outcome = outcome)
  }
  
  # load node-degree connectivity measures
  source('~/NARSAD/Aim_1/code/load_connectivity_data_noresid_graphtheory.R')
  #conn <- load_connectivity_data_noresid_graphtheory() # node-degree distribution 
  load('/ifshome/bwade/NARSAD/Aim_1/data/conn_ndd.Rdata')
  
  # load between-network connectivity measures
  source('~/NARSAD/Aim_1/code/load_connectivity_data_noresid_betwen_network_conn.R')
  load('/ifshome/bwade/NARSAD/Aim_1/data/conn_bnc.Rdata')
  
  # Diffusion data
  source('~/NARSAD/Aim_1/code/load_dti_data.R')
  dti <- load_dti_data()
  dti <- dti[dti$timepoint == '01', ]
  
  # FreeSurfer
  source('~/NARSAD/Aim_1/code/load_aparc.R')
  source('~/NARSAD/Aim_1/code/load_aseg.R')
  aparc <- load_aparc()
  aseg <- load_aseg()
  
  df <- Reduce(function(...) merge(..., by='screen_id'), list(dem, aparc, aseg, conn, conndat_bnc, dti))
  table(df$group)
  
  df <- df[, !names(df) %in% c(grep('hypo', names(df), value = T), 'timepoint', 'uid', 'redcap_event_name')]
  
  # drop sleep deprivation controls labeled with an 's'; they have baseline hdrs scores = 0-1. 
  controls_to_drop <- c('s0006', 's0007', 's0008', 's0010', 's0011', 's0013', 's0014', 's0015', 's0016', 's0019', 's0020', 's0021', 's0023', 's0024', 's0027', 's0028')
  
  problematic_scans_drop <- c('k0051', 'e0024')
  
  df <- df[!df$screen_id %in% c(controls_to_drop, problematic_scans_drop), ]
 
  return(df)
   
}

df <- load_clf_data(end_tp = 'end_of_treatment', outcome='rumq_tcqr_total_st')
save(df, file = '/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/tcqr_end_of_treatment.Rdata')

## Binary classification with feature selection ##
require(caret)
require(parallel)
require(doParallel)
require(randomForest)
require(xgboost)

#df$responder <- factor(ifelse(df$outcome <= -0.5, 'responder', 'nonresponder'))
#table(df$group, df$responder)

# Classification
source('/ifshome/bwade/NARSAD/Aim_1/code/predict_co_rfe.R')
results <- lapply(levels(df$group), function(x){
  
  print(x)
  
  regdat <- df[df$group == x, ]
  
  print(dim(regdat))
  
  regdat <- regdat[, !names(regdat) %in% c('group')]
  
  res <- predict_co_rfe(y='outcome', df=regdat, 
                        nboot = 50, 
                        quantile_threshold=.9, 
                        ntree=1000, 
                        rfetree=500, 
                        nrfe=5, 
                        var_freq=3, 
                        custom_of = TRUE, 
                        RFE = TRUE, 
                        cbf=TRUE,
                        clf = 'rf')
  
})

## Evaluate RFE Version
idx <- 2

pred <- c(sapply(results[[idx]], function(x) x$predicted))
actual <- c(sapply(results[[idx]], function(x) x$actual))
tdf <- data.frame(pred, actual)
tdf <- tdf[tdf$actual> -15, ]

plot(pred, actual)
cor(pred, actual)
cor.test(pred, actual)

train_pred <- c(sapply(results[[idx]], function(x) x$train_pred))
train_actual <- c(sapply(results[[idx]], function(x) x$train_actual))
cor(train_pred, train_actual)
plot(train_pred, train_actual, col='red', pch=19)
plot(train_pred, train_actual)

plot(c(unlist(sapply(results[[idx]], function(x) x$predicted))), c(unlist(sapply(results[[idx]], function(x) x$actual))))
abline(lm(c(unlist(sapply(results[[idx]], function(x) x$actual))) ~ c(unlist(sapply(results[[idx]], function(x) x$predicted)))))

f <- sapply(results[[idx]], function(r) r$features)
table(unlist(f))[table(unlist(f)) > quantile(table(unlist(f)), .9)]

tmp <- names(table(unlist(f)))[table(unlist(f)) ==50]
grep('cingulate', tmp, value = T)


## Assess gini importance scores ##
vlist <- lapply(results[[2]], function(x){
  vimp <- data.frame(x$varimp)
  vimp$roi <- gsub('`','', rownames(vimp)) #rownames(vimp)
  return(vimp)
})

vimpmat <- do.call(cbind, lapply(vlist, function(x) x$IncNodePurity))
rowSums(vimpmat)

idf <- data.frame(roi=vlist[[1]]$roi, imp=rowSums(vimpmat))

idf[order(idf$imp, decreasing = TRUE),]


## Assess gini importance scores, option 2 ##
ordered_vlist <- lapply(vlist, function(v){
  v[order(v$IncNodePurity, decreasing=TRUE), ][1:30, ]
})

tab <- table(unlist(lapply(ordered_vlist, function(x) x$roi)))
tab[tab>quantile(tab, .9)]

# model lookup
mod <- caret::modelLookup()
tail(mod[mod$forReg==TRUE, ], 100)



dftmp <- df
ifelse(dftmp$outcome)

# Correlation filter internal
filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)
set.seed(10)
#rfWithFilter <- sbf(x, y, sbfControl = filterCtrl)
rfWithFilter <- sbf(x=subset(df, select = -outcome), y=df$outcome, sbfControl = filterCtrl)


# THink about adding
tdf <- df
tdf <- tdf[, !names(tdf) %in% c('outcome', 'outcome_baseline', 'group')]


fc <- findCorrelation(tdf, cutoff = .9, names=TRUE)

tdf <- tdf[, !names(tdf) %in% fc]




## One Shot Pass ##
require(caret)
require(parallel)
require(doParallel)
require(randomForest)

cl2 <- makeCluster(25)
doParallel::registerDoParallel(cl2)

df <- load_clf_data(end_tp = 'end_of_treatment')

fitted <- lapply(c('e', 'k', 's'), function(g){
  
  print(g)
  
  cdf <- df[df$group==g, ]
  exclude <- grep('_AD|_MD|_RD|_kurt', names(df), value = T)
  cdf <- cdf[, !names(cdf)%in%exclude]
  fc <- findCorrelation(cdf[, !names(cdf) %in% c('group', 'outcome', 'outcome_baseline')], names=T, cutoff = .5)
  cdf <- df[df$group==g, !names(df) %in% c(fc, names(cdf)[nzv(cdf)], exclude)]
  
  trCtrl <- trainControl(method = 'repeatedcv', 
                         number = 10,
                         repeats = 10,
                         savePredictions = TRUE,
                         allowParallel = TRUE)
  
  train_fit <- train(outcome ~ .,
                     data = cdf,
                     method = 'xgbTree',
                     trControl = trCtrl,
                     ntree = 1000,
                     tuneLength=5)
  
  return(list(
    mod=train_fit,
    data=cdf
  ))
  
})

registerDoSEQ()

idx <- 1

imp <- importance(fitted[[idx]]$mod$finalModel)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op <- par(mfrow=c(3, 3))
for (i in seq_along(impvar[1:9])) {
  partialPlot(fitted[[idx]]$mod$finalModel, fitted[[idx]]$data, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
}
par(op)

randomForest::partialPlot(x=train_fit$finalModel, pred.data=cdf, 'Tapetum_L_kurt')
randomForest::varImpPlot(train_fit$finalModel)

fitted[[idx]]$mod
pred <- fitted[[idx]]$mod$pred
#pred <- pred[pred$mtry==2,]# RF

sigma=fitted[[idx]]$mod$bestTune[['sigma']]
C=fitted[[idx]]$mod$bestTune[['C']]
pred <- pred[pred$sigma==sigma & pred$C==C,]# svm

plot(pred$pred, pred$obs)
cor(pred$pred, pred$obs)
cor.test(pred$pred, pred$obs)
abline(lm(pred$obs~pred$pred))






