setwd('/ifshome/bwade/NARSAD/Aim_1/')

# Demographic/Clinical data
source('code/load_clinical_demographic_data_v112219.R')
dem <- load_clinical_demographic_data()

source('code/load_clinical_demographic_data.R')
dem2 <- load_clinical_demographic_data_old()

source('~/NARSAD/Aim_1/code/load_connectivity_data_noresid_graphtheory.R')
#conn <- load_connectivity_data_noresid_graphtheory() # node-degree distribution 
load('/ifshome/bwade/NARSAD/Aim_1/data/conn_ndd.Rdata')

# Diffusion data
source('~/NARSAD/Aim_1/code/load_dti_data.R')
dti <- load_dti_data()
dti <- dti[dti$timepoint == '01', ]

# FreeSurfer
source('~/NARSAD/Aim_1/code/load_aparc.R')
source('~/NARSAD/Aim_1/code/load_aseg.R')
aparc <- load_aparc()
aseg <- load_aseg()

df <- Reduce(function(...) merge(..., by='screen_id'), list(dem_hamd, aparc, aseg, conn, dti))
df <- df[, !names(df) %in% c(grep('hypo', names(df), value = T), 'timepoint', 'uid', 'redcap_event_name', 'screen_id')]

## Binary classification with feature selection ##
require(caret)
require(parallel)
require(doParallel)

df$responder <- factor(ifelse(df$qids_percent_change <= -0.5, 'responder', 'nonresponder'))
table(df$group, df$responder)

classification_list <- lapply(levels(df$group), function(x){
  
  print(x)
  
  cl2 <- makeCluster(25)
  doParallel::registerDoParallel(cl2)
  
  # Restrict to treatment group
  classification_data <- df[df$group==x, !names(df) %in% c('screen_id', 'qids_percent_change', grep('group|redcap_event', names(df), value = T))]
  print(dim(classification_data))
  
  nflds <- 10 #nrow(classification_data)
  nrfe <- 3
  outcome <- 'responder'
  
  set.seed(1)
  flds <- caret::createFolds(y = classification_data$responder, k = nflds, list = FALSE)
  
  outer_folds <- lapply(1:nflds, function(f){
    
    train_data <- classification_data[flds!=f, ]
    test_data <- classification_data[flds==f, ]
    
  
    # RFE feature selection
    rfeVars <- lapply(1:nrfe, function(r){
      
      set.seed(r)
      inRFE <- createFolds(train_data[[outcome]], k = 10, list = FALSE)
      
      print(sprintf('RFE Iteration %s', r))
      rfe_dat <- train_data[inRFE != r, ]
      rfFuncs$fit <- function(x,y,first,last,...) {library(randomForest); randomForest(x, y, importance = first, ntree = 500, ...) } # 1K trees for RFE
      control <- rfeControl(functions=rfFuncs, method="cv", number=10, allowParallel = TRUE)
      results <- rfe(subset(rfe_dat, select = -get(outcome)), rfe_dat[[outcome]], sizes = c( round(seq(from=1, to = ncol(rfe_dat), length.out = 20)) ), rfeControl = control, metric = 'Accuracy')
      return(results$optVariables)
      
    })
    
    tab <- table(unlist(rfeVars))
    #selected_variables <- names(tab)[tab>=quantile(tab, quantile_threshold)] 
    selected_variables <- names(tab)[tab>=quantile(tab, .5)]
    
    
    ## Create model
    trCtrl <- trainControl(method = 'cv', 
                           number = 10,
                           savePredictions = TRUE,
                           #classProbs = TRUE,
                           allowParallel = TRUE)
    
    train_fit <- train(responder ~. , 
                       data = train_data[, c('responder', selected_variables)],
                       method = 'rf', 
                       metric = 'Accuracy', 
                       trControl = trCtrl,
                       ntree = 1000,
                       tuneLength=3)
    
    pred_class <- predict(object = train_fit, newdata = test_data)
    actual_class <- test_data$responder
    
    return(list(
      model=train_fit,
      predicted=pred_class,
      actual=actual_class,
      variables=selected_variables
    ))
    
  })
  
  stopCluster(cl2)
  registerDoSEQ()
  
  return(outer_folds)
  
})


idx <- 2

caret::confusionMatrix(unlist(sapply(classification_list[[idx]], function(x) x$actual)), unlist(sapply(classification_list[[idx]], function(x) x$predicted)))

tab <- table(unlist(sapply(classification_list[[idx]], function(x) x$variables)))
names(tab)[tab>quantile(tab, .9)]













## Variable Outcome Approach ##
source('~/NARSAD/Aim_1/code/load_clinical_demographic_data_variable_outcome.R')
dem_hamd <- load_clinical_demographic_data_variable_outcome(outcome = 'hdrs6', percent = FALSE)

# load the rest up top for now then return here #

source('/ifshome/bwade/NARSAD/Aim_1/code/predict_co_rfe.R')
results <- lapply(levels(df$group), function(x){
  
  print(x)
  
  regdat <- df[df$group == x, ]
  
  print(dim(regdat))
  
  regdat <- regdat[, !names(regdat) %in% c('group')]
  
  res <- predict_co_rfe(y='outcome_percent_change', df=regdat, nboot = 50, quantile_threshold=.9, ntree=500, rfetree=200, nrfe=5, var_freq=1, custom_of = TRUE, RFE = FALSE, clf = 'glmnet')
  
})


## Evaluate RFE Version
idx <- 2

pred <- c(sapply(results[[idx]], function(x) x$predicted))
actual <- c(sapply(results[[idx]], function(x) x$actual))
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




