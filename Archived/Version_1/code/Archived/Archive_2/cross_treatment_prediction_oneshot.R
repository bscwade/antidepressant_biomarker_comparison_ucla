## Load imaging data

setwd('/ifshome/bwade/NARSAD/Aim_1/')

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


source('~/NARSAD/Aim_1/code/load_clinical_demographic_data_variable_outcome.R')
dem <- load_clinical_demographic_data_variable_outcome(outcome = 'rumq_rrsr_total_st', percent = FALSE)

df <- Reduce(function(...) merge(..., by='screen_id'), list(dem, aparc, aseg, conn))
df <- df[, !names(df) %in% c(grep('hypo', names(df), value = T), 'timepoint', 'uid', 'redcap_event_name', 'screen_id')]

train_data <- df[df$group=='k', !names(df) %in% c('group')]
test_data <- df[df$group=='s', !names(df) %in% c('group')]


corrmsesummary <- function(data,lev=NULL,model=NULL){
  out <- (cor(data$obs, data$pred)*10 - caret::RMSE(data$obs, data$pred)) # correlation - rmse
  names(out) <- 'CorRMSE'
  out
}


## RFE
nrfe <- 10
outcome <- 'rumq_rrsr_total_st'
var_freq <- 1

cl2 <- makeCluster(25)
doParallel::registerDoParallel(cl2)

rfeVars <- lapply(1:nrfe, function(r){
  
  set.seed(r)
  inRFE <- createFolds(train_data[[outcome]], k = 10, list = FALSE)
  
  print(sprintf('RFE Iteration %s', r))
  rfe_dat <- train_data[inRFE != r, ]
  rfFuncs$fit <- function(x,y,first,last,...) {library(randomForest); randomForest(x, y, importance = first, ntree = 200, ...) } # 1K trees for RFE
  
  print("Using Custom Objective Function for RFE")
  rfFuncs$summary <- corrmsesummary
  control <- rfeControl(functions=rfFuncs, method="cv", number=10, allowParallel = TRUE)
  # results <- rfe(subset(rfe_dat, select = -get(outcome)), rfe_dat[[outcome]], 
  #                sizes = c( round(seq(from=1, to = ncol(rfe_dat), length.out = 20)) ), 
  #                rfeControl = control, metric = 'CorRMSE', maximize = TRUE)
  results <- rfe(x=subset(rfe_dat, select = -outcome_percent_change), y=rfe_dat[['outcome_percent_change']], 
                 sizes = c( round(seq(from=1, to = ncol(rfe_dat), length.out = 20)) ), 
                 rfeControl = control, metric = 'CorRMSE', maximize = TRUE)
  
  return(results$optVariables)
  
})

tab <- table(unlist(rfeVars))
selected_variables <- names(tab)[tab>=var_freq]

## end RFE



trCtrl <- trainControl(method = 'cv', 
                       number = 10,
                       summaryFunction = corrmsesummary,
                       savePredictions = TRUE,
                       allowParallel = TRUE)

cl2 <- makeCluster(25)
doParallel::registerDoParallel(cl2)

train_fit <- train(outcome_percent_change~., 
                   data = train_data[, c('outcome_percent_change',selected_variables)],
                   method = 'rf', 
                   metric = 'CorRMSE', 
                   maximize = TRUE,
                   trControl = trCtrl,
                   importance=TRUE,
                   ntree = 5000,
                   tuneLength=5)

registerDoSEQ()


caret::varImp(train_fit$finalModel)
varImpPlot(train_fit$finalModel)

pred <- train_fit$pred[train_fit$pred$mtry==train_fit$bestTune[[1]], 'pred']
obs <- train_fit$pred[train_fit$pred$mtry==train_fit$bestTune[[1]], 'obs']
cor(obs, pred)
plot(obs, pred)

cross_pred <- predict(object = train_fit, newdata = test_data)
cross_actual <- test_data$outcome_percent_change

plot(cross_pred, cross_actual, 
     pch=19,
     cex=.75,
     xlab='Predicted TSD Change',
     ylab='Actual TSD Change')
abline(lm(cross_actual ~ cross_pred),
       col='red',
       lwd=2)


cor(cross_actual, cross_pred)
cor.test(cross_actual, cross_pred)






