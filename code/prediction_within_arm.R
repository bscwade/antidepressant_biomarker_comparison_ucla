# To do:
# pass R2 metric to both RFE and train

require(caret)
require(MLmetrics) # calculates sum of squres based R2
require(randomForest)
require(parallel)
require(doParallel)

cl <- makePSOCKcluster(20)
registerDoParallel(cl)

# Set parameters here
classifier <- 'rf'
atlas <- 'mmp'
nfolds <- 10
demographics <- FALSE
fc_filter <- TRUE

if(atlas=='mmp'){ # mmp requires more regulation to protect against stack overflow memory error
  hyperparam_grid <- expand.grid(correlation_cutoff=c(0.3, 0.5),
                                 num_trees = c(1000, 5000))
}else{
  hyperparam_grid <- expand.grid(correlation_cutoff=c(0.0, .1, .2, .3),
                                 num_trees = c(500, 1000, 2500))
}


# custom metric
## See https://topepo.github.io/caret/model-training-and-tuning.html#metrics
RSQ_SS <- function(data, lev = NULL, model = NULL) {
  rsqss <- MLmetrics::R2_Score(y_pred = data$pred, y_true = data$obs)
  c(R2SS = rsqss)
}

## load data
# load('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/hdrs6_end_of_treatment.Rdata')
# df$cont_sex <- as.factor(df$cont_sex)
#df <- df[, c('outcome','group', grep('lh_|rh_', names(df), value = T))]

source('/ifshome/bwade/NARSAD/Aim_1/code/load_atlas_rsfc.R')
source('/ifshome/bwade/NARSAD/Aim_1/code/load_dti_data.R')

dem <- read.csv('/ifshome/bwade/NARSAD/Aim_1/data/clinical_data/clinical_demographic_data_12012020.csv')
dem <- plyr::rename(dem, replace = c('Subject'='screen_id', 'hdrs6'='outcome'))
rsfc <- load_atlas_rsfc(atlas=atlas)
dti <- load_dti_data()
dti <- dti[dti$timepoint=='01', ]
dti <- dti[, !names(dti) %in% c('uid', 'timepoint')]

img <- merge(rsfc, dti, by = 'screen_id')
df <- merge(dem[, c('screen_id', 'outcome', 'age', 'sex', 'arm')], img, by = 'screen_id')

colnames(df) <- gsub('-', '_', colnames(df))

# subset data 
if(demographics==FALSE){
  df <- df[, !names(df) %in% c('age', 'sex', 'dabst_duration', 'dabst_num_episodes', 'outcome_baseline')]
}

print('Iterating Over Treatments...')
fitted <- lapply(c('e', 'k', 's'), function(g){ # iterate over treatment arms
  print(sprintf('Treatment arm: %s', g))
  
  df_red <- df[df$arm==g, ]
  
  performance_summary <- list()

  for(i in 1:nfolds){ # iterate over folds
    print(sprintf('Fold %s / %s', i, nfolds))
    
    set.seed(i)
    in_train <- createFolds(df_red$outcome, k=10, list=FALSE)>1
    
    data_train <- df_red[in_train, !names(df_red) %in% c('screen_id', 'arm')]
    data_test <- df_red[!in_train, !names(df_red) %in% c('screen_id', 'arm')]
    
    # impute any missings based on training data: done outside train function due to nested CV method
    medimp <- preProcess(data_train, method = 'medianImpute')
    data_train <- predict(medimp, data_train)
    data_test <- predict(medimp, data_test)
    
    hyperparameter_performance_list <- list()
    
    for(u in 1:nrow(hyperparam_grid)){
      
      print(sprintf('Hyperparameter grid search %s / %s', u, nrow(hyperparam_grid)))
    
      # mild correlation-based filter to speed up RFE (note: RFE-selected features are highly correlated with outcome so this is justified)
      cm <- sapply(data_train, function(x) cor(x, data_train[, 'outcome']))
      test <- which(abs(cm) >= hyperparam_grid$correlation_cutoff[u] )
      if(length(test) < 5 ){
        
        train_performance <- list(
          RSQSS = NA,
          R2COR = NA,
          MAE = NA,
          RMSE = NA,
          model = NA,
          correlation_cutoff = NA,
          ntrees = NA
        )
        
      }else{
        
        cm <- which(abs(cm) >= hyperparam_grid$correlation_cutoff[u] )
        cm <- cm[!names(cm) %in% c('outcome')]
        data_train <- data_train[, c('outcome', names(cm))]
        
        if(fc_filter==TRUE){
          fc <- findCorrelation(cor(subset(data_train, select = -outcome)), cutoff=.5, names=TRUE)
          data_train <- data_train[, !names(data_train) %in% fc]
        }
        
        if(classifier=='rf' || classifier == 'xgbTree'){
          print('Processing RF-based RFE')
          
          # recursive feature elimination using permutation-based importance scores
          rfFuncs$fit <- function(x,y,first,last,...) { library(randomForest); randomForest(x, y, importance = first, ntree = 1000)}
          
          control <- rfeControl(functions=rfFuncs, 
                                method="cv", 
                                number=10,
                                allowParallel = TRUE)
          
          results <- rfe(x=subset(data_train, select = -outcome), 
                         y=data_train$outcome, 
                         sizes = ceiling(seq(from=5, to=ncol(data_train)-1, length.out=10)),
                         rfeControl=control)
        }else{
          print('Processing SVM-based RFE')
          
          svmrfecontrol <- rfeControl(functions=caretFuncs, 
                                      method="cv", 
                                      number=10,
                                      allowParallel = FALSE)
          
          results <- rfe(x=subset(data_train[, !names(data_train) %in% c('outcome','cont_sex')]), 
                         y=data_train$outcome, 
                         method='svmLinear',
                         preProc = c("center", "scale"),
                         sizes = ceiling(seq(from=5, to=ncol(data_train)-1, length.out=10)),
                         rfeControl=svmrfecontrol)
          
        }
        
        print('Processing Model...')
        trCtrl <- trainControl(method = 'adaptive_cv', 
                               number = 10,
                               repeats = 1,
                               adaptive = list(min=8, alpha=0.05,
                                               method='gls', complete=TRUE),
                               savePredictions = 'final', 
                               verboseIter = TRUE,
                               allowParallel = TRUE)
        
        set.seed(1)
        train_fit <- train(outcome ~ .,
                           data = data_train[, c('outcome', results$optVariables)],
                           method = classifier,
                           ntree = hyperparam_grid$num_trees[u], 
                           importance = TRUE,
                           trControl = trCtrl,
                           tuneLength=5,
                           nthread=10)
        
        train_performance <- list(
          RSQSS = MLmetrics::R2_Score(y_pred = train_fit$finalModel$predicted, y_true = data_train$outcome),
          R2COR = cor(train_fit$finalModel$predicted, data_train$outcome)^2,
          MAE = MLmetrics::MAE(y_pred = train_fit$finalModel$predicted, y_true = data_train$outcome),
          RMSE = MLmetrics::RMSE(y_pred = train_fit$finalModel$predicted, y_true = data_train$outcome),
          model = train_fit$finalModel,
          correlation_cutoff = hyperparam_grid$correlation_cutoff[u],
          ntrees = hyperparam_grid$num_trees[u]
        )
        
      }
      
      hyperparameter_performance_list[[u]] <- train_performance
      
    } # end hyper parameter search
    
    # evaluate which hyperparameter combination is best and use that model
    best_train <- which.max(sapply(hyperparameter_performance_list, function(x) x$RSQSS))
    best_model <- hyperparameter_performance_list[[best_train]]$model
    
    pred <- predict(best_model, data_test)
    
    test_performance <- list(
      RSQSS = MLmetrics::R2_Score(y_pred = pred, y_true = data_test$outcome),
      R2COR = cor(pred, data_test$outcome) ^ 2,
      MAE = MLmetrics::MAE(y_pred = pred, y_true = data_test$outcome),
      RMSE = MLmetrics::RMSE(y_pred = pred, y_true = data_test$outcome),
      predicted = pred,
      actual = data_test$outcome,
      best_model = best_model
    )
    
    performance_summary[[i]] <- test_performance
    
  }
    
  return(performance_summary)
  
})


#save(fitted, file=sprintf('/ifshome/bwade/NARSAD/Aim_1/results/within_arm_performance_classifier-%s_atlas-%s_demographics-%s.Rdata', classifier, atlas, demographics))
save(fitted, file=sprintf('/ifshome/bwade/NARSAD/Aim_1/results/tests/within_arm_performance_classifier-%s_atlas-%s-filtered_demographics-%s.Rdata', classifier, atlas, demographics))








