## Create model
trCtrl <- trainControl(method = 'cv', 
                       number = 10,
                       allowParallel = TRUE)

tmp <- df[, c('qids_percent_change', names(df)[!names(df) %in% c('redcap_event_name', 'screen_id', 'uid', 'timepoint')])]

data_split <- split(tmp, f = tmp$group)

data_split <- lapply(data_split, function(x){
  #subset(x, select = -qids_percent_change.1)
  #x <- x[, !names(x) %in% c('qids_percent_change.1', 'group', grep('^X', names(tmp), value = T))]
  x <- x[, !names(x) %in% c('qids_percent_change.1', 'group')]
  return(x)
})


require(caret)
require(doParallel)
cl2 <- makeCluster(6)
registerDoParallel(cl2)

fits <- lapply(data_split, function(f){
  
  train_fit <- train(qids_percent_change ~ ., 
                     data = f,
                     method = 'rf', 
                     metric = 'RMSE', 
                     trControl = trCtrl,
                     ntree = 1000,
                     tuneLength=5)
  
  return(train_fit)
  
})

randomForest::varImpPlot(fits[[1]]$finalModel)

cm <- sapply(fits, function(x){
  sapply(data_split, function(y){
    cor(y$qids_percent_change, predict(object = x, newdata = y))
  })
})

#plot
par(mfrow=c(3,3))
sapply(fits, function(x){
  sapply(data_split, function(y){
    plot(y$qids_percent_change, predict(object = x, newdata = y))
  })
})

#predict(object = train_fit, newdata = test_data[, c(outcome, selected_variables, cm_forced)])
predict(object = fits[[2]], newdata = data_split[[1]])






