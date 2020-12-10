require(caret)
require(doParallel)
cl2 <- makeCluster(6)
registerDoParallel(cl2)

## Create model
trCtrl <- trainControl(method = 'cv', 
                       number = 10,
                       allowParallel = TRUE)

tmp <- df[, c('qids_percent_change', names(df)[!names(df) %in% c('redcap_event_name', 'screen_id', 'uid', 'timepoint')])]
tmp <- subset(tmp, select = -qids_percent_change.1)

fits <- lapply(unique(tmp$group), function(g){
  
  print(g)
  
  train_group <- tmp[tmp$group!=g, ]
  test_group <- tmp[tmp$group==g, ]
  
  train_group <- subset(train_group, select = -group)
  test_group <- subset(test_group, select = -group)
  
  train_fit <- train(qids_percent_change ~ ., 
                     data = train_group,
                     method = 'rf', 
                     metric = 'RMSE', 
                     trControl = trCtrl,
                     ntree = 1000,
                     tuneLength=5)
  
  ret <- data.frame(
    cor=cor(test_group$qids_percent_change, predict(object = train_fit, newdata = test_group)),
    actual=test_group$qids_percent_change,
    pred=predict(object = train_fit, newdata = test_group)
  )
  
  return(ret)
  
})


par(mfrow=c(3,1))
sapply(fits, function(x){
  plot(x$actual, x$pred)
})

