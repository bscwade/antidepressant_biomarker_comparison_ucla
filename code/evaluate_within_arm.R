load('/ifshome/bwade/NARSAD/Aim_1/results/tests/within_arm_performance_classifier-rf_atlas-mmp-filtered_demographics-FALSE.Rdata')

names(fitted[[1]][[1]])

boxplot(sapply(fitted[[1]], function(x) x$RSQSS),
        sapply(fitted[[2]], function(x) x$RSQSS),
        sapply(fitted[[3]], function(x) x$RSQSS))


plot(unlist(sapply(fitted[[2]], function(x) x$predicted)),
     unlist(sapply(fitted[[2]], function(x) x$actual)))



# Calculate average importance across folds
to_df <- function(imp){
 tmp<-data.frame(feature=rownames(imp), importance=imp[, '%IncMSE']) 
 rownames(tmp)<-NULL
 return(tmp)
}

ilist <- lapply(fitted[[2]], function(x) to_df(x$best_model$importance))

idf <- Reduce(function(...) merge(..., all=TRUE, by = 'feature'), ilist)

idf_means <- data.frame(feature=idf$feature, importance=rowMeans(subset(idf, select = -feature), na.rm=TRUE))
idf_means[order(idf_means$importance, decreasing = TRUE), ]



## Simple CV evaluation ##
idx <- 3
plot(fitted[[idx]]$model$predicted, fitted[[idx]]$model$y)







