# Scripts used to generate results for "Cross-treatment Model Performance" section. 

load('/ifshome/bwade/NARSAD/Aim_1/results/cross_correlation_results_with_parameters.Rdata')

df$basemodel <- paste0('basemodel_', df$basemodel)
df$crosstx <- paste0('predicting_', df$crosstx)

df <- df[df$y=='hdrs6', ]

d <- df[!is.na(df$cross),]
d <- d[d$baseline == TRUE, ]

plot(d$base, d$cross)
cor(d$base, d$cross)

fit <- lm(cross ~ cutoff + ndthresh + nrounds + eta + maxdepth + colsamp + childweight + subsample + base, data = d)

ds <- split(d, f = list(d$basemodel, d$crosstx))

sapply(ds, function(x) cor(x$base, x$cross))

sapply(ds, function(x) mean(x$cross))

sapply(ds, function(x) tryCatch(cor.test(x$base, x$cross)$p.value, error=function(e) NA)) <= 0.05
