# newdat is for acnp
# olddat is for ohbm

## 
# aparcs and asegs agree
# qids change agrees

load('~/Desktop/hbm-t1t3-data.Rdata') # dat
load('~/Desktop/acnp-t1t3-data.Rdata') # df

newdat <- df[df$screen_id %in% dat$screen_id, ]
names(dat) <- gsub('[.]', '_', names(dat))
names(dat) <- gsub('_thickness','', names(dat))

dat <- plyr::rename(dat, replace = c('qids1_total_score_pct1t3'='qids_percent_change'))


var_keep <- intersect(names(dat), names(newdat))

newdat <- newdat[, var_keep]
olddat <- dat[, var_keep]

names(newdat)[names(newdat) %in% names(dat)]

sapply(names(newdat), function(x){
  sum(newdat[, x] != olddat[, x])
})

newdat[newdat$screen_id == 'k0060', 'CC_Central']
olddat[olddat$screen_id == 'k0060', 'CC_Central']


df[df$screen_id == 'k0051', 'qids_percent_change']
dat[dat$screen_id == 'k0051', 'qids1_total_score_pct1t3']
