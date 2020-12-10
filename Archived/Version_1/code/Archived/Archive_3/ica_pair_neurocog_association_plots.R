# Script to plot correlation between pairs of ICA components and neurocognitive outcomes


## Not Run ##
# load('/ifshome/bwade/NARSAD/Aim_1/data/connectivity_mapping_data.Rdata')
# 
# source('/ifshome/bwade/NARSAD/Aim_1/code/Archived/load_connectivity_data_noresid.R')
# conn <- load_connectivity_data_noresid()
# 
# m <- data.frame(matrix(NA, nrow=172, ncol=172))
# colnames(m) <- c(1:73,75:79,81:85,89:90,93:94,98:100,102:104,106,111,114:115,119:122,125:131,133:141,143:144,146:149,151:154,156:200)
# rownames(m) <- c(1:73,75:79,81:85,89:90,93:94,98:100,102:104,106,111,114:115,119:122,125:131,133:141,143:144,146:149,151:154,156:200)
# 
# for(i in 1:nrow(m)){
#   for(j in 1:ncol(m)){
#     m[i,j] <- paste0(rownames(m)[i], '_', colnames(m)[j])
#   }
# }
# 
# colnames(conn) <- c('screen_id', m[upper.tri(m, diag = FALSE)])
# 
# save(conn, file = '/ifshome/bwade/NARSAD/Aim_1/data/labeled_connectivity_pairs.Rdata')

# load labeled connectivity pairs data
load('/ifshome/bwade/NARSAD/Aim_1/data/labeled_connectivity_pairs.Rdata')

# FreeSurfer
source('~/NARSAD/Aim_1/code/load_aparc.R')
source('~/NARSAD/Aim_1/code/load_aseg.R')
aparc <- load_aparc()
aseg <- load_aseg()

# demographic data
source('~/NARSAD/Aim_1/code/load_clinical_demographic_data_variable_outcome.R')
dem <- load_clinical_demographic_data_variable_outcome(outcome = 'rumq_rrsr_total_st', percent = FALSE)

# merge
df <- Reduce(function(...) merge(..., by='screen_id'), list(dem, aparc, aseg, conn))
df <- df[, !names(df) %in% c(grep('hypo', names(df), value = T), 'timepoint', 'uid', 'redcap_event_name', 'screen_id')]

# plot: may have to swap component order
cor(df[['66_75']], df$outcome_percent_change)
plot(df[['66_75']], df$outcome_percent_change)





