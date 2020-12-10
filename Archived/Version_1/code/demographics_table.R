require(plyr)
require(knitr)

load('/ifshome/bwade/NARSAD/Aim_1/data/compiled_datasets/hdrs6_end_of_treatment.Rdata')

## make 99-valued variables NAs?
table((df$dabst_duration == 99), df$group)
table((df$dabst_num_episodes == 99), df$group)

df$dabst_duration[which(df$dabst_duration == 99)] <- NA
df$dabst_num_episodes[which(df$dabst_num_episodes == 99)] <- NA

## Test for between-group demographic differences

# Age
summary(aov(cont_age ~ group, data = df))
TukeyHSD(aov(cont_age ~ group, data = df))

# Sex
chisq.test(table(df$cont_sex, df$group))

## Clinical factors

# baseline symptoms
summary(aov(outcome_baseline ~ group, data = df))
TukeyHSD(aov(outcome_baseline ~ group, data = df))
boxplot(df$outcome_baseline ~ df$group)

# percent change in symptoms
tmp <- data.frame(pc=(df$outcome / df$outcome_baseline), group=df$group)
summary(aov(pc ~ group, data = tmp))
TukeyHSD(aov(pc ~ group, data = tmp))
boxplot(tmp$pc ~ tmp$group)

# depression duration
summary(aov(dabst_duration ~ group, data = df))
TukeyHSD(aov(dabst_duration ~ group, data = df))
boxplot(df$dabst_duration ~ df$group)

# number of episodes
summary(aov(dabst_num_episodes ~ group, data = df))
TukeyHSD(aov(dabst_num_episodes ~ group, data = df))


## Table
tab <- ddply(df, .(group), summarise, n=length(cont_sex), mean_age=mean(cont_age), sd_age=sd(cont_age), males=sum(cont_sex), females=sum(cont_sex==0), sx_baseline=mean(outcome_baseline), sx_baseline_sd=sd(outcome_baseline),
      sx_percent_change=mean(outcome/outcome_baseline), sx_percent_change_sd=sd(outcome/outcome_baseline), depression_duration=mean(dabst_duration, na.rm=T), depression_duration_sd=sd(dabst_duration, na.rm=T), number_of_episodes=mean(dabst_num_episodes, na.rm=T), number_of_episodes_sd=sd(dabst_num_episodes, na.rm=T))

t(tab)








