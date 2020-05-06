require(ggplot2)
require(plyr)
require(lubridate)
require(readODS)
require(tidyr)
require(dplyr)
require(ranger)
require(caret)

### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')
Sys.getenv('TZ') # make sure it got set

### load the combined data
comb_all <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/data/combined_data/pressure_bomb_combined_data.rds')




# random forest indicates these variables are importnat:
# irrig, leaftemp_highest, VPD_leaf, treatment, leaftemp_mean
# less important: minutes, VPD_air, bmp_box_temp, sht2_low_temp, sht2_low_rh


# calculate air-leaf temp diffs
comb_all$altd <- comb_all$temp_high_mean - comb_all$leaftemp_mean
plot(comb_all$altd, comb_all$mean_psi_MPa, col=comb_all$treatment)
cor(comb_all$altd, comb_all$mean_psi_MPa, use='complete.obs')

#for testing below
x = subset(comb_all, date=='2019-11-06' & block == 'D')

# mean altd daily
mean_altd <- ddply(comb_all, .(date, block), function(x) {
  # uncomment to calculate for previous day
  # x = subset(comb_all, date == (unique(x$date)-1) & block == unique(x$block))
  y = subset(x, hour(by15) >= 9 & hour(by15) <= 14)
  setNames(mean(y$altd, na.rm=T) , 'mean_altd')
})
ggplot(mean_altd, aes(x=date, y=mean_altd, color=block)) + geom_line() +
  scale_x_date(breaks='1 week', date_labels = '%m-%d')

# now add this data back to comb_all
for(i in 1:nrow(comb_all)) {
  d=comb_all$date[i]; b=comb_all$block[i]
  a=mean_altd$mean_altd[mean_altd$date==d & mean_altd$block==b]
  comb_all[i,'mean_altd'] <- a
}



# ---- mean VPD daily
mean_vpd_leaf <- ddply(comb_all, .(date, block), function(x) {
  # uncomment to calculate for previous day
  # x = subset(comb_all, date == (unique(x$date)-1) & block == unique(x$block))
  y = subset(x, hour(by15) >= 9 & hour(by15) <= 14)
  setNames(mean(y$VPD_air, na.rm=T) , 'mean_vpd_leaf')
})
ggplot(mean_vpd_leaf, aes(x=date, y=mean_vpd_leaf, color=block)) + geom_line() +
  scale_x_date(breaks='1 week', date_labels = '%m-%d')

# now add this data back to comb_all
for(i in 1:nrow(comb_all)) {
  d=comb_all$date[i]; b=comb_all$block[i]
  a=mean_vpd_leaf$mean_vpd_leaf[mean_vpd_leaf$date==d & mean_vpd_leaf$block==b]
  comb_all[i,'mean_vpd_leaf'] <- a
}


# --- mean PAR for CURRENT hour .... DOESNT SEEM USEFUL.
#for testing below
x = subset(comb_all, date=='2019-11-06' & hour == 11  & block == 'D')

# comb_all$hour <- hour(comb_all$by15)

# probably should do this in more sophisticted way (use hour immediately previous)
# mean_par <- ddply(comb_all, .(date, hour, block), function(x) {
#   setNames(mean(x$par1_n, na.rm=T) , 'mean_par')
# })
# ggplot(mean_par, aes(x=date, y=mean_par)) + geom_line() +
#   scale_x_date(breaks='1 week', date_labels = '%m-%d')
# # now add this data back to comb_all
# for(i in 1:nrow(comb_all)) {
#   d=comb_all$date[i]; h=comb_all$hour[i]; b=comb_all$block[i]
#   a=mean_par$mean_par[mean_par$date==d & mean_par$hour==h & mean_par$block==b]
#   comb_all[i,'mean_par'] <- a
# }

# add PAR threshold variable
df2$par_threshold <- ifelse(df2$line_PAR_west_umol_m2_s > 300, 1, 0)
