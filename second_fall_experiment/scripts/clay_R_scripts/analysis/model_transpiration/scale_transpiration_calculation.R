
rm(list = ls())
require(plyr); require(ggplot2); require(lubridate)

### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')
Sys.getenv('TZ') # make sure it got set

# read in 15-minute aggregated (and flagged) balance data 
baldat <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_long_aggflag.rds')

# rename the 15-minute timestamp column to match other datasets
names(baldat)[names(baldat)=='roundTime'] <- 'by15'

# show flags: we will filter data later using these
table(baldat$flag)

# ensure data is in chronological order (by plant_id)
baldat <- baldat[order(baldat$plant_id, baldat$by15), ]

# gap analysis
# s <- split(baldat, baldat$scale)
# s2 <- lapply(s, function(x){
#   x = x[order(x$by15), ]
#   x$d = c(NA, diff(x$by15))
#   return(x)
# })
# s3 <- do.call(rbind, s2)


# # table time diffs, see where there are gaps
# x=diff(baldat$by15)
# table(x)
# gaps = which(!x %in% c(0,900)) # 900 = 60*15 minutes
# # NOTE: not sure how these gaps got there; would need to revisit data prep script...
# # but they are pretty minimal so I'll carry on for now.
# for(i in gaps) {
#   print(baldat[(i-1):(i+1),])
# } 



# split into 2 sets of data: with flags and without
# only use the noflag data to calculate transpiration, 
# but then at the end we will re-combine the two sets for completeness
table(baldat$flag)
badflags <- c(1:3, 'irrig1', 'irrig2', 'irrig3', 'man')

baldat_noflag <- baldat[!baldat$flag  %in% badflags, ]
baldat_flag <- baldat[baldat$flag %in% badflags, ]
nrow(baldat) == nrow(baldat_flag) + nrow(baldat_noflag)

# ensure data is in chronological order (by plant_id)
baldat_noflag <- baldat_noflag[order(baldat_noflag$plant_id, baldat_noflag$by15), ]
baldat_flag <- baldat_flag[order(baldat_flag$plant_id, baldat_flag$by15), ]


# calculate transpiration (by plant_id) using 1st-order differences
s_weight <- split(baldat_noflag$mean_weight_kg, baldat_noflag$plant_id)
s_time <- split(baldat_noflag$by15, baldat_noflag$plant_id)

# note: need the "as.numeric()" here to make sure they all are converted to seconds (not minutes)
sdiff_weight <- lapply(s_weight, function(x) c(diff(as.numeric(x)), NA))
sdiff_time <- lapply(s_time, function(x) c(diff(as.numeric(x)), NA))
all(sapply(sdiff_weight, length) == sapply(sdiff_time, length))


# Calculate transpiration as differences in weight between 15-minute steps
# Note: this code finds any gaps that are not 15-minutes and omits them from T calculation.(NA)
baldat_noflag$T_mg_s <- NA
for(plant in names(sdiff_weight)) {
  ind <- baldat_noflag$plant_id == plant
  wt_diffs <- as.numeric(unlist(sdiff_weight[plant]))
  time_diffs <- as.numeric(unlist(sdiff_time[plant]))
  gaps <- time_diffs != 900 # gaps are anything that is not 900 seconds (or 15 mins.)
  wt_diffs[gaps] <- NA # omit any diffs that are not 15-minute gaps
  baldat_noflag$T_mg_s[ind] <- -(wt_diffs) # change transpiration sign to positive
}
# convert from kg/15-min to mg/s
baldat_noflag$T_mg_s <- baldat_noflag$T_mg_s / (15*60) * 1E6
summary(baldat_noflag$T_mg_s)

# change any flagged data transpiration values to NA
baldat_flag$T_mg_s <- NA

# combine flagged and not flagged data back together
transp <- rbind(baldat_flag, baldat_noflag)

# # examine data
# by(transp, transp$block, function(x) summary(x$T_mg_s)) # why so many NA in block D?
# table(transp$block)

sub=subset(transp, date=='2019-10-24' & block=='W')
ggplot(sub, aes(x=by15, y=T_mg_s, color=plant_id)) + geom_line() + geom_point()
ggplot(sub, aes(x=by15, y=mean_weight_kg, color=plant_id)) + geom_line() + geom_point()

# plot -- some obviously bad data a bit after Nov 15...
ggplot(transp[transp$scale==5,], aes(x=by15, y=T_mg_s, color=scale)) +
  geom_line()

i = which(transp$T_mg_s > 100)
transp[i,] # nov. 19...
ggplot(subset(transp, by15 > '2019-11-19 08:00' & by15 < '2019-11-19 11:00'),
       aes(x=by15, y=T_mg_s, color=scale)) + geom_line()

# something fishy going on at 10:00 am, most scales...just NA them all for this time point
transp$T_mg_s[transp$by15 == '2019-11-19 10:00'] <- NA

# scale 5 has obvious outlier
ggplot(subset(transp, by15 > '2019-11-19 14:00' & by15 < '2019-11-19 16:00'),
      aes(x=by15, y=T_mg_s, color=scale)) + geom_line()
transp$T_mg_s[transp$by15 == '2019-11-19 15:15' & transp$scale == 5] <- NA

# x = transp$T_mg_s[baldat$hour >= 9 & baldat$hour <= 17]
x = transp$T_mg_s
summary(x); plot(density(x, na.rm = T))

# examine T above 100 or below -10 
high <- which(transp$T_mg_s > 100); length(high)
low <- which(transp$T_mg_s < -10); length(low)
View(transp[low,])
# not sure about these; I'll just filter out negatives during analysis


# read in leaf area (modeled) data
# note: the follwing dates and blocks were outside the range of any measurments, 
# so we may wish to exclude the leaf area estimates for them:
# 10/24 to 10/29 (all blocks), and 11-28 to 12-05 (W block only (virgins))
la <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/continuous_LA_pred_for_analysis.rds')


# Merge leaf area to other data
transp <- merge(transp, la, by = c('date','block'), all.x = T)
summary(transp$mean_plant_leaf_area_cm2) 

# note: 10-23 doesn't have leaf area data; but this is before treatments began so we will
# exclude this data anyway
transp <- subset(transp, date >= '2019-10-24')


# --- Transpiration needs to be scaled by leaf area. Since we are doing the analysis 
# at the block-level, we use the (mean) total plant leaf area in each block, and the 
# mean transpiration for each block.
# also we want to convert from cm2 to m2 so we divide by 10000.
transp$mean_plant_leaf_area_m2 <- (transp$mean_plant_leaf_area_cm2) / 1E4
transp$mean_plant_leaf_area_cm2 <- NULL
transp$T_mg_m2_s <- transp$T_mg_s / transp$mean_plant_leaf_area_m2
summary(transp$mean_plant_leaf_area_m2)
summary(transp$T_mg_m2_s)
summary(transp$T_mg_s)




# Write raw transpiration data (plant-level, 15 minute) to file
saveRDS(transp, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_transpiration/transpiration_by_plant.rds')

# read back in
# transp <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_transpiration/transpiration_by_plant.rds')

### Examine data distribution (omit flagged data)

# all data
summary(transp$T_mg_m2_s)
plot(density(transp$T_mg_m2_s, na.rm = T))

# daytime data only
daydat <- subset(transp, hour > 3 & hour < 20)
summary(daydat$T_mg_m2_s)
plot(density(daydat$T_mg_m2_s, na.rm = T))

# night data only
nightdat <- subset(transp, hour < 4 | hour > 19)
summary(nightdat$T_mg_m2_s)
plot(density(nightdat$T_mg_m2_s, na.rm = T))


### Now, aggregate to block/treatment level

# first, exclude data from Maria's scales (border plants). These plants were switched multiple times
# and the treatments not always consistent with the other 4 treated plants within the same block.

aggdat <- subset(transp, !scale %in% 15:16)

# aggregate
aggdat <- ddply(aggdat, .(by15, block, treatment), function(x) {
  d = x$T_mg_m2_s[!is.na(x$T_mg_m2_s)]
  setNames(c(mean(d), sd(d), length(d)), c('mean_T_mg_m2_s', 'sd_T_mg_m2_s', 'n_T_mg_m2_s'))
})

# view summary
summary(aggdat$mean_T_mg_m2_s)

# plot
# trt 1
subdat <- subset(aggdat, date(by15) >= '2019-10-24' & date(by15) <= '2019-11-04')
# trt 2
subdat <- subset(aggdat, date(by15) >= '2019-11-05' & date(by15) <= '2019-11-27')
# trt 3
subdat <- subset(aggdat, date(by15) >= '2019-11-28')

table(subdat$block)
ggplot(subdat, aes(x=by15, y=mean_T_mg_m2_s, color=block)) + 
  geom_line()

# Save aggregated data (by block/trt)
saveRDS(aggdat, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_psi_leaf/transpiration_by_block.rds')


### Now, read in combined data (plus psi_leaf predictions) and merge to transpiration data (at block-level)
combdat <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_psi_leaf/combined_data_predicted_psi_leaf.rds')

head(combdat)
head(aggdat)
# keep only the dates after treatments began (all.x = TRUE) 
# (there is scale data from before treatments started)
alldat <- merge(combdat, aggdat, by = c('by15','block','treatment'), all.x = TRUE)

# save the combined data
saveRDS(alldat, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_psi_leaf/combined_data_predict_psi_leaf_transpiration.rds')









### -------- OLD CODE, NOT USED ----------------

require(ggplot2)
subdat <- subset(baldat, 
                   date >= '2019-10-24' & date <= '2019-10-28' &
                   flag == 0)
                   # !flag %in% c('irrig','man') )

# plot all data in subset
ggplot(subdat, aes(x=by15, y=T_mg_s, color = plant_id)) +
  geom_line() + facet_grid(~block) +
  ylim(c(-10, 250))


# look at single day
day <- '2019-11-10'
blk <- 'D'
subdat2 <- subset(baldat, date == day & block == blk)
# only unflagged data
subdat2 <- subset(baldat, date == day & flag == 0 & block == blk)

ggplot(subdat2, aes(x=by15, y=T_mg_s, color = plant_id)) +
  geom_point() + geom_line() 


## Good examples of bad data:
# 11/6, block D and M (spikes)


### try running median
d <- subset(baldat, plant_id=='W-6' & date>='2019-10-25' & date <='2019-11-14' & flag == 0)
d <- subset(baldat, scale == 5 & date>='2019-11-25' & date <='2019-12-12' & flag == 0)

### k = 3-5 seems like a reasonable choice. 
d$rm[!is.na(d$T_mg_s)] <- runmed(d$T_mg_s[!is.na(d$T_mg_s)], k=5)
summary(d$T_mg_s); summary(d$rm)
ggplot(d) +
  geom_line(aes(x=by15, y=rm), color = 'red') +
  geom_point(aes(x=by15, y=T_mg_s), color='blue', alpha = .5) + 
  ylim(c(-50,200))
  

### Do values make sense? (do they add up to a reasonable number?)
d <- subset(baldat, plant_id == 'D-11' & date=='2019-10-23' & flag == 0)
d$rm[!is.na(d$T_mg_s)] <- runmed(d$T_mg_s[!is.na(d$T_mg_s)], k=5)
ggplot(d) +
  geom_line(aes(x=by15, y=rm), color = 'red') +
  geom_point(aes(x=by15, y=T_mg_s), color='blue', alpha = .5) 

# 'integrated' T over day
sum(d$rm/4) # units are in mL_hr, so need to divide by 4 since timesteps are 1/4 hr
sum(d$T_mg_s/4) # using raw transpiration

# absolute water (weight) loss over day
ggplot(d) + geom_point(aes(x=by15, y=mean_weight_kg), color = 'red')
(max(d$mean_weight_kg) - min(d$mean_weight_kg)) * 1000 # raw transpiration based on weights



