
rm(list = ls())
require(plyr); require(ggplot2); require(lubridate)

### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')

# read in 15-minute aggregated (and flagged) balance data 
baldat <- readRDS('/home/sean/github/2020_greenhouse/first_summer_experiment/data/scale_output/scale_data_long_aggflag.rds')

# subset to dates on or after treatments began
baldat <- subset(baldat, date >= '2019-08-24')

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
all(sapply(sdiff_weight, length) == sapply(sdiff_time, length)) # check: should be TRUE


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
# by(transp, transp$block, function(x) summary(x$T_mg_s)) # why so many NA in block W?
# table(transp$block, transp$flag) # more data flagged in block W

# plot all data
ggplot(transp, aes(x=by15, y=T_mg_s, color=plant_id)) + geom_line() + facet_grid(~treatment)


# plot subset; NOTE plant D-2 is noisy.... see note below.
sub=subset(transp,  block=='D' & T_mg_s > 0)
by(sub, sub$plant_id, function(x) summary(x$T_mg_s))
boxplot(T_mg_s ~ plant_id, sub)
ggplot(sub, aes(x=by15, y=T_mg_s, color=plant_id)) + geom_line() +  facet_grid(~plant_id)

### ------
### NOTE: Plant D-2 (scale 8) data is noisier than other plant's data. However, from boxplots above (after omitting negative values),
### I'm not convinced the data is that much differet that we need to exclude it from analysis.
### -----

# Plot the treatment means; looks reasonable.
trtMeans <- transp %>% group_by(by15, treatment) %>% summarize(mean_T_mg_s = mean(T_mg_s, na.rm = T))
ggplot(trtMeans, aes(x=by15, y=mean_T_mg_s, color=treatment)) + geom_line()

# remove columns not needed
transp <- select(transp, -c(scale, max_diff_weight, hour))

# rename flag column
colnames(transp)[colnames(transp) == 'flag'] <- 'scale_flag'


# Read in estimated leaf area (modeled using GDD and assuming same model as Experiment 2)
la <- readRDS('/home/sean/github/2020_greenhouse/first_summer_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/plant_leaf_area_predictions.rds')
summary(la)

# rename column 
# Merge leaf area to other data
transp <- merge(transp, la, by = c('date','treatment'), all.x = T)
summary(transp)

# --- Transpiration needs to be scaled by leaf area. 
transp$T_mg_m2_s <- transp$T_mg_s / transp$mean_plant_leaf_area_m2
summary(transp$mean_plant_leaf_area_m2)
summary(transp$T_mg_m2_s)
summary(transp$T_mg_s)

# plot
ggplot(transp, aes(x=by15, y=T_mg_m2_s, color=plant_id)) + geom_line() + facet_wrap(~treatment)

# treatment means, look reasonable
tm <- transp %>% group_by(by15, treatment) %>% summarize(mean_T_mg_m2_s=mean(T_mg_m2_s, na.rm = T))
ggplot(tm, aes(x=by15, y=mean_T_mg_m2_s, color=treatment)) + geom_line() 

# Write raw transpiration data (plant-level, 15 minute) to file
saveRDS(transp, '/home/sean/github/2020_greenhouse/first_summer_experiment/scripts/clay_R_scripts/analysis/model_transpiration/transpiration_by_plant.rds')

# read back in
# transp <- readRDS('/home/sean/github/2020_greenhouse/first_summer_experiment/scripts/clay_R_scripts/analysis/model_transpiration/transpiration_by_plant.rds')
