# Script to estimate soil matric potential from Experiment 1.
# Uses modeled aboveground plant wet weights (daily block means) of plants in Experiment 2,
# as a rough stand-in for those in Experiment 1, and then subtracts these wet weights
# from scale data to give estimate of soil wet/dry mass at start of experiment.
# Finally, changes in wet soil weight are estimated from scale data and translated into 
# soil water content and then soil matric potential.


rm(list=ls())
packages <- c('lubridate','plyr','ggplot2','car','readODS')
lapply(packages, require, character.only = TRUE)
Sys.setenv(tz='GMT')



# SECTION 1: Get data from 2nd Experiment ---------------------------------

# Read in modeled aboveground plant wet weights, Experiment 2
modeled_plant_wt <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/modeled_aboveground_plant_weights.rds')

# omit Virgins, not applicable to 1st Experiment
modeled_plant_wt <- subset(modeled_plant_wt, block != 'V')

# Read in end-of-experiment 2 plant wet weights and sensor weights
plant_wt <- read_ods('/home/sean/github/2020_greenhouse/second_fall_experiment/data/end_of_experiment_data.ods',
                     col_names = T)
plant_wt$plant_id <- toupper(plant_wt$plant_id)
# add up weight of sensors (will be used below)
plant_wt$all_sensors_wt_kg <- 1e-3*rowSums(plant_wt[,c('bs_sensor1_wt_g','bs_sensor2_wt_g','teros_wt_g','watermark_wt_g')], na.rm = T)


# read in 15-minute aggregated (and flagged) balance data 
baldat <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_long_aggflag.rds')
# omit border plants
baldat <- subset(baldat, !grepl('border', baldat$plant_id)) 
# rename scale weight column for clarity
colnames(baldat)[colnames(baldat)=='mean_weight_kg'] <- 'scale_weight_kg'
colnames(baldat)[colnames(baldat)=='roundTime'] <- 'by15'


## Question: Can we tell which pots had more sensors, just on weight alone?
sub=subset(baldat, block=='W' & date <='2019-10-25')
sub %>% group_by(plant_id) %>% summarize(mean(scale_weight_kg, na.rm = T))
ggplot(sub, aes(x=by15, y=scale_weight_kg, color=plant_id))+geom_line()
# yes, W-6 heaviest
sub=subset(baldat, block=='M' & date <='2019-10-25')
sub %>% group_by(plant_id) %>% summarize(mean(scale_weight_kg, na.rm = T))
ggplot(sub, aes(x=by15, y=scale_weight_kg, color=plant_id))+geom_line()
sub=subset(baldat, block=='D' & date <='2019-10-25')
sub %>% group_by(plant_id) %>% summarize(mean(scale_weight_kg, na.rm = T))
ggplot(sub, aes(x=by15, y=scale_weight_kg, color=plant_id))+geom_line()

# Verdict: Seems like we can tell which pots have 4 sensors but not slam dunk.


### -------- SECTION 2: Calculate Soil Water Content and Matric Potential -------------


### 2.1 --- Recreate vanBavel desorption curve 
###         (for converting soil water content to water potential)


# read in desorption-curve data (reverse-engineered from vanBavel etal, 1978, fig. 1)
vb <- read.csv("/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/vanBavel/van_bavel_data_extract.csv")
names(vb) <- c('volumetric_water_content','pressure_potential_kPa')

# plot vanBavel curve (points extracted from curve in fig. 1)
plot(pressure_potential_kPa ~ volumetric_water_content, data = vb, log='y', xlim=c(0,0.8), ylim=c(1e-1, 1e4), xaxt='n')
axis(side = 1, at=seq(0,0.8,0.1)); axis(side = 2, at=10^(-1:4))

# fit a loess spline to curve
# NOTE: We aren't concerned with overfitting here; the points in the plot above are
# not actual data but are digitally extracted from the curve in Fig. 1, vanBavel
vb_loess_fit <- loess(pressure_potential_kPa~volumetric_water_content, data = vb, span=0.1); summary(vb_loess_fit)

# prediction grid
vb_pred <- data.frame(volumetric_water_content=seq(0, 0.8, 0.001))
vb_pred$pressure_potential_kPa <- predict(vb_loess_fit, newdata = vb_pred)

# overlay loess fit to original data/curve: looks perfect!
plot(pressure_potential_kPa ~ volumetric_water_content, data = vb, log='y', xlim=c(0,0.8), ylim=c(1e-1, 1e4), xaxt='n')
axis(side = 1, at=seq(0,0.8,0.1)); axis(side = 2, at=10^(-1:4))
lines(pressure_potential_kPa ~ volumetric_water_content,data = vb_pred, col='red')

plot(pressure_potential_kPa ~ volumetric_water_content, data = vb_pred, type = 'b')

# Save loess fit vb_pred
vb_pred <- vb_pred[complete.cases(vb_pred),] # remove NA data outside of range
vb_pred$pressure_potential_kPa <- -(vb_pred$pressure_potential_kPa) # reverse the sign (tension not pressure)
# write.csv(vb_pred, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/vanBavel/vanBavel_curve_data_final.csv',
#           row.names = F)


### 2.2 ---  Calculate Soil Water Content for each Treatment Period


# read in 15-minute aggregated (and flagged) balance data 
baldat <- readRDS('/home/sean/github/2020_greenhouse/first_summer_experiment/data/scale_output/scale_data_long_aggflag.rds')

# rename scale weight column for clarity
colnames(baldat)[colnames(baldat)=='mean_weight_kg'] <- 'scale_weight_kg'
colnames(baldat)[colnames(baldat)=='roundTime'] <- 'by15'

## Question: Can we tell which pots had more sensors, just on weight alone?
sub=subset(baldat, block=='W')
sub %>% group_by(plant_id) %>% summarize(mean(scale_weight_kg, na.rm = T))
ggplot(sub, aes(x=by15, y=scale_weight_kg, color=plant_id))+geom_line()
# I guess that W-2 has the additional sensors but not sure.
sub=subset(baldat, block=='M')
sub %>% group_by(plant_id) %>% summarize(mean(scale_weight_kg, na.rm = T))
ggplot(sub, aes(x=by15, y=scale_weight_kg, color=plant_id))+geom_line()
# Seems pretty likely that M-2 has the additional sensors.
sub=subset(baldat, block=='D')
sub %>% group_by(plant_id) %>% summarize(mean(scale_weight_kg, na.rm = T))
ggplot(sub, aes(x=by15, y=scale_weight_kg, color=plant_id))+geom_line()
# Maybe D-4 has additional sensors?

# VERDICT: I don't think we can really tell which pots have 4 sensors.
# Maybe Sean and Garret can figure out from their notes. 


# Calculate mean sensor total weight for pots with all 4 sensors
allSensorMeanWt <- mean(plant_wt$all_sensors_wt_kg[plant_wt$all_sensors_wt_kg != 0])

# Calculate mean sensor total weight for pots with only 1-2 sensors
# otherSensorMeanWt <- mean(plant_wt$all_sensors_wt_kg[plant_wt$all_sensors_wt_kg < 0.3 & plant_wt$all_sensors_wt_kg > 0])


# rename scale data
wc <- baldat

# For now, assume all pots had only 1-2 sensors 

# calculate mass of wet soil only: subtract out pot weight and sensor weights
# pot weight guestimated to be 0.15 kg
# For now, assume all pots had only 1-2 sensors (use 'otherSensorMeanWt')
wc$wet_soil_mass_kg <- wc$scale_weight_kg - allSensorMeanWt - 0.15


# NOW, need to change dates for modeled plant weights (Experiment 2) to match dates in Experiment 1
summary(modeled_plant_wt)
modeled_plant_wt$date <- as.Date('2019-07-11') + modeled_plant_wt$date_num - 1
modeled_plant_wt <- subset(modeled_plant_wt, date <= as.Date('2019-09-03'))
summary(modeled_plant_wt)


# now merge modeled plant weights 
wc2 <- merge(wc, modeled_plant_wt, all.x = T)

# convert modeled plant weights from g to kg
wc2$modeled_weight_logistic_kg <- wc2$modeled_weight_logistic_g/1000
wc2 <- subset(wc2, select = -c(modeled_weight_linear_g, modeled_weight_logistic_g, date_num))

# Show the logistic modeled wet weights we will apply to experiment 1
# (they look linear over the last week of the experiment)
ggplot(wc2, aes(x=date, y=modeled_weight_logistic_kg, color=block)) + geom_line()

# subtract out modeled above-ground plant biomass.
# Use logistic model, since no Virign plants involved.
wc2$wet_soil_mass_kg <- wc2$wet_soil_mass_kg - wc2$modeled_weight_logistic_kg
ggplot(wc2, aes(x=by15, y=wet_soil_mass_kg, color=block)) + geom_point()


# Investigate what time is best to use as starting/reference for fully saturated soil....
sub = subset(wc2, date >= as.Date('2019-08-24'))
ggplot(sub, aes(x=by15, y=scale_weight_kg, color=plant_id)) + geom_line()
# looks like we only have data for all scales on starting on the night that treatments began...
wcStart <- subset(wc2, by15 == as.POSIXct('2019-10-23 02:00', tz = "GMT"))
# can we see any drainage in drought treatment (suggesting it was fully saturated or close to it?)
sub = subset(wc2, date >= '2019-08-24' & date <= '2019-08-25' & block == 'W')
ggplot(sub, aes(x=by15, y=scale_weight_kg, color=plant_id)) + geom_line()


# hmm hard to tell for sure...let's look at raw data instead.
# not convinced that even all of the wet treatment plants were fully saturated, but don't really have any
# choice but to use 8/24 as reference (saturated) pot weights....
rawScale <- readRDS('/home/sean/github/2020_greenhouse/first_summer_experiment/data/scale_output/scale_data_compiled_raw_long.rds')
sub=subset(rawScale, timestamp >= as.POSIXct('2019-08-24 17:00', tz='GMT')  &
             timestamp <= as.POSIXct('2019-08-24 18:30', tz='GMT') & block=='W')
ggplot(sub, aes(x=timestamp, y=weight, color=plant_id)) + geom_line()


# Now, subset out the starting pot weights to use as a reference in calculating dry mass of soil
wcStart <- subset(wc2, by15 == as.POSIXct('2019-08-24 20:00', tz = "GMT"))

# back-calculate mass of dry soil, assuming water content of 0.45 (vanBavel) and bulk density of 0.65 (from Garrett; air dry)
saturated_prop_water <- 0.45
bulk_density <- 0.65
wcStart$dry_soil_mass_kg <- wcStart$wet_soil_mass_kg / (1 + saturated_prop_water / bulk_density)
wcStart$dry_soil_vol_L <- wcStart$dry_soil_mass_kg / bulk_density


# now calculate water content for each plant at 15-min steps
wcSplit <- split(wc2, wc2$plant_id)
wc2 <- do.call(rbind, lapply(wcSplit, function(x) {
  pl <- unique(x$plant_id)
  drywt <- wcStart$dry_soil_mass_kg[wcStart$plant_id == pl]
  dryvol <- wcStart$dry_soil_vol_L[wcStart$plant_id == pl]
  x$volumetric_water_content <- (x$wet_soil_mass_kg - drywt) / dryvol
  return(x)
}))

# use the loess model (see above; vanBavel) to convert soil water content to water potential
# note: we have to change the sign to get tension instead of postive pressure.
wc2$pressure_potential_kPa <- -(predict(vb_loess_fit, newdata = wc2))
summary(wc2)

# save the predcited soil water potentials: we will add to these below...
wcAll <- wc2

# plots of soil water potential
png(paste0('/home/sean/github/2020_greenhouse/first_summer_experiment/figures/clay_figures/mass_balance/modeled_soil_water_potential/',
           'BlockD_FullDrought'), width=1500, height=900)
ggplot(subset(wc2, block == 'D')) + 
  geom_point(aes(x=by15, y=pressure_potential_kPa, color = plant_id)) +
  scale_x_datetime(date_breaks = '2 days', date_labels = '%m-%d') +
  ggtitle("Modeled soil water potential, full drought block")
dev.off()

png(paste0('/home/sean/github/2020_greenhouse/first_summer_experiment/figures/clay_figures/mass_balance/modeled_soil_water_potential/',
           'BlockW_WellWatered'), width=1500, height=900)
ggplot(subset(wc2, block == 'W')) + 
  geom_point(aes(x=by15, y=pressure_potential_kPa, color = plant_id)) +
  scale_x_datetime(date_breaks = '2 days', date_labels = '%m-%d') +
  ggtitle("Modeled soil water potential, well-watered block")
dev.off()

png(paste0('/home/sean/github/2020_greenhouse/first_summer_experiment/figures/clay_figures/mass_balance/modeled_soil_water_potential/',
           'BlockM_ModerateDrought'), width=1500, height=900)
ggplot(subset(wc2, block == 'M')) + 
  geom_point(aes(x=by15, y=pressure_potential_kPa, color = plant_id)) +
  scale_x_datetime(date_breaks = '2 days', date_labels = '%m-%d') +
  ggtitle("Modeled soil water potential, moderate drought block")
dev.off()


# Save the predicted soil water potential data
saveRDS(wc2, '/home/sean/github/2020_greenhouse/first_summer_experiment/scripts/clay_R_scripts/analysis/mass_balance/modeled_psi_soil.rds')
