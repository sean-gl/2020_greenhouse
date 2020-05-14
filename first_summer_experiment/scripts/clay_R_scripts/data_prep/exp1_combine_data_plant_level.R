
# clear workspace
rm(list=ls())


require(ggplot2)
require(plyr)
require(lubridate)
require(readODS)
require(tidyr)
require(dplyr)

### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')



# assign treatment start date/time. Irrigation started at 18:00, so I set for 1 hour after that. 
exp_start <- as.POSIXct('2019-08-24 19:00:00', tz='GMT')


### -------------------------------------------
###  --- First, merge data NOT specific to a block/treatment  ----
### -------------------------------------------



####
# 1. Sean's arduino data (rh, par, pyranometer)
###

rh <- read.csv('/home/sean/github/2020_greenhouse/first_summer_experiment/data/RH_temp_PAR_logger_data/rh_15.csv')
rh$by15 <- as.POSIXct(rh$by15, tz='GMT')

# remove data before start of treatments
rh <- subset(rh, by15 >= exp_start)

# remove soil temp columsn, these are imported below
rh <- rh %>% select(-contains('soil_t'))

# ----- Add VPD_air ----
# Using Sean's formula
# VPD_air, low
rh$VPD_air_low <- (1 - (rh$sht2_low_rh / 100)) * 0.61121 * exp((17.502 * rh$sht2_low_temp) / (240.97 + rh$sht2_low_temp)) 
summary(rh$VPD_air_low)



### -------------------------------------------
###  --- Second, merge block-level data  ----
### -------------------------------------------


####
# 2. Soil temperature 
###

soil_temp <- read.csv('/home/sean/github/2020_greenhouse/first_summer_experiment/data/RH_temp_PAR_logger_data/soil_temp_15.csv')
soil_temp$by15 <- as.POSIXct(soil_temp$by15, tz='GMT')



####
# 3. Wind data
###

wind <- read.csv('/home/sean/github/2020_greenhouse/first_summer_experiment/data/wind_sensor_data/wind_15.csv')
wind$by15 <- as.POSIXct(wind$by15, tz='GMT')

# convert to long format
windWide <- tidyr::pivot_wider(wind, names_from = 'position', values_from = 'wind_speed_m_s')
names(windWide)[names(windWide)=='bottom'] <- 'windspeed_bottom_m_s' 
names(windWide)[names(windWide)=='middle'] <- 'windspeed_middle_m_s' 
names(windWide)[names(windWide)=='top'] <- 'windspeed_top_m_s' 
head(windWide)


### MERGE data
allDataBlock <- merge(windWide, soil_temp, by=c('by15','treatment','block'), all = TRUE)

# find rows with no data
ind <- apply(select(allDataBlock, -c(by15, treatment, block)), 1, function(x) all(is.na(x)))
which(ind) # 0 rows w/no data


### --- Now, merge experiment-level to plot/treatment-level data
allData <- merge(allDataBlock, rh, by = 'by15', all = TRUE)

# find rows with no data
ind <- apply(select(allData, -c(by15, treatment, block)), 1, function(x) all(is.na(x)))
which(ind) # no rows w/no data


### ----- At this point, let's save the "treatment-level only" dataset
saveRDS(allData, '/home/sean/github/2020_greenhouse/first_summer_experiment/data/combined_data/combdat_treatment_level_only.rds')


# read back in
# allData <- readRDS('/home/sean/github/2020_greenhouse/first_summer_experiment/data/combined_data/combdat_treatment_level_only.rds')



### -------------------------------------------
###  --- Lastly, merge plant-level data ----
### -------------------------------------------



####
# 4. Leaf temperature (thermistors)
###

lt <- readRDS('/home/sean/github/2020_greenhouse/first_summer_experiment/data/leaf_thermistor_data/leaf_thermistor_data_15min_agg_flagged.rds')


# omit data prior to start of treatments
lt <- lt[lt$by15 >= exp_start, ]

# check that block names match plant id
bn <- substr(lt$plant_id, 1, 1)
all(bn == lt$block)

# also remove thermistor number, not useful
lt$thermistor <- NULL
# remove position column, not useful
lt$position <- NULL
colnames(lt)[colnames(lt)=='canopy_position'] <- 'position'
table(lt$position, useNA = 'a')

# change position categoies to match wind data
lt$position[lt$position=='lower'] <- 'bottom'
lt$position[lt$position=='upper'] <- 'top'


# filter data by flag 
# (omit data that is probably or definitely bad)
lt <- subset(lt, flag <= 2 & temperature_flag == 'none')

# remove flag columns
lt$flag <- lt$temperature_flag <- NULL

# convert to wide format
lt_wide <- pivot_wider(lt, names_from = 'position', values_from = 'mean_leaftemp_C',
                       names_prefix = 'leaftemp_', 
                       values_fn = list(mean_leaftemp_C = function(x) mean(x, na.rm = T)))

# calculate mean leaftemperature at all canopy positions with data
lt_wide$leaftemp_mean <- rowMeans(lt_wide[,c('leaftemp_bottom','leaftemp_top')], na.rm = T)

# get the highest position with available data
lt_wide$leaftemp_highest_avail <- apply(lt_wide, 1, function(x) {
  ind <- which(!is.na(x[c('leaftemp_bottom','leaftemp_middle','leaftemp_top')]))
  if(length(ind) > 0) {
    out <- as.numeric(x[c('leaftemp_bottom','leaftemp_middle','leaftemp_top')][max(ind)])
  } else out <- NA
  return(out)
})

summary(lt_wide)



# --- Assign treatments
lt_wide$treatment <- NA
lt_wide$treatment[lt_wide$block=='W'] <- 'well_watered'
lt_wide$treatment[lt_wide$block=='M'] <- 'moderate_drought'
lt_wide$treatment[lt_wide$block=='D'] <- 'full_drought'
table(lt_wide$treatment, useNA = 'a')


# Plot raw data....don't see any obvious outliers. 
# sub = lt[lt$block=='D',]
# ggplot(sub, aes(x=by15, y=mean_leaftemp_C, color=position)) + geom_point() + geom_line() + facet_grid(~plant_id)
# sub = lt[lt$block=='W',]
# ggplot(sub, aes(x=by15, y=mean_leaftemp_C, color=position)) + geom_point() + geom_line() + facet_grid(~plant_id)
# sub = lt[lt$block=='M',]
# ggplot(sub, aes(x=by15, y=mean_leaftemp_C, color=position)) + geom_point() + geom_line() + facet_grid(~plant_id)



####
# 5. Transpiration data (calculated from scale weights)
###

# read data
transp <- readRDS('/home/sean/github/2020_greenhouse/first_summer_experiment/scripts/clay_R_scripts/analysis/model_transpiration/transpiration_by_plant.rds')
colnames(transp)[colnames(transp)=='mean_weight_kg'] <- 'scale_weight_kg'

# select columns to keep in merge
head(transp)
transp <- subset(transp, select = c(by15, plant_id, treatment, block, scale_flag,
                                    T_mg_s, T_mg_m2_s, scale_weight_kg, mean_plant_leaf_area_m2))

### MERGE
allDataPlant <- merge(transp, lt_wide, by=c('by15','block','treatment','plant_id'), all = TRUE)


nrow(transp); nrow(lt_wide); nrow(allDataPlant)

# find rows with no data
ind <- apply(select(allDataPlant, -c(by15, treatment, block, plant_id)), 1, function(x) all(is.na(x)))
which(ind) # no rows w/no data


####
# 6. Soil water potential (calculated from scale weights)
###

soilwp <- readRDS('/home/sean/github/2020_greenhouse/first_summer_experiment/scripts/clay_R_scripts/analysis/mass_balance/modeled_psi_soil.rds')
head(soilwp)

# omit data prior to start of treatments
soilwp <- soilwp[soilwp$by15 >= exp_start, ]

# rename column
soilwp <- rename(soilwp, soil_water_potential_kPa = pressure_potential_kPa)

# remove unneeded columns
soilwp <- select(soilwp, c(by15, block, treatment, plant_id, soil_water_potential_kPa))

### MERGE
allDataPlant2 <- merge(soilwp, allDataPlant, by=c('by15','block','treatment','plant_id'), all = TRUE)

# find rows with no data
ind <- apply(select(allDataPlant2, -c(by15, treatment, block)), 1, function(x) all(is.na(x)))
which(ind) # no rows w/no data



### ------- Finally, merge together Plant-level data with Experiment + Block-level data


allData2 <- merge(allData, allDataPlant2, by=c('by15','block','treatment'), all = TRUE)

# find rows with no data
ind <- apply(select(allData2, -c(by15, treatment, block, plant_id)), 1, function(x) all(is.na(x)))
which(ind) # no rows w/no data




### ----- Final Section: Derived/Modelled Variables added here -----------



# --- Now calculate VPD_leaf, using highest available leaf temp, and "low" air temp/RH (75% canopy ht.)

# First calculate the satVP_of_leaf:
# use highest available leaf thermistor, with upper-most sensor corresponding to ~75% canopy height
satVP_leaf <- 1e-3*(exp(77.345+0.0057*(allData2$leaftemp_highest_avail+273.15)-7235/(allData2$leaftemp_highest_avail+273.15)))/(allData2$leaftemp_highest_avail+273.15)^8.2
summary(satVP_leaf)

# Then, calculate the satVP_of_atmosphere:
# use "low" air temperature, also at 75% canopy height
satVP_atm <- 1e-3*(exp(77.345+0.0057*(allData2$sht2_low_temp+273.15)-7235/(allData2$sht2_low_temp+273.15)))/(allData2$sht2_low_temp+273.15)^8.2
summary(satVP_atm)


# Then, use satVP_of_atmosphere and RH data (from sensors) to calculate actual VP of atmosphere:
VP_atm <- satVP_atm * allData2$sht2_low_rh/100 
summary(VP_atm)

# then, finally, calculate VPD_leaf:
allData2$VPD_leaf <- satVP_leaf - VP_atm
summary(allData2$VPD_leaf)



### --- Add psi_leaf linear model predictions ----


# add 'minutes' column (used in model)
allData2$minutes <- hour(allData2$by15)*60 + minute(allData2$by15)


# --- First, need to add irrig (L) given the night before, this is also used in model

# read in raw scale data
sdat <- readRDS('/home/sean/github/2020_greenhouse/first_summer_experiment/data/scale_output/scale_data_compiled_raw_long.rds')
sdat <- subset(sdat, date >= '2019-08-23')

# testing
x = subset(sdat, date=='2019-08-26' & plant_id=='W-1') 
  
# For each plant, calculate irrigation each day (plants watered at 18:00)
irrig <- ddply(sdat, .(date, plant_id, treatment, block), function(x) {
  print(paste(unique(x$date), unique(x$plant_id)))
  y <- subset(x, minutes >= 17*60 + 30 & minutes <= 18*60 + 30)
  # plot(y$timestamp, y$weight)
  setNames(max(y$weight, na.rm=T) - min(y$weight, na.rm = T), 'irrig')
})

# but we actually want irrigation of the previous day, so fix that...
irrig$date <- irrig$date + 1

# use block means (since that's what we did in experiment 2)
irrigMeansByDate <- irrig %>% group_by(date, block, treatment) %>% summarise(irrig = mean(irrig, na.rm=T))
ggplot(irrigMeansByDate, aes(x=date, y=irrig, color=treatment)) + geom_line()

# looks great, but exclude 8/24 and 9/4
irrig <- subset(irrig, ! date %in% c(as.Date('2019-08-24'), as.Date('2019-09-04')))

# now, calculate block means over entire experiment
irrigMeans <- irrig %>% group_by(block, treatment) %>% summarise(irrig = mean(irrig))
irrigMeans

# NOTE: Treatments were not exactly same as in Expreiment 2!
# Both drought treatments recieved more water, and well_watered recieved less water, than in Exp. 2.

# Add irrig column to full dataset, finally!
allData3 <- merge(allData2, irrigMeans, by=c('block','treatment'), all = TRUE)
table(allData3$irrig, allData3$treatment, useNA = 'a')


## --- Now, predict psi_leaf !!

# read in linear model (from 2nd Experiment) used to predict continuous psi_leaf
psi_leaf_model <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_psi_leaf/psi_leaf_linear_model.rds')

allData3$mean_psi_leaf_MPa_modeled <- predict(psi_leaf_model, newdata = allData3)
summary(allData3$psi_leaf_MPa_modeled)

# Hmmm model is predicting the same water potentials for all 3 treatments. Not sure why.
ggplot(allData3, aes(x=by15, y=mean_psi_leaf_MPa_modeled, color=treatment)) + geom_line()


# omit minutes column
allData3$minutes <- NULL


# find rows with no data
ind <- apply(select(allData3, -c(by15, treatment, block, plant_id)), 1, function(x) all(is.na(x)))
which(ind) # no rows w/no data



### ---- Finallly, save the complete (plant-level) dataset
saveRDS(allData3, '/home/sean/github/2020_greenhouse/first_summer_experiment/data/combined_data/combdat_plant_level.rds')
