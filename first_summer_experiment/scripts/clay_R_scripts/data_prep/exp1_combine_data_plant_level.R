
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




# --- Verdict: Both PAR and PYR data have lots of negative values, but only at baseline (no light)
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
sub = lt[lt$block=='D',]
ggplot(sub, aes(x=by15, y=mean_leaftemp_C, color=position)) + geom_point() + geom_line() + facet_grid(~plant_id)
sub = lt[lt$block=='W',]
ggplot(sub, aes(x=by15, y=mean_leaftemp_C, color=position)) + geom_point() + geom_line() + facet_grid(~plant_id)
sub = lt[lt$block=='M',]
ggplot(sub, aes(x=by15, y=mean_leaftemp_C, color=position)) + geom_point() + geom_line() + facet_grid(~plant_id)



####
# 5. Transpiration data (calculated from scale weights)
###

# read data
transp <- readRDS('/home/sean/github/2020_greenhouse/first_summer_experiment/scripts/clay_R_scripts/analysis/model_transpiration/transpiration_by_plant.rds')
