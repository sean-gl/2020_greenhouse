
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
Sys.getenv('TZ') # make sure it got set



# assign treatment dates... don't forget the "tz='GMT' !!
exp1_start <- as.POSIXct('2019-10-24 12:00:00', tz='GMT')
exp1_end   <- as.POSIXct('2019-11-04 16:00:00', tz='GMT') # clay: changed end time from 09:00, to get more pressure bomb data

exp2_start <- as.POSIXct('2019-11-04 17:00:00', tz='GMT')
exp2_end   <- as.POSIXct('2019-11-27 16:00:00', tz='GMT') # clay: changed end time from 09:00, to get more pressure bomb data

exp3_start <- as.POSIXct('2019-11-27 17:00:00', tz='GMT')
exp3_end   <- as.POSIXct('2019-12-12 16:00:00', tz='GMT') # clay: changed end time from 09:00, to get more pressure bomb data




### -------------------------------------------
###  --- First, merge data NOT specific to a block/treatment  ----
### -------------------------------------------


  
####
# 1. Sean's arduino data (rh, par, pyranometer)
###

rh <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/RH_temp_PAR_logger_data/rh_15.csv')
rh$by15 <- as.POSIXct(rh$by15, tz='GMT')

# remove data before 10-24 (just a few rows), these are crap
rh <- subset(rh, date(by15) >= '2019-10-24')
# remove soil temp columsn, these are imported below
rh <- rh %>% select(-contains('soil_t'))


####
# 2. Line Quantum (PAR) data
###

lq <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/line_PAR_sensors/line_PAR_15.csv')
lq$by15 <- as.POSIXct(lq$by15, tz = 'GMT')

allDataExp <- merge(rh, lq, by='by15', all=TRUE)


### ---- Special Section: Add LED PAR contribution ----


# add date & minutes columns
allDataExp$date <- date(allDataExp$by15)
allDataExp$minutes <- hour(allDataExp$by15)*60 + minute(allDataExp$by15)

# general indexes for times between 03:15 and 19:15 (when LEDs were on)

# NOTE: Apparently the greenhouse system was set to daylight time, so on 11/3 the LED
# started coming on an hour later (relative to all other data loggers)
ind1 <- with(allDataExp, by15 < as.POSIXct('2019-11-03 02:00', tz="GMT") & 
              (minutes >= 2*60 + 15) & (minutes <= 18*60 + 15)) # TRUE = LEDs on.
ind2 <- with(allDataExp, by15 >= as.POSIXct('2019-11-03 02:00', tz="GMT") & 
               (minutes >= 3*60 + 15) & (minutes <= 19*60 + 15)) # TRUE = LEDs on.

# special indices for times when LEDs manually turned off (see greehouse logbook)
# TRUE = LEDS ON
ind3 <- !(with(allDataExp, date == '2019-12-05' & (minutes >= 10*60 + 40) & (minutes <= 17*60 + 30)))
ind4 <- !(with(allDataExp, date == '2019-12-10' & (minutes >= 16*60 + 50)))
ind5 <- !(with(allDataExp, date == '2019-12-11' & (minutes <= 7*60 + 15)))

# combine all indices 
ind <- (ind1 | ind2) & ind3 & ind4 & ind5
# ind <- ind & ind1 & ind2 & ind3

# add LED contribution to indexed timestamps
allDataExp$line_PAR_east_umol_m2_s_plusLED[!ind] <- allDataExp$line_PAR_east_umol_m2_s[!ind]
allDataExp$line_PAR_east_umol_m2_s_plusLED[ind] <- allDataExp$line_PAR_east_umol_m2_s[ind] + 225
allDataExp$line_PAR_west_umol_m2_s_plusLED[!ind] <- allDataExp$line_PAR_west_umol_m2_s[!ind]
allDataExp$line_PAR_west_umol_m2_s_plusLED[ind] <- allDataExp$line_PAR_west_umol_m2_s[ind] + 225
allDataExp$par1_n_plusLED[!ind] <- allDataExp$par1_n[!ind]
allDataExp$par1_n_plusLED[ind] <- allDataExp$par1_n[ind] + 225
allDataExp$par2_n_plusLED[!ind] <- allDataExp$par2_n[!ind]
allDataExp$par2_n_plusLED[ind] <- allDataExp$par2_n[ind] + 225

# Check: plot lq before (black) and after (red) adding LED
x=subset(allDataExp, date >= '2019-12-04' & date <= '2019-12-12')
plot(x$by15, x$line_PAR_east_umol_m2_s_plusLED, type = 'l', col='red'); lines(x$by15, x$line_PAR_east_umol_m2_s)
plot(x$by15, x$par1_n_plusLED, type = 'l', col='red'); lines(x$by15, x$par1_n)

# ---- Looks good, now replace the original data columns with added PAR
allDataExp$line_PAR_east_umol_m2_s <- allDataExp$line_PAR_east_umol_m2_s_plusLED
allDataExp$line_PAR_east_umol_m2_s_plusLED <- NULL 
allDataExp$line_PAR_west_umol_m2_s <- allDataExp$line_PAR_west_umol_m2_s_plusLED
allDataExp$line_PAR_west_umol_m2_s_plusLED <- NULL
allDataExp$par1_n <- allDataExp$par1_n_plusLED
allDataExp$par1_n_plusLED <- NULL 
allDataExp$par2_s <- allDataExp$par2_s_plusLED
allDataExp$par1_n_plusLED <- NULL 

# now, omit date & minutes columns (they make merging messy)
allDataExp <- select(allDataExp, -c(date, minutes))

# find rows with no data
ind <- apply(select(allDataExp, -by15), 1, function(x) all(is.na(x)))
which(ind) # all rows have some data


# ----- Add VPD_air ----

# require(plantecophys)

# Examine RH data
# colors <- c('am2320'='black', 'sht_hi'='red', 'sht_lo'='blue')
# # hi v. low
# ggplot(allDataExp, aes(x=by15)) +
#   geom_line(aes(y=sht1_high_rh, color='sht_hi')) +
#   geom_line(aes(y=sht2_low_rh, color='sht_lo'))
# 
# # sht hi vs. am2320 hi
# ggplot(allDataExp, aes(x=by15)) +
#   geom_line(aes(y=am2320_high_rh, color='am2320')) +
#   geom_line(aes(y=sht1_high_rh, color='sht_hi')) 
#  
# cor(allDataExp$sht1_high_rh, allDataExp$am2320_high_rh, use='complete.obs')
# cor(allDataExp$sht1_high_rh, allDataExp$sht2_low_rh, use='complete.obs')

# Using Sean's formula

# VPD_air, high (note: using am2320 RH/temp results in very similar VPD)
allDataExp$VPD_air_high <- (1 - (allDataExp$sht1_high_rh / 100)) * 0.61121 * exp((17.502 * allDataExp$sht1_high_temp) / (240.97 + allDataExp$sht1_high_temp)) 

# VPD_air, low
allDataExp$VPD_air_low <- (1 - (allDataExp$sht2_low_rh / 100)) * 0.61121 * exp((17.502 * allDataExp$sht2_low_temp) / (240.97 + allDataExp$sht2_low_temp)) 

# nearly same result using this function, assuming atm pressure = 101 kPa
# allDataExp$VPD_air_high <- plantecophys::RHtoVPD(RH = allDataExp$sht1_high_rh, 
#                                                TdegC = allDataExp$sht1_high_temp, 
#                                                Pa = 101)

summary(allDataExp$VPD_air_high)
summary(allDataExp$VPD_air_low)




### -------------------------------------------
###  --- Second, merge block-level data  ----
### -------------------------------------------


####
# 3. Soil temperature 
###

soil_temp <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/RH_temp_PAR_logger_data/soil_temp_15.csv')
soil_temp$by15 <- as.POSIXct(soil_temp$by15, tz='GMT')

# change order of columns
soil_temp <- soil_temp[,c('by15','treatment','block','soil_temp_C')]


####
# 4. Wind data
###

wind <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/wind_sensor_data/wind_15.csv')
wind$by15 <- as.POSIXct(wind$by15, tz='GMT')

# convert to long format
windWide <- tidyr::spread(wind, 'position', 'wind_speed_m_s')
windWide <- tidyr::pivot_wider(wind, names_from = 'position', values_from = 'wind_speed_m_s', names_prefix = 'windspeed_')
head(windWide)


### MERGE data
allDataPlot <- merge(windWide, soil_temp, by=c('by15','treatment','block'), all = TRUE)

# find rows with no data
ind <- apply(select(allDataPlot, -c(by15, treatment, block)), 1, function(x) all(is.na(x)))
which(ind) # 3 rows w/no data

# omit rows with all NA
allDataPlot <- allDataPlot[!ind,]


####
# 5. Pressure bomb (block means)
###

pb <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/pressure_bomb/pressure_bomb_15.csv')
pb$by15 <- as.POSIXct(pb$by15, tz='GMT')

# omit bad observation & missing observation
pb <- pb[pb$data_ok=='yes' & !is.na(pb$psi_MPa), ]

## Make some edits to data this day...
pb$by15[date(pb$by15)=='2019-11-15' & pb$treatment=='moderate_drought'] <- '2019-11-15 13:45:00 GMT'

# add block column
pb$block <- toupper(substr(pb$plant_id,1,1))

# get means by timestamp and treatment/block
pbMeans <- ddply(pb, .(by15, treatment, block), function(x) {
  setNames(mean(x$psi_MPa), 'mean_psi_leaf_MPa')
})


### MERGE data
allDataPlot2 <- merge(pbMeans, allDataPlot, by=c('by15','treatment','block'), all = TRUE)


# find rows with no data
ind <- apply(select(allDataPlot2, -c(by15, treatment, block)), 1, function(x) all(is.na(x)))
which(ind) # no rows w/no data

# omit rows with all NA
# allDataPlot <- allDataPlot[!ind,]


### --- Now, merge experiment-level to plot/treatment-level data
allData <- merge(allDataExp, allDataPlot2, by = 'by15', all = TRUE)

# find rows with no data
ind <- apply(select(allData, -c(by15, treatment, block)), 1, function(x) all(is.na(x)))
which(ind) # no rows w/no data


### ----- At this point, let's save the "treatment-level only" dataset
saveRDS(allData, '/home/sean/github/2020_greenhouse/second_fall_experiment/data/combined_data/combdat_treatment_level_only.rds')


# read back in
# allData <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/data/combined_data/combdat_treatment_level_only.rds')


### -------------------------------------------
###  --- Lastly, merge plant-level data ----
### -------------------------------------------



####
# 6. Leaf temperature (thermistors)
###

lt <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/data/leaf_thermistor_data/leaf_thermistor_data_15min_agg_flagged.rds')

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
lt$position[lt$position=='middle'] <- 'middle'
lt$position[lt$position=='upper'] <- 'top'

#
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
lt_wide$leaftemp_mean <- rowMeans(lt_wide[,c('leaftemp_bottom','leaftemp_middle','leaftemp_top')], na.rm = T)

# get the highest position with available data
lt_wide$leaftemp_highest_avail <- apply(lt_wide, 1, function(x) {
  ind <- which(!is.na(x[c('leaftemp_bottom','leaftemp_middle','leaftemp_top')]))
  if(length(ind) > 0) {
    out <- as.numeric(x[c('leaftemp_bottom','leaftemp_middle','leaftemp_top')][max(ind)])
  } else out <- NA
  return(out)
})


# --- Assign treatments
lt_wide$treatment <- NA

# Block D
i <- with(lt_wide, block=='D' & by15 >= exp1_start & by15 <= exp1_end)
lt_wide$treatment[i] <- 'full_drought'
i <- with(lt_wide, block=='D' & by15 >= exp2_start & by15 <= exp2_end)
lt_wide$treatment[i] <- 'well_watered'
i <- with(lt_wide, block=='D' & by15 >= exp3_start & by15 <= exp3_end)
lt_wide$treatment[i] <- 'full_drought'
table(lt_wide$treatment[lt_wide$block=='D'], useNA = 'a')

# Block M
i <- with(lt_wide, block=='M' & by15 >= exp1_start & by15 <= exp1_end)
lt_wide$treatment[i] <- 'moderate_drought'
i <- with(lt_wide, block=='M' & by15 >= exp2_start & by15 <= exp2_end)
lt_wide$treatment[i] <- 'moderate_drought'
i <- with(lt_wide, block=='M' & by15 >= exp3_start & by15 <= exp3_end)
lt_wide$treatment[i] <- 'well_watered'
table(lt_wide$treatment[lt_wide$block=='M'], useNA = 'a')

# Block W
i <- with(lt_wide, block=='W' & by15 >= exp1_start & by15 <= exp1_end)
lt_wide$treatment[i] <- 'well_watered'
i <- with(lt_wide, block=='W' & by15 >= exp2_start & by15 <= exp2_end)
lt_wide$treatment[i] <- 'full_drought'
i <- with(lt_wide, block=='W' & by15 >= exp3_start & by15 <= exp3_end)
lt_wide$treatment[i] <- 'virgin_drought'
table(lt_wide$treatment[lt_wide$block=='W'], useNA = 'a')

# omit rows without a treatment assigned
nrow(lt_wide)
lt_wide <- subset(lt_wide, !is.na(treatment)); nrow(lt_wide)



####
# 7. Transpiration data (calculated from scale weights)
###

# read data
transp <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_transpiration/transpiration_by_plant.rds')

# remove border plants
transp <- subset(transp, !grepl('border_plant', plant_id))

# check that block names match plant id
bn <- substr(transp$plant_id, 1, 1)
all(bn == transp$block)

# re-do assignment of treatments to match those above
transp$treatment <- NA

# Block D
i <- with(transp, block=='D' & by15 >= exp1_start & by15 <= exp1_end)
transp$treatment[i] <- 'full_drought'
i <- with(transp, block=='D' & by15 >= exp2_start & by15 <= exp2_end)
transp$treatment[i] <- 'well_watered'
i <- with(transp, block=='D' & by15 >= exp3_start & by15 <= exp3_end)
transp$treatment[i] <- 'full_drought'
table(transp$treatment[transp$block=='D'], useNA = 'a')

# Block M
i <- with(transp, block=='M' & by15 >= exp1_start & by15 <= exp1_end)
transp$treatment[i] <- 'moderate_drought'
i <- with(transp, block=='M' & by15 >= exp2_start & by15 <= exp2_end)
transp$treatment[i] <- 'moderate_drought'
i <- with(transp, block=='M' & by15 >= exp3_start & by15 <= exp3_end)
transp$treatment[i] <- 'well_watered'
table(transp$treatment[transp$block=='M'], useNA = 'a')

# Block W
i <- with(transp, block=='W' & by15 >= exp1_start & by15 <= exp1_end)
transp$treatment[i] <- 'well_watered'
i <- with(transp, block=='W' & by15 >= exp2_start & by15 <= exp2_end)
transp$treatment[i] <- 'full_drought'
i <- with(transp, block=='W' & by15 >= exp3_start & by15 <= exp3_end)
transp$treatment[i] <- 'virgin_drought'
table(transp$treatment[transp$block=='W'], useNA = 'a')

# NA are all on dates treatments switched or before start of experiment
table(transp$date[is.na(transp$treatment)]) 
with(transp[is.na(transp$treatment),], table(date, plant_id))

# remove rows without treatment
transp <- transp[!is.na(transp$treatment), ]

# reanme flag column
colnames(transp)[colnames(transp)=='flag'] <- 'scale_flag'
colnames(transp)[colnames(transp)=='mean_weight_kg'] <- 'scale_weight_kg'

# select columns to keep in merge
head(transp)
transp <- subset(transp, select = c(by15, plant_id, treatment, block, scale_flag,
                                    T_mg_s, T_mg_m2_s, scale_weight_kg, mean_plant_leaf_area_m2))

# check
# sub=subset(transp, block=='D')
# ggplot(sub, aes(x=by15, y=T_mg_m2_s, color=plant_id)) + geom_line()


### MERGE
allDataPlant <- merge(transp, lt_wide, by=c('by15','block','treatment','plant_id'), all = TRUE)


nrow(transp); nrow(lt_wide); nrow(allDataPlant)

# find rows with no data
ind <- apply(select(allDataPlant, -c(by15, treatment, block, plant_id)), 1, function(x) all(is.na(x)))
which(ind) # no rows w/no data



####
# 8. Soil water potential (calculated from scale weights)
###

soilwp <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/modeled_psi_soil.rds')
head(soilwp)


# --- Assign treatments
soilwp$treatment <- NA

# Block D
i <- with(soilwp, block=='D' & by15 >= exp1_start & by15 <= exp1_end)
soilwp$treatment[i] <- 'full_drought'
i <- with(soilwp, block=='D' & by15 >= exp2_start & by15 <= exp2_end)
soilwp$treatment[i] <- 'well_watered'
i <- with(soilwp, block=='D' & by15 >= exp3_start & by15 <= exp3_end)
soilwp$treatment[i] <- 'full_drought'
table(soilwp$treatment[soilwp$block=='D'], useNA = 'a')

# Block M
i <- with(soilwp, block=='M' & by15 >= exp1_start & by15 <= exp1_end)
soilwp$treatment[i] <- 'moderate_drought'
i <- with(soilwp, block=='M' & by15 >= exp2_start & by15 <= exp2_end)
soilwp$treatment[i] <- 'moderate_drought'
i <- with(soilwp, block=='M' & by15 >= exp3_start & by15 <= exp3_end)
soilwp$treatment[i] <- 'well_watered'
table(soilwp$treatment[soilwp$block=='M'], useNA = 'a')

# Block W
i <- with(soilwp, block=='W' & by15 >= exp1_start & by15 <= exp1_end)
soilwp$treatment[i] <- 'well_watered'
i <- with(soilwp, block=='W' & by15 >= exp2_start & by15 <= exp2_end)
soilwp$treatment[i] <- 'full_drought'
i <- with(soilwp, block=='W' & by15 >= exp3_start & by15 <= exp3_end)
soilwp$treatment[i] <- 'virgin_drought'
table(soilwp$treatment[soilwp$block=='W'], useNA = 'a')

# omit rows without a treatment assigned
nrow(soilwp)
soilwp <- subset(soilwp, !is.na(treatment)); nrow(soilwp)

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

# NOTE: after merge, some rows missing treatment/block; remove these, they're outside the treatment windows
ind <- is.na(allData2$treatment) | is.na(allData2$block)
# View(allData2[ind,])
# drop rows
allData2 <- allData2[!ind, ]




### ----- Final Section: Derived/Modelled Variables added here -----------



# --- Now calculate VPD_leaf, using highest available leaf temp, and "low" air temp/RH (75% canopy ht.)

# First calculate the satVP_of_leaf:
satVP_leaf <- 1e-3*(exp(77.345+0.0057*(allData2$leaftemp_highest_avail+273.15)-7235/(allData2$leaftemp_highest_avail+273.15)))/(allData2$leaftemp_highest_avail+273.15)^8.2
summary(satVP_leaf)

# Then, calculate the satVP_of_atmosphere:
satVP_atm <- 1e-3*(exp(77.345+0.0057*(allData2$sht2_low_temp+273.15)-7235/(allData2$sht2_low_temp+273.15)))/(allData2$sht2_low_temp+273.15)^8.2
summary(satVP_atm)


# Then, use satVP_of_atmosphere and RH data (from sensors) to calculate actual VP of atmosphere:
VP_atm <- satVP_atm * allData2$sht2_low_rh/100  #[ note this is to express RH as a fraction ]
summary(VP_atm)

# then, finally, calculate VPD_leaf:
allData2$VPD_leaf <- satVP_leaf - VP_atm
summary(allData2$VPD_leaf)



### --- Add psi_leaf linear model predictions ----


# add irrigation amount (ml); its used in the model
allData2$irrig <- NA
allData2$irrig[date(allData2$by15) < "2019-11-05" & allData2$treatment == 'well_watered'] <- 750
allData2$irrig[date(allData2$by15) >= "2019-11-05" & allData2$treatment == 'well_watered'] <- 1000
allData2$irrig[allData2$treatment == 'moderate_drought'] <- 375
allData2$irrig[allData2$treatment %in% c('full_drought','virgin_drought')] <- 150
table(allData2$irrig, useNA = 'a')

# add mean PAR column, also used in model
allData2$line_PAR_mean_umol_m2_s <- rowMeans(allData2[,c('line_PAR_west_umol_m2_s','line_PAR_east_umol_m2_s')], na.rm = TRUE)

# read in linear model to predict continuous psi_leaf
psi_leaf_model <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_psi_leaf/psi_leaf_final_model.rds')

# add 'minutes' column (used in model)
allData2$minutes <- hour(allData2$by15)*60 + minute(allData2$by15)

# predict psi_leaf on continuous data
allData2$mean_psi_leaf_MPa_modeled <- predict(psi_leaf_model, newdata = allData2)

# omit minutes column
allData2$minutes <- NULL

summary(allData2$mean_psi_leaf_MPa_modeled)


# find rows with no data
ind <- apply(select(allData2, -c(by15, treatment, block, plant_id)), 1, function(x) all(is.na(x)))
which(ind) # no rows w/no data



### ---- Finallly, save the complete (plant-level) dataset
saveRDS(allData2, '/home/sean/github/2020_greenhouse/second_fall_experiment/data/combined_data/combdat_plant_level.rds')
