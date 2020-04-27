
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

rh <- read.csv('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/RH_temp_PAR_logger_data/rh_15.csv')
rh$by15 <- as.POSIXct(rh$by15, tz='GMT')

# remove data before 10-24 (just a few rows), these are crap
rh <- subset(rh, date(by15) >= '2019-10-24')
# remove soil temp columsn, these are imported below
rh <- rh %>% select(-contains('soil_t'))


####
# 2. Line Quantum (PAR) data
###

lq <- read.csv('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/line_PAR_sensors/line_PAR_15.csv')
lq$by15 <- as.POSIXct(lq$by15, tz = 'GMT')

allData1 <- merge(rh, lq, by='by15', all=TRUE)


### -------------------------------------------
###  --- Second, merge to block-level data  ----
### -------------------------------------------


####
# 3. Soil temperature 
###

soil_temp <- read.csv('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/RH_temp_PAR_logger_data/soil_temp_15.csv')
soil_temp$by15 <- as.POSIXct(soil_temp$by15, tz='GMT')

# change order of columns
soil_temp <- soil_temp[,c('by15','treatment','block','soil_temp_C')]

# --- MERGE
allData2 <- merge(soil_temp, allData1, by='by15')


####
# 4. Wind data
###

wind <- read.csv('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/wind_sensor_data/wind_15.csv')
wind$by15 <- as.POSIXct(wind$by15, tz='GMT')

# convert to long format
windWide <- tidyr::spread(wind, 'position', 'wind_speed_m_s')
windWide <- tidyr::pivot_wider(wind, names_from = 'position', values_from = 'wind_speed_m_s', names_prefix = 'windspeed_')
head(windWide)



### MERGE data
allData3 <- merge(windWide, allData2, by=c('by15','treatment','block'), all = TRUE)


####
# 5. Pressure bomb (block means)
###

pb <- read.csv('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/pressure_bomb/pressure_bomb_15.csv')
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
allData4 <- merge(pbMeans, allData3, by=c('by15','treatment','block'), all = TRUE)



### ----- At this point, let's save the "treatment-level only" dataset
saveRDS(allData4, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/combined_data/combdat_treatment_level_only.rds')


# read back in
# allData4 <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/combined_data/combdat_treatment_level_only.rds')


### -------------------------------------------
###  --- Lastly, merge to plant-level data ----
### -------------------------------------------



####
# 6. Leaf temperature (thermistors)
###

lt <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/leaf_thermistor_data/leaf_thermistor_data_15min_agg_flagged.rds')

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
  

# --- MERGE TO FULL DATA SET

allData5 <- merge(lt_wide, allData4, by=c('by15','treatment','block'), all = TRUE)
nrow(allData5); nrow(allData4)*4 # close to the correct number of rows (4 plants/block)


####
# 6. Transpiration data (calculated from scale weights)
###

# read data
transp <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_transpiration/transpiration_by_plant.rds')

# remove border plants
transp <- subset(transp, !grepl('border_plant', plant_id))

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
# remove rows without treatment
transp <- transp[!is.na(transp$treatment), ]

# reanme flag column
colnames(transp)[colnames(transp)=='flag'] <- 'transpiration_flag'
colnames(transp)[colnames(transp)=='mean_weight_kg'] <- 'scale_weight_kg'

# remove columns not wanted in merge
head(transp)
transp <- subset(transp, select = c(by15, plant_id, treatment, block, transpiration_flag, T_mg_s, scale_weight_kg))


ggplot(transp, aes(x=by15, y=T_mg_s))
### MERGE
allData6 <- merge(transp, allData5, by=c('by15','block','treatment','plant_id'))