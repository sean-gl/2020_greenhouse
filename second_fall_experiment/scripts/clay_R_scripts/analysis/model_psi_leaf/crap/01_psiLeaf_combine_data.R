
# File to combine data for modelling psi_leaf
# Also adds some variables (VPD, mean leaf temp, mean air temp, irrigation amount, etc...)

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



### SECTION 1: Read data sets and do some processing  -----------------


# 1. leaf temperature
lt <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/data/leaf_thermistor_data/leaf_thermistor_data_15min_agg_flagged.rds')

# remove position column, not useful
lt$position <- NULL
colnames(lt)[colnames(lt)=='canopy_position'] <- 'position'
table(lt$position, useNA = 'a')

# change position categoies to match wind data
lt$position[lt$position=='lower'] <- 'bottom'
lt$position[lt$position=='middle'] <- 'middle'
lt$position[lt$position=='upper'] <- 'top'

# filter data by flag 
lt_filter <- subset(lt, flag <= 2 & temperature_flag == 'none')
nrow(lt_filter)/nrow(lt)

# Aggregate by block
lt_block <- ddply(lt_filter, .(by15, block, treatment, position), function(x){
  setNames(mean(x$mean_leaftemp_C, na.rm = T), 'mean_leaftemp_C')
})


# 2. PAR
lq <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/line_PAR_sensors/line_PAR_15.csv')
lq$by15 <- as.POSIXct(lq$by15, tz = 'GMT')

# 3. RH, air temp, soil temp
rh <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/RH_temp_PAR_logger_data/rh_15.csv')
rh$by15 <- as.POSIXct(rh$by15, tz='GMT')

# remove data before 10-24 (just a few rows), these we don't want
rh <- subset(rh, date(by15) >= '2019-10-24')
# remove soil temp columsn, these are imported below
rh <- rh %>% select(-contains('soil_t'))

rh$par2_s <- NULL # REMOVE THIS VARIABLE, DATA ARE BAD


soil_temp <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/RH_temp_PAR_logger_data/soil_temp_15.csv')
soil_temp$by15 <- as.POSIXct(soil_temp$by15, tz='GMT')

### Merge leaf temp and "RH" (includes air temp, rh, and light data)

lat <- merge(lt_block, rh, all = T)
lat$date <- lubridate::date(lat$by15)

ind <- is.na(lat$block)
unique(lat$date[ind])

# split off data to expand (to fill in block, treatment and position)
# dat = lat[!ind, names(lat) %in% c('treatment','position','block')]
dat = lat[!ind,]
missing = lat$by15[ind]

# y = lat[ind, names(lat)[!names(lat) %in% c('position','treatment','block','mean_leaftemp_C')]]
# x = expand.grid(as.list(y), treatment=c('well_watered','full_drought','virgin_drought'))

nrow(dat)+length(missing) == nrow(lat)
edat = expand.grid(by15=missing, treatment=c('well_watered','full_drought','virgin_drought'))
edat$position <- 'top' # all thermistors were at top position for these 2 days
lat[ind,'position'] = 'top'
edat$block[edat$treatment=='well_watered'] = 'M'
edat$block[edat$treatment=='virgin_drought'] = 'W'
edat$block[edat$treatment=='full_drought'] = 'D'
with(edat, table(block, treatment, useNA = 'a'))
nrow(edat)
edat2 <- merge(edat, lat[ind,], by = c('by15','position'), all = T); nrow(edat2)
edat2[,c('block.y','treatment.y')]=NULL
names(edat2)[names(edat2)=='treatment.x'] = 'treatment'
names(edat2)[names(edat2)=='block.x'] = 'block'
all(complete.cases(edat2[,names(edat2)[!names(edat2)%in%'mean_leaftemp_C']]))
with(edat2, table(block, treatment, useNA = 'a'))
with(edat2, table(position, useNA = 'a'))

lat2 <- rbind(dat, edat2)
nrow(lat2)
nrow(dat)+nrow(edat2)
table(lat2$treatment, useNA = 'a')
table(lat2$block, useNA = 'a')
table(lat2$position, useNA = 'a')

# convert to wide
lat_wide <- tidyr::spread(lat2, 'position', 'mean_leaftemp_C')
names(lat_wide)[names(lat_wide) %in% c('bottom','middle','top')] <- c('leaftemp_bottom','leaftemp_middle','leaftemp_top')


# Since position changes, sometimes data isn't available at a given position.
# Let's add a couple variables to handle these cases.

# Highest position with available data
lat_wide$leaftemp_highest_avail <- apply(lat_wide, 1, function(x) {
  ind <- which(!is.na(x[c('leaftemp_bottom','leaftemp_middle','leaftemp_top')]))
  if(length(ind) > 0) {
    out <- as.numeric(x[c('leaftemp_bottom','leaftemp_middle','leaftemp_top')][max(ind)])
  } else out <- NA
  return(out)
})
summary(lat_wide$leaftemp_highest_avail)

# Mean of all position's data
lat_wide$leaftemp_mean <- rowMeans(lat_wide[,c('leaftemp_bottom','leaftemp_middle','leaftemp_top')], na.rm = T)


# 4. Wind sensors
wind <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/wind_sensor_data/wind_15.csv')
wind$by15 <- as.POSIXct(wind$by15, tz='GMT')
wind$date <- date(wind$by15)

# convert to long format
windWide <- tidyr::spread(wind, 'position', 'wind_speed_m_s')
colnames(windWide) <- c('by15','treatment','date','windspeed_bottom','windspeed_middle','windspeed_top')
head(windWide)


### try merging soil temp and windwide, and then fixing treatments, adding blocks
windsoil <- merge(soil_temp, windWide, all = T)

### ADD BLOCK...AND FIX SOME ISSUES WITH TREATMENTS 
windsoil$block <- NA
# baseline (pre-treatment)
ind = windsoil$date <= '2019-10-24'
windsoil$block[ind & windsoil$treatment == 'well_watered'] <- 'W'
windsoil$block[ind & windsoil$treatment == 'moderate_drought'] <- 'M'
windsoil$block[ind & windsoil$treatment == 'full_drought'] <- 'D'
# also, fix treatment (should all be well_watered)
windsoil$treatment[ind] <- 'well_watered'
# treatment 1
ind = windsoil$date > '2019-10-24' & windsoil$date <= '2019-11-04'
windsoil$block[ind & windsoil$treatment == 'well_watered'] = 'W'
windsoil$block[ind & windsoil$treatment == 'moderate_drought'] = 'M'
windsoil$block[ind & windsoil$treatment == 'full_drought'] = 'D'
# treatment 2
ind = windsoil$date > '2019-11-04' & windsoil$date <= '2019-11-27'
windsoil$block[ind & windsoil$treatment == 'well_watered'] = 'D'
windsoil$block[ind & windsoil$treatment == 'moderate_drought'] = 'M'
windsoil$block[ind & windsoil$treatment == 'full_drought'] = 'W'
# note: there are some virgins on 11/27. They were switched midday but treatment didn't 
# start until that night. For consistency, I'll say it started on 11/28.
windsoil$block[ind & windsoil$treatment == 'virgin_drought'] = 'W'
windsoil$treatment[ind & windsoil$treatment == 'virgin_drought'] = 'full_drought'

# treatment 3 (final)
ind = windsoil$date > '2019-11-27'
windsoil$block[ind & windsoil$treatment == 'well_watered'] = 'M'
windsoil$block[ind & windsoil$treatment == 'virgin_drought'] = 'W'
windsoil$block[ind & windsoil$treatment == 'full_drought'] = 'D'
# convert to factor
windsoil$block <- as.factor(windsoil$block)
table(windsoil$block, useNA = 'always')
table(windsoil$treatment, useNA = 'always')

# 5. Pressure bomb data
pb <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/pressure_bomb/pressure_bomb_15.csv')
pb$by15 <- as.POSIXct(pb$by15, tz='GMT')

# omit bad observation & missing observation
pb <- pb[pb$data_ok=='yes' & !is.na(pb$psi_MPa), ]

## Make some edits to data this day...
pb$by15[date(pb$by15)=='2019-11-15' & pb$treatment=='moderate_drought'] <- '2019-11-15 13:45:00 GMT'

# get means by day and treatment/block
pb$block <- toupper(substr(pb$plant_id,1,1))
pb$date <- lubridate::date(pb$by15)
pbMeans <- ddply(pb, .(date, by15, block, treatment), function(x) {
  setNames(mean(x$psi_MPa), 'mean_psi_MPa')
})
with(pbMeans, table(date))


# 6. Garret's soil matric potential data (and soil temperature)
# soilmat <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/data/soil_water_potential/soil_water_potential_compiled_condensed.rds')
# head(soilmat)
# 
# # remove the "mean soil temp column" (just use the 1 sensor that is always available)
# soilmat$teros_soil_temp_mean_C <- NULL




### SECTION 3. Merge datasets and add more variables ---------------------

## Examine start/end dates
# start: '10-23-17:00' (lq)
# end: '12-12 15:45' (lat_wide)
# summary(lq$by15)
# summary(soil_temp$by15)
# summary(windWide$by15)
# summary(lat_wide$by15)
# nrow(lq); nrow(soil_temp)
# summary(rh$by15)


# Begin by merging lat_wide with lq, since lat_wide has treatment & block (and latest time), 
# and lq has earliest time....

# first fill in dates/treatments missing in lat_wide that are in lq.
ind <- which(!lq$by15 %in% lat_wide$by15)
# ind
df = data.frame(by15 = lq$by15[ind])
df1 = subset(df, date(by15) <= '2019-10-24')
df2 = subset(df, date(by15) >= '2019-12-11'); nrow(df1)+nrow(df2)
df1 = expand.grid(df1$by15, 'well_watered', c('W','M','D'))
df2 = expand.grid(df2$by15, c('well_watered','virgin_drought','full_drought'))
df2$Var3[df2$Var2=='well_watered'] = 'M'
df2$Var3[df2$Var2=='virgin_drought'] = 'W'
df2$Var3[df2$Var2=='full_drought'] = 'D'
df3 = rbind(df1, df2)
names(df3)=c('by15','treatment','block')
df4 = merge(lat_wide, df3, all=T)
with(df4, table(block, treatment, useNA = 'a'))
with(subset(df4, date(by15) <= '2019-10-24'), table(treatment))

# now merge 
nrow(lq); nrow(df4)
comb <- merge(lq, df4, all=T); nrow(comb)
with(comb, table(treatment, block, useNA = 'a'))
with(subset(comb, date(by15) <= '2019-10-24'), table(treatment,block,useNA = 'a'))
# check for any duplicated column names in merges above
which(grepl('\\.x', names(comb)) | grepl('\\.y', names(comb)))

# now merge in wind + soil data
nrow(comb)
comb <- merge(comb, windsoil, by=c('by15', 'treatment','block','date'), all = T); nrow(comb)
with(comb, table(treatment, block, useNA = 'a'))

# check for any duplicated column names in merges above
which(grepl('\\.x', names(comb)) | grepl('\\.y', names(comb)))

# now merge in pressure bomb means
comb <- merge(comb, pbMeans, by=c('by15','date','treatment','block'), all = T); nrow(comb)

# check for any duplicated column names in merges above
which(grepl('\\.x', names(comb)) | grepl('\\.y', names(comb)))

# lastly, merge in Garret's soil matric potential data
# comb <- merge(comb, soilmat, by=c('date','by15','block'), all = T); nrow(comb)

# check for any duplicated column names in merges above
# which(grepl('\\.x', names(comb)) | grepl('\\.y', names(comb)))

# add "minutes" (of day) column
comb$minutes <- 60*hour(comb$by15) + minute(comb$by15)

# add irrigation amount (ml)
comb$date <- date(comb$by15)
comb$irrig <- NA
comb$irrig[comb$date < "2019-11-05" & comb$treatment == 'well_watered'] <- 750
comb$irrig[comb$date >= "2019-11-05" & comb$treatment == 'well_watered'] <- 1000
comb$irrig[comb$treatment == 'moderate_drought'] <- 375
comb$irrig[comb$treatment %in% c('full_drought','virgin_drought')] <- 150
table(comb$irrig, useNA = 'a')

# add calculated irrigation amounts
irrigDat <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/mean_irrigation_by_block.rds')

# cummulative stress index, by block and date
irrigDat$stress_index <- 0

# trt 1
ind <- with(irrigDat, date >= '2019-10-24' & date <= '2019-11-03' & block == 'D')
irrigDat$stress_index[ind] <- cumsum(1-irrigDat$mean_irrig_kg[ind])
# M block has longer treatment period 
ind <- with(irrigDat, date >= '2019-10-24' & date <= '2019-11-27' & block == 'M')
irrigDat$stress_index[ind] <- cumsum(1-irrigDat$mean_irrig_kg[ind])

# trt2
ind <- with(irrigDat, date >= '2019-11-04' & date <= '2019-11-27' & block == 'W')
irrigDat$stress_index[ind] <- cumsum(1-irrigDat$mean_irrig_kg[ind])

# trt 3
ind <- with(irrigDat, date >= '2019-11-28' & block == 'D')
irrigDat$stress_index[ind] <- cumsum(1-irrigDat$mean_irrig_kg[ind])
ind <- with(irrigDat, date >= '2019-11-28' & block == 'W')
irrigDat$stress_index[ind] <- cumsum(1-irrigDat$mean_irrig_kg[ind])
plot(cumsum(1-irrigDat$mean_irrig_kg[ind]))

comb <- merge(comb, irrigDat[,c('date','block','mean_irrig_kg','stress_index')], all.x = T)
any(is.na(comb$mean_irrig_kg))


# RH high are strongly correlated with each other and with RH low
# cor(comb$sht1_high_rh, comb$am2320_high_rh, use = 'complete.obs')
# cor(comb$sht2_low_rh, comb$sht1_high_rh,  use = 'complete.obs')
# 
# # correlation of air temp with psi_leaf
# cor(comb$sht1_high_temp, comb$mean_psi_MPa, use = 'complete.obs')
# cor(comb$am2320_high_temp, comb$mean_psi_MPa, use = 'complete.obs')
# 
# cor(comb$sht2_low_temp, comb$mean_psi_MPa, use = 'complete.obs')
# cor(comb$bmp_box_temp, comb$mean_psi_MPa, use = 'complete.obs')
# 
# # correlation of leaftemp and psi_leaf
# cor(comb$leaftemp_bottom, comb$mean_psi_MPa, use = 'complete.obs')
# cor(comb$leaftemp_middle, comb$mean_psi_MPa, use = 'complete.obs') # highest r but smaller n
# cor(comb$leaftemp_top, comb$mean_psi_MPa, use = 'complete.obs')
# 
# cor(comb$leaftemp_highest_avail, comb$mean_psi_MPa, use = 'complete.obs') # highest n and r
# cor(comb$leaftemp_mean, comb$mean_psi_MPa, use = 'complete.obs')
# summary(comb$leaftemp_bottom); summary(comb$leaftemp_middle); summary(comb$leaftemp_top)
# 
# # use mean "high" RH and temp
# comb$rh_high_mean <- rowMeans(comb[ , c('sht1_high_rh','am2320_high_rh')], na.rm = T)
# comb$temp_high_mean <- rowMeans(comb[ , c('sht1_high_temp','am2320_high_temp')], na.rm = T)
# cor(comb$rh_high_mean, comb$mean_psi_MPa, use='complete.obs')
# cor(comb$temp_high_mean, comb$mean_psi_MPa, use='complete.obs')

# calculate VPD_leaf based on leaf temperature
comb$VPD_leaf <- (1 - (comb$rh_high_mean / 100)) * 0.61121 * exp((17.502 * comb$leaftemp_highest_avail) / (240.97 + comb$leaftemp_highest_avail)) 

# using low RH/temp has best correlation w/ psi_leaf
comb$VPD_air <- (1 - (comb$sht2_low_rh / 100)) * 0.61121 * exp((17.502 * comb$sht2_low_temp) / (240.97 + comb$sht2_low_temp)) 
# comb$VPD_air <- (1 - (comb$sht1_high_rh / 100)) * 0.61121 * exp((17.502 * comb$sht1_high_temp) / (240.97 + comb$sht1_high_temp)) 

summary(comb$VPD_leaf); summary(comb$VPD_air)
cor(comb$VPD_leaf, comb$mean_psi_MPa, use='complete.obs')
cor(comb$VPD_air, comb$mean_psi_MPa, use='complete.obs')

### Add days since treatment started
summary(comb$date)
comb$daysPostTrt <- NA
ind <- comb$date < '2019-10-25'
comb$daysPostTrt[ind] <- 0 # put zero for days before any treatment began.
ind <- comb$date >= '2019-10-25' & comb$date < '2019-11-05' 
comb$daysPostTrt[ind] <- comb$date[ind] - as.Date('2019-10-25')
ind <- comb$date > '2019-11-04' & comb$date < '2019-11-28'
comb$daysPostTrt[ind] <- comb$date[ind] - as.Date('2019-11-05')
ind <- comb$date > '2019-11-27' 
comb$daysPostTrt[ind] <- comb$date[ind] - as.Date('2019-11-28')
summary(comb$daysPostTrt)

# Change blcok to factor
comb$block <- as.factor(comb$block)

# Save combined data 
saveRDS(comb, '/home/sean/github/2020_greenhouse/second_fall_experiment/data/combined_data/pressure_bomb_combined_data.rds')

# comb <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/data/combined_data/pressure_bomb_combined_data.rds')


## Check date ranges for data sets
datnames <- c('lq','lt','rh','soil_temp','wind','comb')
for(nm in datnames) {
  print(nm)
  print(min(get(nm)$by15))
  print(max(get(nm)$by15))
}
