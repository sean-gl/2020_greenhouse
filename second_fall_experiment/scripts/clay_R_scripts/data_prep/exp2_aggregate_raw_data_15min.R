   
setwd("/home/sean/github/2020_greenhouse/second_fall_experiment/figures")
# on U drive
# setwd("U:/Staff Folders/Gleason, Sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/figures")

# load data & libaries
library(smatr)
# library(RColorBrewer)
library(doBy)
#library(shape)
#library(scales)
library(minpack.lm)
library(openxlsx)
library(lubridate)

# require(devtools)
# library(devtools)
# install_version("glmnet", version = "2.0-18", repos = "https://cloud.r-project.org")
# library(glmnet)

#library(magick)

# rm(list=ls())
  
###_______ organize data and save as 15-min .csv files  _______### 
###_______ organize data and save as 15-min .csv files  _______### 
###_______ organize data and save as 15-min .csv files  _______### 

# assign treatment dates... don't forget the "tz='GMT' !!
exp1_start <- as.POSIXct('2019-10-24 12:00:00', tz='GMT')
exp1_end   <- as.POSIXct('2019-11-04 16:00:00', tz='GMT') # clay: changed end time from 09:00, to get more pressure bomb data

exp2_start <- as.POSIXct('2019-11-04 17:00:00', tz='GMT')
exp2_end   <- as.POSIXct('2019-11-27 16:00:00', tz='GMT') # clay: changed end time from 09:00, to get more pressure bomb data

exp3_start <- as.POSIXct('2019-11-27 17:00:00', tz='GMT')
exp3_end   <- as.POSIXct('2019-12-12 16:00:00', tz='GMT') # clay: changed end time from 09:00, to get more pressure bomb data


###________________PAR, RH, soil_temp data_____________###
###________________PAR, RH, soil_temp data_____________###

rh1 <- read.table(paste("/home/sean/github/2020_greenhouse/second_fall_experiment//",
                        "data/RH_temp_PAR_logger_data/read_only/11_4_2019_v2.TXT", sep=""), header = F, sep = ",")
rh2 <- read.table(paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                          "data/RH_temp_PAR_logger_data/read_only/11_18_2019.TXT", sep=""), header = F, sep = ",")
rh3 <- read.table(paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                        "data/RH_temp_PAR_logger_data/read_only/12_12_2019.TXT", sep=""), header = F, sep = ",")

rh <- rbind.data.frame(rh1, rh2, rh3)
colnames(rh) <- c('par1_n', 'par2_s', 'pyr1_n', 'pyr2_s', 'am2320_high_temp', 'am2320_high_rh', 'sht1_high_temp', 
                  'sht1_high_rh', 'sht2_low_temp', 'sht2_low_rh', 'bmp_box_temp', 'bmp_box_atm_p', 'soil_t1', 
                  'soil_t2', 'soil_t3', 'soil_t4', 'date', 'time')
# radiation sensors moved to the top of beams on 9/3/2019... use this as the start date for usable data
# sht1 and am2320 sensors wre placed ca 1m above canopy
# sht2 was placed at 75% canopy height
# the bmp atmopheric pressure and temp were taken inside the shadded datalogger box
# par1_n was placed on the beam (above leds) on north side of the plants
# par2_s was placed on the beam (above leds) on sounth side of the plants
# ditto for pyr -n and -s sensors.
# soil sensors wre placed inside plots approximately at 50% soil depth

# add POSIXct time
rh$date_time <- paste(rh$date, rh$time); rh$date_time[1:10]
rh$date_time <- as.POSIXct(rh$date_time, format = "%d/%m/%y %k:%M:%S", tz = "GMT")
rh$date_time[1:15]


# --- QA/QC

# note: most sensors have some stray data collected on 10/21 and then a gap until 10/24,
# when treatments began. I'll just delete all the data on 10/21, since we won't be using it anyway.
rh <- rh[rh$date_time > as.POSIXct('2019-10-22 00:00', tz='GMT'), ]

# PAR/PYR DATA

# Note: many values below zero, looks like mostly not during daylight hours, but should check...
summary(rh[,c('par1_n','par2_s','pyr1_n','pyr2_s')])

# ---par 1: data more noisy in first half of experiment...otherwise looks mostly ok.
plot(rh$date_time, rh$par1_n); abline(c(0,0), col='red')

ind <- with(rh, date_time >= as.POSIXct('2019-11-01 08:00', tz='GMT') & 
              date_time <= as.POSIXct('2019-11-02 15:00', tz='GMT'))
plot(par1_n ~ date_time, rh[ind,], type='b')

ind <- with(rh, date_time >= as.POSIXct('2019-11-29 08:00', tz='GMT') & 
              date_time <= as.POSIXct('2019-11-29 15:00', tz='GMT'))
plot(par1_n ~ date_time, rh[ind,], type='b')

# --- par2_s: lots of abberantly high/low values, esp after mid-Nov.
plot(rh$date_time, rh$par2_s); abline(c(0,0), col='red')


# ----
# ---- let's clean par2_s up ----

sub = subset(rh, date(date_time) <= '2019-10-23')
plot(par2_s ~ date_time, sub) # omit values before Oct 24, it's junk
rh$par2_s[rh$date_time <= '2019-10-23'] <- NA
sub = subset(rh, date(date_time) >= '2019-10-23' & date(date_time) <= '2019-10-25')
plot(par2_s ~ date_time, sub) # data looks very odd, not sure what to do here but will just NA it for now.
sub = subset(rh, date_time >= as.POSIXct('2019-10-24 11:00', tz='GMT') & 
               date_time <= as.POSIXct('2019-10-25 16:30', tz='GMT'))
plot(par2_s ~ date_time, sub); identify(sub$date_time, sub$par2_s, sub, labels = sub$time)

# set bad data to NA
ind <- with(rh, date_time >= as.POSIXct('2019-10-24 11:00', tz='GMT') & 
              date_time <= as.POSIXct('2019-10-25 16:00', tz='GMT'))
plot(rh$par2_s[ind])
rh$par2_s[ind] <- NA

# Now check later time periods
ind <- with(rh, date_time >= as.POSIXct('2019-11-12 05:10', tz='GMT') & 
              date_time <= as.POSIXct('2019-11-13 16:50', tz='GMT'))
plot(par2_s~date_time, rh[ind,], ylim=c(0,1000))
# isolate the period 
ind <- with(rh, date_time >= as.POSIXct('2019-11-13 13:00', tz='GMT') & 
              date_time <= as.POSIXct('2019-11-13 18:00', tz='GMT'))
# looks like 13:25 - 16:50 should cover it.
plot(par2_s~date_time, rh[ind,],ylim=c(0,1000))
identify(rh$date_time[ind], rh$par2_s[ind], labels = rh$time[ind])

# set bad data to NA
ind <- with(rh, date_time >= as.POSIXct('2019-11-13 13:25', tz='GMT') & 
              date_time <= as.POSIXct('2019-11-13 16:50', tz='GMT'))
plot(par2_s~date_time, rh[ind,])
rh$par2_s[ind] <- NA

# check
ind <- with(rh, date_time >= as.POSIXct('2019-11-11 00:00', tz='GMT') & 
              date_time <= as.POSIXct('2019-11-15 00:00', tz='GMT'))
plot(par2_s~date_time, rh[ind,]) # looks good

# Now, the end of experiment looks dicey...
# see a overview: looks like most of data after 11/15 is bad, could be some ok data on 12/11?
ind <- with(rh, date_time >= as.POSIXct('2019-11-14 00:00', tz='GMT') & 
              date_time <= as.POSIXct('2019-12-13 00:00', tz='GMT'))
plot(par2_s~date_time, rh[ind,])

# find the starting point
ind <- with(rh, date_time >= as.POSIXct('2019-11-15 10:50', tz='GMT') & 
              date_time <= as.POSIXct('2019-11-15 12:00', tz='GMT'))
plot(par2_s~date_time, rh[ind,]) # looks like 11/15 11:00 is the start of bad data.

# find the end, if there is one
ind <- with(rh, date_time >= as.POSIXct('2019-12-11 08:00', tz='GMT') & 
              date_time <= as.POSIXct('2019-12-13 00:00', tz='GMT'))
plot(par2_s~date_time, rh[ind,], ylim=c(0,1000)) # zoom in
plot(par2_s~date_time, rh[ind,])
identify(rh$date_time[ind], rh$par2_s[ind], labels = rh$time[ind])
# looks like 12/11 10:30 to 12/12 09:50 data are probably ok
ind <- with(rh, date_time >= as.POSIXct('2019-12-11 10:30', tz='GMT') & 
              date_time <= as.POSIXct('2019-12-12 09:50', tz='GMT'))
plot(par2_s~date_time, rh[ind,]) # yep

# now remove bad data after 11/15
ind <- with(rh, (date_time > as.POSIXct('2019-11-15 11:00', tz='GMT') & 
              date_time < as.POSIXct('2019-12-11 10:30', tz='GMT')) |
              date_time > as.POSIXct('2019-12-12 09:50', tz='GMT'))
plot(par2_s~date_time, rh[ind,])
rh$par2_s[ind] <- NA

# Finally, plot all par2_s data again to check cleaning efforts
plot(par2_s~date_time, rh) # looks good!

# ----
# ---- END,  par2_s clean-up ----


# --- pyr1_n: Mostly looks ok. Note an odd "sinking baseline" starting 2nd-3rd week in Nov.
plot(rh$date_time, rh$pyr1_n); abline(c(0,0), col='red')

# --- pyr2_s: Similar to pyr1_n
plot(rh$date_time, rh$pyr2_s); abline(c(0,0), col='red')

# Plot air temp. sensors together
# Data look ok, and sensors track each other.
plot(rh$date_time, rh$am2320_high_temp, type='l')
lines(rh$date_time, rh$sht1_high_temp, col='red')
lines(rh$date_time, rh$sht2_low_temp, col='blue')
lines(rh$date_time, rh$bmp_box_temp, col='green')

# Plot RH sensors. Data look ok, sensors track each other. 
# Note that the am2320 has much lower RH values than other sensors.
plot(rh$date_time, rh$am2320_high_rh, type='l', ylim=c(0, 60))
lines(rh$date_time, rh$sht1_high_rh, col='red')
lines(rh$date_time, rh$sht2_low_rh, col='blue')

# atm pressure; looks ok to me but don't know if these values are believable (or important)
plot(rh$date_time, rh$bmp_box_atm_p)

# Plot soil temp. sensors together
# Data mostly look ok, with high values at very start...
plot(rh$date_time, rh$soil_t1, type='l')
lines(rh$date_time, rh$soil_t2, col='red')
lines(rh$date_time, rh$soil_t3, col='blue')
lines(rh$date_time, rh$soil_t4, col='green')

# look into high values at start
ind <- with(rh, date_time < as.POSIXct('2019-10-25 11:00', tz='GMT'))
plot(soil_t1~date_time, rh[ind,])
points(rh$date_time, rh$soil_t2, col='red')
points(rh$date_time, rh$soil_t3, col='blue')
points(rh$date_time, rh$soil_t4, col='green')
# looks like only 2 obs. but all 4 sensors, have value > 70 
which(rh$soil_t1 > 70)
which(rh$soil_t3 > 70)
which(rh$soil_t4 > 70)
which(rh$soil_t2 > 70)
# omit these data
rh[130:131, c('soil_t1','soil_t2','soil_t3','soil_t4')] <- NA




# "cut" data into 15 minute groups and take the mean
rh$by15 <- lubridate::ceiling_date(rh$date_time, "15 minutes")   # CHANGE HERE TO CHANGE TIME INTERVAL
# calculate mean mass for each 15-minute time group by scale number
rh <- summaryBy(par1_n + par2_s + pyr1_n + pyr2_s + am2320_high_temp + am2320_high_rh + sht1_high_temp + 
                  sht1_high_rh + sht2_low_temp + sht2_low_rh + bmp_box_temp + bmp_box_atm_p + soil_t1 + 
                  soil_t2 + soil_t3 + soil_t4 ~ by15, data=rh, FUN=mean, na.rm=TRUE, keep.names=TRUE)
rh <- rh[order(rh$by15),]



# write rh data to file
write.csv(rh, paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                    "data/RH_temp_PAR_logger_data/rh_15.csv", sep=""), row.names=FALSE)

# build another df with only temps, but all data in one column
# separate each soil sensor
# t1=west block; t2=east block; t3=also east block (i.e., t2 and t3 are the same treatment); t4=middle block
sen1 <- subset(rh, select=c(by15, soil_t1))
sen2 <- subset(rh, select=c(by15, soil_t2))
sen3 <- subset(rh, select=c(by15, soil_t3))
sen4 <- subset(rh, select=c(by15, soil_t4))

# take mean of soil sensors T2 and T3 because they are both in same block (treatment sequence)
sen2_3 <- merge(sen2, sen3, by='by15')
colnames(sen2_3) <- c('by15', 't2', 't3')
sen2_3$mean_temp <- apply(sen2_3[,c('t2','t3')], 1, mean, na.rm=TRUE)
sen2_3 <- subset(sen2_3, select=c(by15, mean_temp))

# CLAY: added "block" variable, to help with merge to other data sets
sen1$block <- 'D'; sen2_3$block <- 'W'; sen4$block <- 'M'

# change column name and add correct treatment to each date
# first soil sensor (T1) (in west block)
df <- sen1
  colnames(df) <- c('by15', 'soil_temp_C', 'block')
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp1_start & 
                          as.POSIXct(df$by15, tz='GMT') <= exp1_end, "full_drought", "woo")
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp2_start & 
                          as.POSIXct(df$by15, tz='GMT') <= exp2_end, "well_watered", df$treat)
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp3_start & 
                          as.POSIXct(df$by15, tz='GMT') <= exp3_end, "full_drought", df$treat)
  df <- subset(df, treatment == 'full_drought' | treatment == 'well_watered' |
                  treatment == 'moderate_drought' | treatment == 'virgin_drought') # remove "woos"
sen1 <- df

# now, the second and third soil sensors (sen2_3 = mean of T2 and T3) (in east block)
df <- sen2_3
  colnames(df) <- c('by15', 'soil_temp_C', 'block')
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp1_start & 
                           as.POSIXct(df$by15, tz='GMT') <= exp1_end, "well_watered", "woo")
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp2_start & 
                           as.POSIXct(df$by15, tz='GMT') <= exp2_end, "full_drought", df$treat)
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp3_start & 
                           as.POSIXct(df$by15, tz='GMT') <= exp3_end, "virgin_drought", df$treat)
  df <- subset(df, treatment == 'full_drought' | treatment == 'well_watered' |
                 treatment == 'moderate_drought' | treatment == 'virgin_drought') # remove "woos"
sen2_3 <- df
  
# lastly, the fourth soil sensor (T4) (in the middle block)
df <- sen4
  colnames(df) <- c('by15', 'soil_temp_C', 'block')
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp1_start & 
                         as.POSIXct(df$by15, tz='GMT') <= exp1_end, "moderate_drought", "woo")
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp2_start & 
                         as.POSIXct(df$by15, tz='GMT') <= exp2_end, "moderate_drought", df$treat)
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp3_start & 
                         as.POSIXct(df$by15, tz='GMT') <= exp3_end, "well_watered", df$treat)
  df <- subset(df, treatment == 'full_drought' | treatment == 'well_watered' |
               treatment == 'moderate_drought' | treatment == 'virgin_drought') # remove "woos"
sen4 <- df  

# rbind all sensor dataframes together
comb <- rbind.data.frame(sen1, sen2_3, sen4)
# export to file
write.csv(comb, paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                    "data/RH_temp_PAR_logger_data/soil_temp_15.csv", sep=""), row.names=FALSE)

# testing
# subsetting data
sub <- subset(rh, as.POSIXct(by15, tz='GMT') > as.POSIXct('2019-11-04 01:00:00', tz='GMT')
                & as.POSIXct(by15, tz='GMT') < as.POSIXct('2019-11-27 01:00:00', tz='GMT'))

plot(sub$soil_t1 ~ sub$by15, type='l', ylim=c(17,31))
points(sub$soil_t2 ~ sub$by15, type='l', col='red')
points(sub$soil_t3 ~ sub$by15, type='l', col='blue')
points(sub$soil_t4 ~ sub$by15, type='l', col='green')
sma1 <- sma(sub$soil_t1 ~ sub$soil_t4); summary(sma1); plot(sma1)
sma1 <- sma(sub$soil_t2 ~ sub$soil_t3); summary(sma1); plot(sma1)



###_______________ import wind data____________________###
###_______________ import wind data____________________###

wind1 <- read.table(paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                          "data/wind_sensor_data/read_only/11_4_2019.TXT", sep=""), header = F, sep = ",")
wind2 <- read.table(paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                          "data/wind_sensor_data/read_only/11_18_2019.TXT", sep=""),  header = F, sep = ",")
wind3 <- read.table(paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                          "data/wind_sensor_data/read_only/12_12_2019.TXT", sep=""), header = F, sep = ",")
wind <- rbind.data.frame(wind1, wind2, wind3)
# don't need data from 12_5_2019.TXT because this is inlcuded in 12_12_2019.TXT file
# holy crap... over a million rows of wind data...

colnames(wind) <- c('w1_sb_m_s', 'w1_sm_m_s', 'w1_st_m_s', 
                    'w2_sb_m_s', 'w2_sm_m_s', 'w2_st_m_s',
                    'w3_sb_m_s', 'w3_sm_m_s', 'w3_st_m_s',
                    'temp1', 'temp2', 'temp3',
                    'temp4', 'temp5', 'temp5',
                    'date', 'time')
# "1" denotes sensors on the eastern most arra; "2" in the middle, and "3" on western most array
# omit crap data
wind <- wind[10:nrow(wind),]

# set date and time to POSIXct without daylight savings (GMT)
wind$date_time <- paste(wind$date, wind$time); wind$date_time[1:10]
wind$date_time <- as.POSIXct(wind$date_time, format = "%d/%m/%y %k:%M:%S", tz = "GMT")
wind$date_time[1:15]
# wind$date <- substring(wind$date_time, first=1, last=10); wind$date[1:5]
# wind$time <- substring(wind$date_time, first=12, last=19); wind$time[1:5]

# "cut" data into 15 minute groups and take the mean
wind$by15 <- lubridate::ceiling_date(wind$date_time, "15 minutes")   # CHANGE HERE TO CHANGE TIME INTERVAL
# calculate mean mass for each 15-minute time group by scale number
wind <- summaryBy(w1_sb_m_s + w1_sm_m_s + w1_st_m_s + w2_sb_m_s + w2_sm_m_s + w2_st_m_s +
                  w3_sb_m_s + w3_sm_m_s + w3_st_m_s ~ by15, data=wind, FUN=mean, 
                  na.rm=TRUE, keep.names=TRUE)
wind <- wind[order(wind$by15),]

# to convert V to m s-1, import temperature from rh sensors
temp <- read.csv(paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                "data/RH_temp_PAR_logger_data/rh_15.csv", sep=""), header=TRUE, sep=",")
temp <- subset(temp, select=c(by15, sht2_low_temp))
temp$by15 <- as.character(temp$by15); wind$by15 <- as.character(wind$by15)
comb <- merge(wind, temp, by='by15', all.x=TRUE, all.y=FALSE)
comb$by15 <- as.POSIXct(comb$by15, tz='GMT')  # back to POSIXct

# calculate wind speed 
# function to convert volts to meters per second
to_m_s <- function(x, t) {
  v <- 1.36; temp <- t # volts in still air; air temp (takne from sht2_lower_temp)
  x <- ifelse(x <= 1.36, 1.36, x)  # to prevent -inf
  mph <- (((x-v) / (3.038517 * (temp^0.115157 ))) / 
            0.087288 )^3.009364
  k_h <- mph * 1.60934
  m_h <- k_h * 1000
  m_s <- m_h/60/60
  m_s
}

# replace V with m/s
comb$w1_sb_m_s <- to_m_s(comb$w1_sb_m_s, comb$sht2_low_temp)
comb$w1_sm_m_s <- to_m_s(comb$w1_sm_m_s, comb$sht2_low_temp)
comb$w1_st_m_s <- to_m_s(comb$w1_st_m_s, comb$sht2_low_temp)
comb$w2_sb_m_s <- to_m_s(comb$w2_sb_m_s, comb$sht2_low_temp)
comb$w2_sm_m_s <- to_m_s(comb$w2_sm_m_s, comb$sht2_low_temp)
comb$w2_st_m_s <- to_m_s(comb$w2_st_m_s, comb$sht2_low_temp)
comb$w3_sb_m_s <- to_m_s(comb$w3_sb_m_s, comb$sht2_low_temp)
comb$w3_sm_m_s <- to_m_s(comb$w3_sm_m_s, comb$sht2_low_temp)
comb$w3_st_m_s <- to_m_s(comb$w3_st_m_s, comb$sht2_low_temp)

# rearrange dataframe such that all wind data are in one column and extra 
# treatment and height columns are added
# stack the sensors within each array, assign
# array1 (east block)
df1 <- comb[,c('by15', 'w1_sb_m_s')]; colnames(df1) <- c('by15', "wind_speed_m_s"); df1$position <- "bottom"
df2 <- comb[,c('by15', 'w1_sm_m_s')]; colnames(df2) <- c('by15', "wind_speed_m_s"); df2$position <- "middle"
df3 <- comb[,c('by15', 'w1_st_m_s')]; colnames(df3) <- c('by15', "wind_speed_m_s"); df3$position <- "top"
array1 <- rbind.data.frame(df1, df2, df3)
# array2 (middle block)    
df1 <- comb[,c('by15', 'w2_sb_m_s')]; colnames(df1) <- c('by15', "wind_speed_m_s"); df1$position <- "bottom"
df2 <- comb[,c('by15', 'w2_sm_m_s')]; colnames(df2) <- c('by15', "wind_speed_m_s"); df2$position <- "middle"
df3 <- comb[,c('by15', 'w2_st_m_s')]; colnames(df3) <- c('by15', "wind_speed_m_s"); df3$position <- "top"
array2 <- rbind.data.frame(df1, df2, df3)
# array3 (west block)
df1 <- comb[,c('by15', 'w3_sb_m_s')]; colnames(df1) <- c('by15', "wind_speed_m_s"); df1$position <- "bottom"
df2 <- comb[,c('by15', 'w3_sm_m_s')]; colnames(df2) <- c('by15', "wind_speed_m_s"); df2$position <- "middle"
df3 <- comb[,c('by15', 'w3_st_m_s')]; colnames(df3) <- c('by15', "wind_speed_m_s"); df3$position <- "top"
array3 <- rbind.data.frame(df1, df2, df3)

# within each array, change column name and add correct treatment to each date
# array1 (east block)
df <- array1
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp1_start & 
                         as.POSIXct(df$by15, tz='GMT') <= exp1_end, "well_watered", "woo")
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp2_start & 
                         as.POSIXct(df$by15, tz='GMT') <= exp2_end, "full_drought", df$treat)
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp3_start & 
                         as.POSIXct(df$by15, tz='GMT') <= exp3_end, "virgin_drought", df$treat)
df <- subset(df, treatment == 'full_drought' | treatment == 'well_watered' |
               treatment == 'moderate_drought' | treatment == 'virgin_drought') # remove "woos"
array1 <- df
array1$block <- 'W' # Clay: add block for merge to other datasets

# array2 (middle block)
df <- array2
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp1_start & 
                         as.POSIXct(df$by15, tz='GMT') <= exp1_end, "moderate_drought", "woo")
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp2_start & 
                         as.POSIXct(df$by15, tz='GMT') <= exp2_end, "moderate_drought", df$treat)
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp3_start & 
                         as.POSIXct(df$by15, tz='GMT') <= exp3_end, "well_watered", df$treat)
df <- subset(df, treatment == 'full_drought' | treatment == 'well_watered' |
               treatment == 'moderate_drought' | treatment == 'virgin_drought') # remove "woos"
array2 <- df
array2$block <- 'M' # Clay: add block for merge to other datasets

# array3 (west block)
df <- array3
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp1_start & 
                         as.POSIXct(df$by15, tz='GMT') <= exp1_end, "full_drought", "woo")
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp2_start & 
                         as.POSIXct(df$by15, tz='GMT') <= exp2_end, "well_watered", df$treat)
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') >= exp3_start & 
                         as.POSIXct(df$by15, tz='GMT') <= exp3_end, "full_drought", df$treat)
df <- subset(df, treatment == 'full_drought' | treatment == 'well_watered' |
               treatment == 'moderate_drought' | treatment == 'virgin_drought') # remove "woos"
array3 <- df
array3$block <- 'D' # Clay: add block for merge to other datasets

# rbind all array dfs together
stacked <- rbind.data.frame(array1, array2, array3)


# --- QA/QC 

summary(stacked$wind_speed_m_s)
boxplot(stacked$wind_speed_m_s)

# block W
plot(wind_speed_m_s ~ by15, data=subset(stacked, block=='W' & position=='bottom'), type='p')
plot(wind_speed_m_s ~ by15, data=subset(stacked, block=='W' & position=='middle'), type='p')
plot(wind_speed_m_s ~ by15, data=subset(stacked, block=='W' & position=='top'), type='p')


# block M
plot(wind_speed_m_s ~ by15, data=subset(stacked, block=='M' & position=='bottom'), type='p')
plot(wind_speed_m_s ~ by15, data=subset(stacked, block=='M' & position=='middle'), type='p')
plot(wind_speed_m_s ~ by15, data=subset(stacked, block=='M' & position=='top'), type='p')

# block D
plot(wind_speed_m_s ~ by15, data=subset(stacked, block=='D' & position=='bottom'), type='p')
plot(wind_speed_m_s ~ by15, data=subset(stacked, block=='D' & position=='middle'), type='p')
plot(wind_speed_m_s ~ by15, data=subset(stacked, block=='D' & position=='top'), type='p')

### I don't see any obviously bad data... 

#  ----- Add column to indicate yes/no for when box fans were on/off
sub = subset(stacked, by15 > as.POSIXct('2019-10-28 13:00', tz='GMT') & 
               by15 < as.POSIXct('2019-10-28 16:00', tz='GMT'))
plot(wind_speed_m_s ~ by15, data=subset(sub, block=='M' & position=='middle'), type='b')
lines(wind_speed_m_s ~ by15, data=subset(sub, block=='D' & position=='middle'), col='red')
lines(wind_speed_m_s ~ by15, data=subset(sub, block=='W' & position=='middle'), col='blue')
# looks like data before 10/28 15:00 had box fans off
stacked$box_fans_on <- 'yes'
stacked$box_fans_on[stacked$by15 < as.POSIXct('2019-10-28 15:00', tz='GMT')] <- 'no'

# Box fans (and ceiling fans) turned off for part of 12/5 (see notebook)
ind <- with(stacked, by15 > as.POSIXct('2019-12-05 07:00', tz='GMT') &
              by15 < as.POSIXct('2019-12-06 13:00', tz='GMT'))
sub <- stacked[ind,]
plot(wind_speed_m_s ~ by15, data=subset(sub, block=='M' & position=='middle'), type='b')
lines(wind_speed_m_s ~ by15, data=subset(sub, block=='D' & position=='middle'), col='red')
lines(wind_speed_m_s ~ by15, data=subset(sub, block=='W' & position=='middle'), col='blue')

# looks like fans were actually off from 12/5 9:30 until 12/6 13:00
ind <- with(stacked, by15 > as.POSIXct('2019-12-05 09:30', tz='GMT') &
              by15 < as.POSIXct('2019-12-06 13:00', tz='GMT'))
stacked$box_fans_on[ind] <- 'no'


# write wind data to file
write.csv(stacked, paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                      "data/wind_sensor_data/wind_15.csv", sep=""), row.names=FALSE)
# testing
# subsetting data
sub <- subset(comb, as.POSIXct(by15, tz='GMT') > as.POSIXct('2019-10-24 00:00:00', tz='GMT')
              & as.POSIXct(by15, tz='GMT') < as.POSIXct('2019-12-12 09:00:00', tz='GMT'))
plot(sub$w1_sb_m_s ~ sub$by15) #, ylim=c(0,0.02))
points(sub$w1_sm_m_s ~ sub$by15, col='blue')
points(sub$w1_st_m_s ~ sub$by15, col='red')



###_____________________line quantum sensor data__________________###
###_____________________line quantum sensor data__________________###

lq1 <- read.csv(paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/11_4_2019_east.csv", sep=""), sep=",", header=T)
    lq1$east_west <-'east'
lq2 <- read.csv(paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/11_4_2019_west.csv", sep=""), sep=",", header=T)
    lq2$east_west <- 'west'
lq3 <- read.csv(paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/11_19_2019_east.csv", sep=""), sep=",", header=T)
    lq3$east_west <- 'east'
lq4 <- read.csv(paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/11_19_2019_west.csv", sep=""), sep=",", header=T)
    lq4$east_west <- 'west'
lq5 <- read.csv(paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/12_5_2019_east.csv", sep=""), sep=",", header=T)
    lq5$east_west <- 'east'
lq6 <- read.csv(paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/12_5_2019_west.csv", sep=""), sep=",", header=T)
    lq6$east_west <- 'west'
lq7 <- read.csv(paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/12_12_2019_east.csv", sep=""), sep=",", header=T)
    lq7$east_west <- 'east'
lq8 <- read.csv(paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/12_12_2019_west.csv", sep=""), sep=",", header=T)
    lq8$east_west <- 'west'

lq_east <- rbind.data.frame(lq1[,c(2,5,13)], lq3[,c(2,5,13)], lq5[,c(2,5,13)], lq7[,c(2,5,13)])
lq_west <- rbind.data.frame(lq2[,c(2,5,13)], lq4[,c(2,5,13)], lq6[,c(2,5,13)], lq8[,c(2,5,13)])     

# set date and time to POSIXct without daylight savings (GMT)
lq_east$date_time <- as.POSIXct(lq_east$Date.and.Time, format = "%m/%d/%Y %l:%M:%S %p", tz = "GMT")
lq_west$date_time <- as.POSIXct(lq_west$Date.and.Time, format = "%m/%d/%Y %l:%M:%S %p", tz = "GMT")



# QAQC line quantum PAR ---------------------------------------------------

# data look ok; don't see any obviously bad data and both sensor track each other.
plot(lq_west$date_time, lq_west$Average.Below.PAR, type='l')
lines(lq_east$date_time, lq_east$Average.Below.PAR, col='red')


# cut into 15-min intervals.  # CHANGE HERE TO CHANGE TIME INTERVAL
lq_east$by15 <- lubridate::ceiling_date(lq_east$date_time, "15 minutes")   
lq_west$by15 <- lubridate::ceiling_date(lq_west$date_time, "15 minutes")   

# calculate mean mass for each 15-minute time group by scale number
lq_east_15 <- summaryBy(Average.Below.PAR ~ by15, data=lq_east, 
                  FUN=mean, na.rm=TRUE, keep.names=TRUE)
lq_west_15 <- summaryBy(Average.Below.PAR ~ by15, data=lq_west, 
                  FUN=mean, na.rm=TRUE, keep.names=TRUE)
colnames(lq_east_15) <- c('by15', 'line_PAR_east_umol_m2_s')
colnames(lq_west_15) <- c('by15', 'line_PAR_west_umol_m2_s')

# merge east and west dfs
comb <- merge(lq_east_15, lq_west_15, by='by15')


### --- Clay added this

# We need to adjust times; apparently the clock was set to MDT, so the clock goes back by
# an hour on 11/3 
ind <- with(comb, by15 <= '2019-11-04 02:00')
head(comb$by15[ind] )
comb$by15[ind] <- comb$by15[ind] - 3600
head(comb$by15[ind] )

# Sean had a note to not use data from WEST sensor on 11/4 between ca. 15:00 and 16:00 
sub = subset(comb, lubridate::date(by15)=='2019-11-04' )
plot(line_PAR_west_umol_m2_s ~ by15, type='l', sub)
lines(line_PAR_east_umol_m2_s ~ by15, type='l', sub, col='red')
ind <- with(comb, by15 >= '2019-11-04 14:45' & by15 <= '2019-11-04 16:15')
comb$line_PAR_west_umol_m2_s[ind] <- NA

# save to file
write.csv(comb, paste("/home/sean/github/2020_greenhouse/second_fall_experiment/",
                      "data/line_PAR_sensors/line_PAR_15.csv", sep=""), row.names=FALSE)
# testing
# sub <- subset(comb, as.POSIXct(by15, tz='GMT') > as.POSIXct('2019-11-01 00:00:00', tz='GMT')
#               & as.POSIXct(by15, tz='GMT') < as.POSIXct('2019-11-07 00:00:00', tz='GMT'))
# plot(sub$line_PAR_east_umol_m2_s ~ sub$by15, ylim=c(0,1000))
#   points(sub$line_PAR_west_umol_m2_s ~ sub$by15, col='red')


#--- END OF CODE


  















