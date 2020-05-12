
# setwd("/home/sean/github/2020_greenhouse/first_summer_experiment/figures")
# on U drive
# setwd("U:/Staff Folders/Gleason, Sean/sean_stuff/r_stuff/2020/greenhouse_2019/first_summer_experiment/figures")

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


# assign treatment start date/time. Irrigation started at 18:00, so I set for 1 hour after that. 
exp_start <- as.POSIXct('2019-08-24 19:00:00', tz='GMT')



###________________PAR, RH, soil_temp data_____________###
###________________PAR, RH, soil_temp data_____________###

rh1 <- read.table(paste("/home/sean/github/2020_greenhouse/first_summer_experiment//",
                        "data/RH_temp_PAR_logger_data/read_only/collected_8_23_2019.TXT", sep=""), header = F, sep = ",")
rh2 <- read.table(paste("/home/sean/github/2020_greenhouse/first_summer_experiment/",
                        "data/RH_temp_PAR_logger_data/read_only/collected_9_3_2019.TXT", sep=""), header = F, sep = ",")

rh <- rbind.data.frame(rh1, rh2)
colnames(rh) <- c('par1_n', 'par2_s', 'pyr1_n', 'pyr2_s', 'am2320_high_temp', 'am2320_high_rh', 'sht2_low_temp',
                  'sht2_low_rh', 'bmp_box_temp', 'bmp_box_atm_p', 'soil_t1', 'soil_t2', 'soil_t3', 'soil_t4', 'date', 'time')


# add POSIXct time
rh$date_time <- paste(rh$date, rh$time); rh$date_time[1:10]
rh$date_time <- as.POSIXct(rh$date_time, format = "%d/%m/%y %k:%M:%S", tz = "GMT")
rh$date_time[1:15]

# fix date
rh$date <- as.Date(rh$date, format = '%d/%m/%y')

# Subtract 1 hour to account for clock being set to MDT in experiment 1 but MST in experiment 2
# rh$date_time <- rh$date_time - 3600


# --- QA/QC

# PAR/PYR DATA
summary(rh[,c('par1_n','par2_s','pyr1_n','pyr2_s')])
# PAR (and pyr) data looks very noisy, even during daytime. See below, though, 15 min means look much better.
plot(rh$date_time, rh$par1_n)
plot(par1_n~date_time, rh[rh$date == '2019-08-25', ])
plot(rh$date_time, rh$par2_s)
plot(par2_s~date_time, rh[rh$date == '2019-08-25', ])


# Soil temp: Investigate strange "blips" in data, see below....

lims=c(min(rh[,c('soil_t1','soil_t2','soil_t3','soil_t4')], na.rm = T),
       max(rh[,c('soil_t1','soil_t2','soil_t3','soil_t4')], na.rm = T))
plot(rh$date_time, rh$soil_t1, type='p', ylim=lims)
points(rh$date_time, rh$soil_t2, col='red')
points(rh$date_time, rh$soil_t3, col='blue')
points(rh$date_time, rh$soil_t4, col='green')
abline(c(10,0))

# Looks like all of the outliers is either > 60 C or < 10 C, let's change bad values to NA
for(clm in c('soil_t1','soil_t2','soil_t3','soil_t4')) {
  ind <- !is.na(rh[[clm]]) & (rh[[clm]] > 60 | rh[[clm]] < 10)
  rh[ind, clm] <- NA
}

# plot again to check, looks good.
lims=c(min(rh[,c('soil_t1','soil_t2','soil_t3','soil_t4')], na.rm = T),
       max(rh[,c('soil_t1','soil_t2','soil_t3','soil_t4')], na.rm = T))
plot(rh$date_time, rh$soil_t1, type='p', ylim=lims)
points(rh$date_time, rh$soil_t2, col='red')
points(rh$date_time, rh$soil_t3, col='blue')
points(rh$date_time, rh$soil_t4, col='green')




# "cut" data into 15 minute groups and take the mean
rh$by15 <- lubridate::ceiling_date(rh$date_time, "15 minutes")   # CHANGE HERE TO CHANGE TIME INTERVAL
# calculate mean mass for each 15-minute time group by scale number
rh <- summaryBy(par1_n + par2_s + pyr1_n + pyr2_s + am2320_high_temp + am2320_high_rh + sht2_low_temp +
                  sht2_low_rh + bmp_box_temp + bmp_box_atm_p + soil_t1 + 
                  soil_t2 + soil_t3 + soil_t4 ~ by15, data=rh, FUN=mean, na.rm=TRUE, keep.names=TRUE)
rh <- rh[order(rh$by15),]




# QAQC 'rh' dataframe (15-minute data) -----------------------------------------------------

# PAR/PYR data look ok, using 15-min means.
plot(rh$by15, rh$par2_s, type='l')
lines(rh$by15, rh$par1_n, col='red')
plot(rh$by15, rh$pyr2_s, type='l')
lines(rh$by15, rh$pyr1_n, col='red')

# Air temp: looks ok, both sensors similar
plot(rh$by15, rh$bmp_box_temp, type='l', ylim=c(15, 45))
lines(rh$by15, rh$sht2_low_temp, col='red')
lines(rh$by15, rh$am2320_high_temp, col='blue')

# RH: looks ok, both sensors similar
plot(rh$by15, rh$sht2_low_rh, type='l')
lines(rh$by15, rh$am2320_high_rh, col='red')

# Soil temp
plot(rh$by15, rh$soil_t1, type='l')
lines(rh$by15, rh$soil_t2, col='red')
lines(rh$by15, rh$soil_t3, col='blue')
lines(rh$by15, rh$soil_t4, col='green')
# hmm some funky moments where apparetly the data logger had an issue....
# I'll fix these above, with raw data




# write rh data to file
write.csv(rh, paste("/home/sean/github/2020_greenhouse/first_summer_experiment/",
                    "data/RH_temp_PAR_logger_data/rh_15.csv", sep=""), row.names=FALSE)

# build another df with only temps, but all data in one column
# separate each soil sensor
# t1=west block; t2=east block; t3=also east block (i.e., t2 and t3 are the same treatment); t4=middle block
sen1 <- subset(rh, select=c(by15, soil_t1))
sen2 <- subset(rh, select=c(by15, soil_t2))
sen3 <- subset(rh, select=c(by15, soil_t3))
sen4 <- subset(rh, select=c(by15, soil_t4))


### CLAY TODO: NOT SURE ABOUT WHICH SENSORS GO TO WHICH BLOCK/TRT......

# take mean of soil sensors T2 and T3 because they are both in same block (treatment sequence)
sen2_3 <- merge(sen2, sen3, by='by15')
colnames(sen2_3) <- c('by15', 't2', 't3')
sen2_3$mean_temp <- apply(sen2_3[,c('t2','t3')], 1, mean, na.rm=TRUE)
sen2_3 <- subset(sen2_3, select=c(by15, mean_temp))

# CLAY: added "block" variable, to help with merge to other data sets
sen1$block <- 'D'; sen2_3$block <- 'W'; sen4$block <- 'M'

# change column name and add correct treatment to each date

# first soil sensor (T1) (in west block)
colnames(sen1) <- c('by15', 'soil_temp_C', 'block')
sen1$treatment <- 'full_drought'

# now, the second and third soil sensors (sen2_3 = mean of T2 and T3) (in east block)
colnames(sen2_3) <- c('by15', 'soil_temp_C', 'block')
sen2_3$treatment <- 'well_watered'

# lastly, the fourth soil sensor (T4) (in the middle block)
colnames(sen4) <- c('by15', 'soil_temp_C', 'block')
sen4$treatment <- 'moderate_drought' 

# rbind all sensor dataframes together
comb <- rbind.data.frame(sen1, sen2_3, sen4)

# check for missing data
any(is.na(comb))

# export to file
write.csv(comb, paste("/home/sean/github/2020_greenhouse/first_summer_experiment/",
                      "data/RH_temp_PAR_logger_data/soil_temp_15.csv", sep=""), row.names=FALSE)




###_______________ import wind data____________________###
###_______________ import wind data____________________###

wind1 <- read.table(paste("/home/sean/github/2020_greenhouse/first_summer_experiment/",
                          "data/wind_sensor_data/read_only/downloaded_8_23_2019.TXT", sep=""), header = F, sep = ",")
wind2 <- read.table(paste("/home/sean/github/2020_greenhouse/first_summer_experiment/",
                          "data/wind_sensor_data/read_only/downloaded_9_3_2019.TXT", sep=""),  header = F, sep = ",")

wind <- rbind.data.frame(wind1, wind2)
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
# wind <- wind[10:nrow(wind),]

# set date and time to POSIXct without daylight savings (GMT)
wind$date_time <- paste(wind$date, wind$time); wind$date_time[1:10]
wind$date_time <- as.POSIXct(wind$date_time, format = "%d/%m/%y %k:%M:%S", tz = "GMT")
wind$date_time[1:15]
# wind$date <- substring(wind$date_time, first=1, last=10); wind$date[1:5]
# wind$time <- substring(wind$date_time, first=12, last=19); wind$time[1:5]

# Subtract 1 hour to account for clock being set to MDT in experiment 1 but MST in experiment 2
wind$date_time <- wind$date_time - 3600

# "cut" data into 15 minute groups and take the mean
wind$by15 <- lubridate::ceiling_date(wind$date_time, "15 minutes")   # CHANGE HERE TO CHANGE TIME INTERVAL
# calculate mean mass for each 15-minute time group by scale number
wind <- summaryBy(w1_sb_m_s + w1_sm_m_s + w1_st_m_s + w2_sb_m_s + w2_sm_m_s + w2_st_m_s +
                    w3_sb_m_s + w3_sm_m_s + w3_st_m_s ~ by15, data=wind, FUN=mean, 
                  na.rm=TRUE, keep.names=TRUE)
wind <- wind[order(wind$by15),]

# to convert V to m s-1, import temperature from rh sensors
temp <- read.csv(paste("/home/sean/github/2020_greenhouse/first_summer_experiment/",
                       "data/RH_temp_PAR_logger_data/rh_15.csv", sep=""), header=TRUE, sep=",")
temp <- subset(temp, select=c(by15, sht2_low_temp))
temp$by15 <- as.character(temp$by15); wind$by15 <- as.character(wind$by15)
comb <- merge(wind, temp, by='by15', all.x=TRUE, all.y=FALSE)
comb$by15 <- as.POSIXct(comb$by15, tz='GMT')  # back to POSIXct

## CLAY NOTE:
# We only have temperature data starting on 8/23, so there are many rows of missing wind data after the merge.
# I won't worry about this though, since treatments didn't start until evening of 8/24 anyway.
comb <- comb[!is.na(comb$sht2_low_temp), ]


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

# ===== CLAY NOTE: These positions may or may not be correct, need to check Sean's notes.
# ==========''

# array1 
df1 <- comb[,c('by15', 'w1_sb_m_s')]; colnames(df1) <- c('by15', "wind_speed_m_s"); df1$position <- "bottom"
df2 <- comb[,c('by15', 'w1_sm_m_s')]; colnames(df2) <- c('by15', "wind_speed_m_s"); df2$position <- "middle"
df3 <- comb[,c('by15', 'w1_st_m_s')]; colnames(df3) <- c('by15', "wind_speed_m_s"); df3$position <- "top"
array1 <- rbind.data.frame(df1, df2, df3)
# array2   
df1 <- comb[,c('by15', 'w2_sb_m_s')]; colnames(df1) <- c('by15', "wind_speed_m_s"); df1$position <- "bottom"
df2 <- comb[,c('by15', 'w2_sm_m_s')]; colnames(df2) <- c('by15', "wind_speed_m_s"); df2$position <- "middle"
df3 <- comb[,c('by15', 'w2_st_m_s')]; colnames(df3) <- c('by15', "wind_speed_m_s"); df3$position <- "top"
array2 <- rbind.data.frame(df1, df2, df3)
# array3 
df1 <- comb[,c('by15', 'w3_sb_m_s')]; colnames(df1) <- c('by15', "wind_speed_m_s"); df1$position <- "bottom"
df2 <- comb[,c('by15', 'w3_sm_m_s')]; colnames(df2) <- c('by15', "wind_speed_m_s"); df2$position <- "middle"
df3 <- comb[,c('by15', 'w3_st_m_s')]; colnames(df3) <- c('by15', "wind_speed_m_s"); df3$position <- "top"
array3 <- rbind.data.frame(df1, df2, df3)


### Plot to see if positions look correct....
### CLAY: STILL NOT SURE. "TOP" NOT CONSISTENTLY GREATER THAN BOTTOM/MIDDLE.
ggplot(array1, aes(x=by15, y=wind_speed_m_s, color=position)) + geom_point() + geom_line() + ggtitle('array1')
ggplot(array2, aes(x=by15, y=wind_speed_m_s, color=position)) + geom_point() + geom_line() + ggtitle('array2')
ggplot(array3, aes(x=by15, y=wind_speed_m_s, color=position)) + geom_point() + geom_line() + ggtitle('array3')

array1 %>% group_by(position) %>% summarise(ws=mean(wind_speed_m_s))
array2 %>% group_by(position) %>% summarise(ws=mean(wind_speed_m_s))
array3 %>% group_by(position) %>% summarise(ws=mean(wind_speed_m_s))


# within each array, change column name and add correct treatment to each date

# array1 (east block)
array1$treatment <- "well_watered"
array1$block <- 'W' 

# array2 (middle block)
array2$treatment <- "moderate_drought"
array2$block <- 'M' 


# array3 (west block)
array3$treatment <- 'full_drought'
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


# --- Verdict: Wind data looks fine; I don't see any obvious outliers or problems.


# write wind data to file
write.csv(stacked, paste("/home/sean/github/2020_greenhouse/first_summer_experiment/",
                         "data/wind_sensor_data/wind_15.csv", sep=""), row.names=FALSE)
# testing
# subsetting data
# sub <- subset(comb, as.POSIXct(by15, tz='GMT') > as.POSIXct('2019-10-24 00:00:00', tz='GMT')
#               & as.POSIXct(by15, tz='GMT') < as.POSIXct('2019-12-12 09:00:00', tz='GMT'))
# plot(sub$w1_sb_m_s ~ sub$by15) #, ylim=c(0,0.02))
# points(sub$w1_sm_m_s ~ sub$by15, col='blue')
# points(sub$w1_st_m_s ~ sub$by15, col='red')




















