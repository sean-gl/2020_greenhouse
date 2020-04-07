 
setwd("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/figures")
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
exp1_end   <- as.POSIXct('2019-11-04 09:00:00', tz='GMT')

exp2_start <- as.POSIXct('2019-11-04 17:00:00', tz='GMT')
exp2_end   <- as.POSIXct('2019-11-27 09:00:00', tz='GMT')

exp3_start <- as.POSIXct('2019-11-27 17:00:00', tz='GMT')
exp3_end   <- as.POSIXct('2019-12-12 09:00:00', tz='GMT')




###____________________ import scale data________________###
###____________________ import scale data________________###

scale1 <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                          "data/scale_output/read_only/scale_output_W_M_12-09.csv", sep=""), header = T, sep = ",")
scale2 <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                          "data/scale_output/read_only/scale_output_D_12-09.csv", sep=""), header = T, sep = ",")

# if importing from U drive...
# import scale data
# scale1 <- read.csv("U:/Staff Folders/Gleason, Sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/data/scale_output/scale_output_W_M_12-09.csv",
#                  header = T, sep = ",")
#    colnames(scale1) <- c('scale_no', 'date_time', 'mass_g')
# scale2 <- read.csv("U:/Staff Folders/Gleason, Sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/data/scale_output/scale_output_D_12-09.csv",
#                   header = T, sep = ",")

colnames(scale1) <- c('scale_no', 'date_time', 'mass_g')
colnames(scale2) <- c('scale_no', 'date_time', 'mass_g')
scale <- rbind.data.frame(scale1, scale2)

# set date and time to POSIXct without daylight savings (GMT)
scale$date_time <- as.POSIXct(scale$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
scale$date <- substring(scale$date_time, first=1, last=10); scale$date[1:5]
scale$time <- substring(scale$date_time, first=12, last=19); scale$time[1:5]
# "cut" data into 15 minute groups
scale$by15 <- lubridate::ceiling_date(scale$date_time, "15 minutes")   # CHANGE HERE TO CHANGE TIME INTERVAL
# calculate mean mass for each 15-minute time group by scale number
crap <- summaryBy(mass_g ~ scale_no + by15, data =scale, FUN=mean, na.rm=TRUE)

# loop through and calculate mass differences between time periods... slow as shit
woo <- c()
for(i in 1:nrow(crap)) {
  if(i > 1) {
  sub1 <- crap[i,]
  sub2 <- crap[(i-1),]
  # sub1 <- crap[15,]
  # sub2 <- crap[14,]
  diff <- sub2$mass_g.mean - sub1$mass_g.mean
  date_time <- sub1$by15
  sub3 <- cbind.data.frame(sub1$scale_no, sub2$scale_no, diff, date_time)
  woo <- rbind.data.frame(woo, sub3)
  }
}

# clean so mass differences to not mix up scales
crap <- subset(woo, woo$`sub1$scale_no` == woo$`sub2$scale_no`)
crap <- subset(crap, select=c("sub2$scale_no", "diff", "date_time"))
colnames(crap) <- c('scale_no', 'mass_loss_15min', 'date_time')
scales <- crap

# test
scale1 <- subset(scales, scale_no == 1)
plot(scales$mass_loss_15min ~ scales$date_time)
points(scale1$mass_loss_15min ~ scale1$date_time, col='red')



###________________PAR, RH, soil_temp data_____________###
###________________PAR, RH, soil_temp data_____________###

rh1 <- read.table(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                        "data/RH_temp_PAR_logger_data/read_only/11_4_2019_v2.TXT", sep=""), header = F, sep = ",")
rh2 <- read.table(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                          "data/RH_temp_PAR_logger_data/read_only/11_18_2019.TXT", sep=""), header = F, sep = ",")
rh3 <- read.table(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
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

# "cut" data into 15 minute groups and take the mean
rh$by15 <- lubridate::ceiling_date(rh$date_time, "15 minutes")   # CHANGE HERE TO CHANGE TIME INTERVAL
# calculate mean mass for each 15-minute time group by scale number
rh <- summaryBy(par1_n + par2_s + pyr1_n + pyr2_s + am2320_high_temp + am2320_high_rh + sht1_high_temp + 
                  sht1_high_rh + sht2_low_temp + sht2_low_rh + bmp_box_temp + bmp_box_atm_p + soil_t1 + 
                  soil_t2 + soil_t3 + soil_t4 ~ by15, data=rh, FUN=mean, na.rm=TRUE, keep.names=TRUE)
rh <- rh[order(rh$by15),]

# write rh data to file
write.csv(rh, paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
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
# change column name and add correct treatment to each date
# first soil sensor (T1) (in west block)
df <- sen1
  colnames(df) <- c('by15', 'soil_temp_C')
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp1_start & 
                          as.POSIXct(df$by15, tz='GMT') < exp1_end, "full_drought", "woo")
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp2_start & 
                          as.POSIXct(df$by15, tz='GMT') < exp2_end, "well_watered", df$treat)
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp3_start & 
                          as.POSIXct(df$by15, tz='GMT') < exp3_end, "full_drought", df$treat)
  df <- subset(df, treatment == 'full_drought' | treatment == 'well_watered' |
                  treatment == 'moderate_drought' | treatment == 'virgin_drought') # remove "woos"
sen1 <- df

# now, the second and third soil sensors (sen2_3 = mean of T2 and T3) (in east block)
df <- sen2_3
  colnames(df) <- c('by15', 'soil_temp_C')
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp1_start & 
                           as.POSIXct(df$by15, tz='GMT') < exp1_end, "well_watered", "woo")
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp2_start & 
                           as.POSIXct(df$by15, tz='GMT') < exp2_end, "full_drought", df$treat)
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp3_start & 
                           as.POSIXct(df$by15, tz='GMT') < exp3_end, "virgin_drought", df$treat)
  df <- subset(df, treatment == 'full_drought' | treatment == 'well_watered' |
                 treatment == 'moderate_drought' | treatment == 'virgin_drought') # remove "woos"
sen2_3 <- df
  
# lastly, the fourth soil sensor (T4) (in the middle block)
df <- sen4
  colnames(df) <- c('by15', 'soil_temp_C')
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp1_start & 
                         as.POSIXct(df$by15, tz='GMT') < exp1_end, "moderate_drought", "woo")
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp2_start & 
                         as.POSIXct(df$by15, tz='GMT') < exp2_end, "moderate_drought", df$treat)
  df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp3_start & 
                         as.POSIXct(df$by15, tz='GMT') < exp3_end, "well_watered", df$treat)
  df <- subset(df, treatment == 'full_drought' | treatment == 'well_watered' |
               treatment == 'moderate_drought' | treatment == 'virgin_drought') # remove "woos"
sen4 <- df  

# rbind all sensor dataframes together
comb <- rbind.data.frame(sen1, sen2_3, sen4)
# export to file
write.csv(comb, paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
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

wind1 <- read.table(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                          "data/wind_sensor_data/read_only/11_4_2019.TXT", sep=""), header = F, sep = ",")
wind2 <- read.table(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                          "data/wind_sensor_data/read_only/11_18_2019.TXT", sep=""),  header = F, sep = ",")
wind3 <- read.table(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
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
temp <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
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
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp1_start & 
                         as.POSIXct(df$by15, tz='GMT') < exp1_end, "well_watered", "woo")
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp2_start & 
                         as.POSIXct(df$by15, tz='GMT') < exp2_end, "full_drought", df$treat)
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp3_start & 
                         as.POSIXct(df$by15, tz='GMT') < exp3_end, "virgin_drought", df$treat)
df <- subset(df, treatment == 'full_drought' | treatment == 'well_watered' |
               treatment == 'moderate_drought' | treatment == 'virgin_drought') # remove "woos"
array1 <- df

# array2 (middle block)
df <- array2
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp1_start & 
                         as.POSIXct(df$by15, tz='GMT') < exp1_end, "moderate_drought", "woo")
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp2_start & 
                         as.POSIXct(df$by15, tz='GMT') < exp2_end, "moderate_drought", df$treat)
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp3_start & 
                         as.POSIXct(df$by15, tz='GMT') < exp3_end, "well_watered", df$treat)
df <- subset(df, treatment == 'full_drought' | treatment == 'well_watered' |
               treatment == 'moderate_drought' | treatment == 'virgin_drought') # remove "woos"
array2 <- df

# array3 (west block)
df <- array3
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp1_start & 
                         as.POSIXct(df$by15, tz='GMT') < exp1_end, "full_drought", "woo")
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp2_start & 
                         as.POSIXct(df$by15, tz='GMT') < exp2_end, "well_watered", df$treat)
df$treatment <- ifelse(as.POSIXct(df$by15, tz='GMT') > exp3_start & 
                         as.POSIXct(df$by15, tz='GMT') < exp3_end, "full_drought", df$treat)
df <- subset(df, treatment == 'full_drought' | treatment == 'well_watered' |
               treatment == 'moderate_drought' | treatment == 'virgin_drought') # remove "woos"
array3 <- df

# rbind all array dfs together
stacked <- rbind.data.frame(array1, array2, array3)

# write wind data to file
write.csv(stacked, paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
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

lq1 <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/11_4_2019_east.csv", sep=""), sep=",", header=T)
    lq1$east_west <-'east'
lq2 <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/11_4_2019_west.csv", sep=""), sep=",", header=T)
    lq2$east_west <- 'west'
lq3 <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/11_19_2019_east.csv", sep=""), sep=",", header=T)
    lq3$east_west <- 'east'
lq4 <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/11_19_2019_west.csv", sep=""), sep=",", header=T)
    lq4$east_west <- 'west'
lq5 <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/12_5_2019_east.csv", sep=""), sep=",", header=T)
    lq5$east_west <- 'east'
lq6 <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/12_5_2019_west.csv", sep=""), sep=",", header=T)
    lq6$east_west <- 'west'
lq7 <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/12_12_2019_east.csv", sep=""), sep=",", header=T)
    lq7$east_west <- 'east'
lq8 <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                        "data/line_PAR_sensors/read_only/12_12_2019_west.csv", sep=""), sep=",", header=T)
    lq8$east_west <- 'west'

lq_east <- rbind.data.frame(lq1[,c(2,5,13)], lq3[,c(2,5,13)], lq5[,c(2,5,13)], lq7[,c(2,5,13)])
lq_west <- rbind.data.frame(lq2[,c(2,5,13)], lq4[,c(2,5,13)], lq6[,c(2,5,13)], lq8[,c(2,5,13)])     

# set date and time to POSIXct without daylight savings (GMT)
lq_east$date_time <- as.POSIXct(lq_east$Date.and.Time, format = "%m/%d/%Y %l:%M:%S %p", tz = "GMT")
lq_west$date_time <- as.POSIXct(lq_west$Date.and.Time, format = "%m/%d/%Y %l:%M:%S %p", tz = "GMT")

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

# save to file
write.csv(comb, paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment",
                      "/data/line_PAR_sensors/line_PAR_15.csv", sep=""), row.names=FALSE)
# testing
sub <- subset(comb, as.POSIXct(by15, tz='GMT') > as.POSIXct('2019-11-01 00:00:00', tz='GMT')
              & as.POSIXct(by15, tz='GMT') < as.POSIXct('2019-11-07 00:00:00', tz='GMT'))
plot(sub$line_PAR_east_umol_m2_s ~ sub$by15, ylim=c(0,1000))
  points(sub$line_PAR_west_umol_m2_s ~ sub$by15, col='red')


  
###________________pressure bomb______________________###
###________________pressure bomb______________________###  

pb <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                      "data/pressure_bomb/read_only/pressurebomb_greenhouse2019.csv", sep=""), sep=",", header=T)
# add POSIXct time
pb$date_time <- as.POSIXct(pb$datetime, format = "%m/%d/%Y %H:%M:%S", tz = "GMT")
# round to nearest quarter hour
pb$by15 <- lubridate::round_date(pb$date_time, "15 minutes")  
# rename 
colnames(pb) <- c('data_ok', 'datetime', 'person', 'plant_id', 'treatment', 'psi_MPa', 'side_of_greenhouse',
                  'diameter_at_base', 'percent_dead', 'notes', 'date_time', 'by15')
# reorganize columns
pb <- subset(pb, select=c('by15', 'psi_MPa', 'plant_id', 'treatment', 'side_of_greenhouse', 'diameter_at_base',
                          'percent_dead', 'data_ok', 'notes'))

# update treatment labels to new ones
pb$treatment <- ifelse(pb$treatment == "dry", "full_drought", as.character(pb$treatment)); pb$treatment
pb$treatment <- ifelse(pb$treatment == "wet", "well_watered", as.character(pb$treatment))
pb$treatment <- ifelse(pb$treatment == "moderate", "moderate_drought", as.character(pb$treatment))
pb$treatment <- ifelse(pb$treatment == "dry_virgin", "virgin_drought", as.character(pb$treatment))
unique(pb$treatment)

# save to file
write.csv(pb, paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                    "data/pressure_bomb/pressure_bomb_15.csv", sep=""), row.names=FALSE)



####_______________________combine data to create continous PSI_leaf data______________###
####_______________________combine data to create continous PSI_leaf data______________###
####_______________________combine data to create continous PSI_leaf data______________###

# import data
# pressure data
pb <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                      "data/pressure_bomb/pressure_bomb_15.csv", sep=""), header = T, sep = ",")
# RH, PAR, soil temp
rh <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                     "data/RH_temp_PAR_logger_data/rh_15.csv", sep=""), header = T, sep = ",")
    rh <- subset(rh, select = -c(soil_t1, soil_t2, soil_t3, soil_t4))
s_temp <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                         "data/RH_temp_PAR_logger_data/soil_temp_15.csv", sep=""), header = T, sep = ",")
# line quantum sensor data
lq <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                     "data/line_PAR_sensors/line_PAR_15.csv", sep=""), header = T, sep = ",")
# wind data 
wind <- read.csv(paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
                     "data/wind_sensor_data/wind_15.csv", sep=""), header = T, sep = ",")

# combine by 15-min date and time
# first, merge data that include water treatments (e.g., not rh, air_temp, etc)
comb <- merge(wind, s_temp, by=c('by15', 'treatment'), all=TRUE); nrow(comb)
comb <- merge(comb, pb, by=c('by15', 'treatment'), all=TRUE); nrow(comb)
comb <- merge(comb, lq, by='by15', all=TRUE); nrow(comb)
comb <- merge(comb, rh, by='by15', all=TRUE); nrow(comb)

# save to file
# write.csv(comb, paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
#                      "data/rh_par_wind_psiLeaf_soilTemp_airTemp_15.csv", sep=""), row.names=FALSE)

df <- comb

# include only data when there are pressure bomb measurements 
df <- subset(df, complete.cases(comb$psi_MPa)); nrow(df)

# add machine learning and lasso libraries
library(caret)
library(randomForest)
library(glmnet)

# first, run simple linear mr with a few key variables...
m1 <- lm(df$psi_MPa ~ df$wind_speed_m_s + df$soil_temp_C + df$line_PAR_east_umol_m2_s + df$am2320_high_temp +
           df$am2320_high_rh + df$sht1_high_temp + df$sht1_high_rh + df$bmp_box_atm_p)
summary(m1)


###___________ lasso regression ______________####
###___________ lasso regression ______________####

# first subset to all possible independent variables and dependent variable
df2 <- subset(df, select=c("by15", "treatment", "position", "psi_MPa", 
                            "wind_speed_m_s", "soil_temp_C", "psi_MPa", 
                            "line_PAR_east_umol_m2_s", "line_PAR_west_umol_m2_s",
                            "par1_n", "par2_s", "pyr1_n", "pyr2_s", "am2320_high_temp", 
                            "am2320_high_rh", "sht1_high_temp", "sht1_high_rh", 
                            "sht2_low_temp", "sht2_low_rh", "bmp_box_temp", "bmp_box_atm_p")); nrow(df2)

# subset to... whatever...
df2 <- subset(df2, df$treatment == "well_watered")  ## CHANGE TREATMENT HERE...   

# there can't be any missing values
df2 <- subset(df2, complete.cases(df2)); nrow(df2)

# matrix "testvar" should include all possible predictor variables
testvar <- subset(df2, select=c("wind_speed_m_s", "soil_temp_C",  
                                "line_PAR_east_umol_m2_s", "line_PAR_west_umol_m2_s",
                                "par1_n", "par2_s", "pyr1_n", "pyr2_s", "am2320_high_temp", 
                                "am2320_high_rh", "sht1_high_temp", "sht1_high_rh", 
                                "sht2_low_temp", "sht2_low_rh", "bmp_box_temp", "bmp_box_atm_p")); nrow(testvar)
testvar <- as.matrix(scale(testvar)); nrow(testvar) # standardize matrix "testvar"

# give a dependent variable as a matrix
dep <- as.matrix(scale(df2$psi_MPa)); length(dep)
is.matrix(testvar); is.matrix(dep)

# "alpha" = lasso.  
fitY = glmnet(testvar, dep, family='gaussian', alpha=1, standardize=F) #already standardized  
plot(fitY, xvar="lambda") # plot coefficients against lambda value
plot(fitY, xvar="dev")    # plot coefficients against fraction of deviance explained
coef(fitY)  # list all coefficients with increasing values of lambda
# now... to choose an appropriate value of lambda run k-fold cross-validation
# note that this will give you slightly different distributions each time it is run
# the number of variables incldued in the model are across the top of the plot... and lambda across the bottom
# we want a model that minimizes MSE (y axis), but is also regulated to avoid over-fitting
# the dotted vertical line to the right is the value of lambda (and numer of fitted variables)
# that give you MSE that is within one SE of the mean... as a way to apply regularization
# as such the methods uses "k-fold" cross validation to select a lambda vallue that is within
# 1 SE of the minimum SE... which is somewhat arbitrary... but a different value could 
# be chosen to aid interpretation, i.e., for a biological reason...
# you can see that the 4-parameter model is nearly always within 1SE of the mean MSE
CV = cv.glmnet(testvar, dep, family='gaussian', alpha=1, standardize=F); plot(CV)

# refit "fitY" using lambda value that is 1 SE of model giving minimum MSE
fitY = glmnet(testvar, dep, family='gaussian', alpha=1, standardize=F, lambda=(CV$lambda.1se))  
coef(fitY)
summary(fitY)

# now include the selected variables into a linear model  
# model with one parameter
mod1 <- lm(dep ~ testvar[,"sht1_high_temp"]) 
summary(mod1); mse1 <- mean(mod1$residuals^2); mse1; extractAIC(mod1)
  plot(dep ~ testvar[,"sht1_high_temp"])
  plot(dep ~ testvar[,"pyr2_s"])
  plot(dep ~ testvar[,"sht1_high_rh"])

# model with two parameters
mod1 <- lm(dep ~ testvar[,"sht1_high_temp"] + testvar[,"pyr2_s"]) 
summary(mod1); mse1 <- mean(mod1$residuals^2); mse1; extractAIC(mod1)

# model with three parameters
mod1 <- lm(dep ~ testvar[,"sht1_high_temp"] + testvar[,"pyr2_s"] + testvar[,"sht1_high_rh"]) 
summary(mod1); mse1 <- mean(mod1$residuals^2); mse1; extractAIC(mod1)

# model with four parameters
mod1 <- lm(dep ~ testvar[,"sht1_high_temp"] + testvar[,"pyr2_s"] + testvar[,"sht1_high_rh"] + 
                      testvar[,"par1_n"]) 
summary(mod1); mse1 <- mean(mod1$residuals^2); mse1; extractAIC(mod1)



###___________ random forest ______________####
###___________ random forest ______________####

df <- comb

# include only data when there are pressure bomb measurements 
df <- subset(df, complete.cases(comb$psi_MPa)); nrow(df)

# first subset to all possible independent variables and dependent variable
df2 <- subset(df, select=c("by15", "treatment", "position", "psi_MPa", 
                           "wind_speed_m_s", "soil_temp_C", 
                           "line_PAR_east_umol_m2_s", "line_PAR_west_umol_m2_s",
                           "par1_n", "par2_s", "pyr1_n", "pyr2_s", "am2320_high_temp", 
                           "am2320_high_rh", "sht1_high_temp", "sht1_high_rh", 
                           "sht2_low_temp", "sht2_low_rh", "bmp_box_temp", "bmp_box_atm_p")); nrow(df2)

# subset to... whatever...
 df2 <- subset(df2, df$treatment == "well_watered")  ## CHANGE TREATMENT HERE...   

# there can't be any missing values
df2 <- subset(df2, complete.cases(df2)); nrow(df2)

# subset into train and test dfs
train_data <- df2[1:(nrow(df2)/2),]
test_data <- df2[(nrow(df2)/2):nrow(df2),]


# training model... will take some time to run...
model <- train(psi_MPa ~ wind_speed_m_s + soil_temp_C +  
              line_PAR_east_umol_m2_s + line_PAR_west_umol_m2_s + par1_n +
              par2_s + pyr1_n + pyr2_s + am2320_high_temp + am2320_high_rh +
              sht1_high_temp + sht1_high_rh + sht2_low_temp + sht2_low_rh +
              bmp_box_temp + bmp_box_atm_p,
              data = train_data, 
              method = 'rf', # Use the 'random forest' algorithm
              trControl = trainControl(method = 'cv', # Use cross-validation
                                      number = 5),
              importance=TRUE) # Use 5 folds for cross-validation
model
varImp(model)

# now run on test data
test_data$rand_forest_pred <- predict(model, newdata = test_data)

plot(test_data$psi_MPa ~ test_data$rand_forest_pred)
SMA <- sma(test_data$psi_MPa ~ test_data$rand_forest_pred); summary(SMA); plot(SMA)
















