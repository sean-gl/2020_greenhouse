# Clay Bliss
# Script to develop model of leaf area based on non-plant measurement data

rm(list=ls())
require(ggplot2)
require(plyr)
require(lubridate)
require(tidyr)
require(dplyr)

### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')


# read in Arduino temperature/RH/PAR data
arduino <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/RH_temp_PAR_logger_data/rh_15.csv',
                  stringsAsFactors = FALSE)
arduino$by15 <- as.POSIXct(arduino$by15, tz='GMT')
arduino$date <- date(arduino$by15)


# omit 1st and last day since we don't have full 24-hours of data (needed to calculate GDD)
arduino <- subset(arduino, !date %in% as.Date(c('2019-10-21', '2019-12-12')))

# read in Argus (greenhouse system) temperature/irradiance data
argus <- read.csv('/home/sean/github/2020_greenhouse/A3 temp light 2019.csv', stringsAsFactors = F)
colnames(argus) <- c('by15', 'Argus_temp_C', 'Argus_irradiance_W_m2')
argus$by15 <- gsub(',', '', argus$by15)
argus$by15 <- as.POSIXct(argus$by15, format='%Y/%m/%d %H:%M:%S', tz='GMT')
argus$date <- date(argus$by15)
# convert F to C
argus$Argus_temp_C <- (argus$Argus_temp_C - 32) * 5/9

# sort and check for duplicate rows
argus <- argus[order(argus$by15), ]
dups <- duplicated(argus)
argus <- argus[!dups, ]

# omit 1 row without data
ind <- which(!complete.cases(argus)); ind
argus <- argus[-ind, ]

# TODO: Note odd upward-shift in temps. around 2nd week of Sept....I don't think we have Arduino data for this period, either.
plot(argus$by15, argus$Argus_temp_C, type = 'p')

# TODO: Hmm why is iradiance so low in much of July/August? Seems like the data must be bad.
plot(argus$by15, argus$Argus_irradiance_W_m2, type = 'p')

# merge arduino & argus data
tempDat <- merge(arduino, argus, by=c('date', 'by15'), all = TRUE)

# look at correlations between argus, arduino

# Argus temp is highly correlated with both high and low sensors
cor(tempDat$Argus_temp_C, tempDat$sht1_high_temp, use='complete.obs')
plot(tempDat$Argus_temp_C, tempDat$sht1_high_temp); abline(c(0,1))
cor(tempDat$Argus_temp_C, tempDat$sht2_low_temp, use='complete.obs')
plot(tempDat$Argus_temp_C, tempDat$sht2_low_temp); abline(c(0,1))

# correlation with pyr/par not as good but still pretty strong, esp when not really low.
cor(tempDat$Argus_irradiance_W_m2, tempDat$pyr1_n, use='complete.obs')
plot(tempDat$Argus_irradiance_W_m2, tempDat$pyr1_n); abline(c(0,1))
cor(tempDat$Argus_irradiance_W_m2, tempDat$pyr2_s, use='complete.obs')
plot(tempDat$Argus_irradiance_W_m2, tempDat$pyr2_s); abline(c(0,1))
cor(tempDat$Argus_irradiance_W_m2, tempDat$par1_n, use='complete.obs')
plot(tempDat$Argus_irradiance_W_m2, tempDat$par1_n); abline(c(0,1))


# calculate daily max/min temperatures at 75% canopy height ("low") and ~1m above canopy("high")
tempSummary <- tempDat %>% 
  group_by(date) %>% 
  summarise(max_temp_high = max(sht1_high_temp), min_temp_high = min(sht1_high_temp),
            max_temp_low = max(sht2_low_temp), min_temp_low = min(sht2_low_temp),
            max_temp_argus = max(Argus_temp_C), min_temp_argus = min(Argus_temp_C))

summary(tempSummary)

# Max temp: Low looks like better fit to 1:1 line
cor(tempSummary$max_temp_argus, tempSummary$max_temp_low, use = 'complete.obs')
cor(tempSummary$max_temp_argus, tempSummary$max_temp_high, use = 'complete.obs')
plot(tempSummary$max_temp_argus, tempSummary$max_temp_low); abline(c(0,1))
plot(tempSummary$max_temp_argus, tempSummary$max_temp_high); abline(c(0,1))

# Min temp: Low looks like better correlation with Argus, too (but poorer fit of 1:1 line)
# Note the range is much smaller than max temps.
cor(tempSummary$min_temp_argus, tempSummary$min_temp_low, use = 'complete.obs')
cor(tempSummary$min_temp_argus, tempSummary$min_temp_high, use = 'complete.obs')
plot(tempSummary$min_temp_argus, tempSummary$min_temp_low); abline(c(0,1))
plot(tempSummary$min_temp_argus, tempSummary$min_temp_high); abline(c(0,1))


#### So, looks like we can either substitute (or model) the arduino (sht2_low_temp) sensor measurements
# using the argus temperature measurements. For calculation of GDD it is probably sufficient
# to just substitute them in. 



# now calculate simple mean temp using daily max/mean
tempSummary$mean_temp_argus <- rowMeans(tempSummary[,c('max_temp_argus','min_temp_argus')])
tempSummary$mean_temp_high <- rowMeans(tempSummary[,c('max_temp_high','min_temp_high')])
tempSummary$mean_temp_low <- rowMeans(tempSummary[,c('max_temp_low','min_temp_low')])
summary(tempSummary)

# Mean temps: argus fits more closely to low than high
cor(tempSummary$mean_temp_argus, tempSummary$mean_temp_low, use = 'complete.obs')
plot(tempSummary$mean_temp_argus, tempSummary$mean_temp_low); abline(c(0,1))
cor(tempSummary$mean_temp_argus, tempSummary$mean_temp_high, use = 'complete.obs')
plot(tempSummary$mean_temp_argus, tempSummary$mean_temp_high); abline(c(0,1))

# Use only Argus temps
tempSummary <- select(tempSummary, date, contains('argus'))
names(tempSummary) <- gsub('_argus', '', names(tempSummary))

# calculate GDD (note: T_base = 50 F = 10 C)
tempSummary$gdd <- ifelse(tempSummary$max_temp > 10, tempSummary$mean_temp - 10, 0)

# calculate modified GDD
tempSummary$max_temp_mod <- ifelse(tempSummary$max_temp > 30, 30, tempSummary$max_temp)
tempSummary$min_temp_mod <- ifelse(tempSummary$min_temp < 10, 10, tempSummary$min_temp)
tempSummary$mean_temp_mod <- rowMeans(tempSummary[,c('max_temp_mod','min_temp_mod')])
tempSummary$mgdd <- ifelse(tempSummary$max_temp_mod > 10, tempSummary$mean_temp_mod - 10, 0)
summary(tempSummary)
plot(tempSummary$date, tempSummary$gdd, type='l')
lines(tempSummary$date, tempSummary$mgdd, type='l', col='red')


# Subset to columns of interest in modeling leaf area
# Also subset to dates after planting in 2nd experiment
tempSummaryExp2 <- subset(tempSummary, date >= '2019-09-09', 
                          select = c(date, gdd, mgdd, max_temp, mean_temp))

# Extend data back in time, using mean value
# missingdates <- c(seq.Date(as.Date('2019-09-09'), as.Date('2019-10-23'), by=1),
#                   as.Date('2019-12-12'))
# missingdates <- data.frame(date=missingdates, 
#                            mgdd_low=mean(tempSummaryExp2$mgdd_low),
#                            mgdd_high=mean(tempSummaryExp2$mgdd_high),
#                            gdd_low=mean(tempSummaryExp2$gdd_low),
#                            gdd_high=mean(tempSummaryExp2$gdd_high))
# tempSummaryExp2 <- merge(tempSummaryExp2, missingdates, all = TRUE)



# add cumsum columns
tempSummaryExp2$gdd_cumsum <- cumsum(tempSummaryExp2$gdd)
tempSummaryExp2$mgdd_cumsum <- cumsum(tempSummaryExp2$mgdd)
tempSummaryExp2$mean_temp_cummean <- cummean(tempSummaryExp2$mean_temp)
tempSummaryExp2$max_temp_cummean <- cummean(tempSummaryExp2$max_temp)

# tempSummaryExp2$mgdd_low_cumsum <- cumsum(tempSummaryExp2$mgdd_low)
# tempSummaryExp2$mgdd_high_cumsum <- cumsum(tempSummaryExp2$mgdd_high)
# tempSummaryExp2$gdd_low_cumsum <- cumsum(tempSummaryExp2$gdd_low)
# tempSummaryExp2$gdd_high_cumsum <- cumsum(tempSummaryExp2$gdd_high)



# # Read in MASTER data set 15-min
comb <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/data/combined_data/combdat_plant_level.rds')
comb$date <- date(comb$by15)


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

ggplot(irrigDat, aes(x=date, y=stress_index, color=block)) + geom_line()


# # get mean leaf area by date & treatment
# # NOTE: The mean() function is used for convenience, this is really just tabling by date.
# la <- comb %>% group_by(date, treatment, block) %>% 
#   summarise(mean_leaf_area_m2 = mean(mean_plant_leaf_area_m2, na.rm=T))


# Read in leaf area data (prepped). Use only data on dates which leaf area was directly measrured,
# or leaf width/length were measured (and leaf area predicted from them.)
la <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/total_plant_leaf_area.rds')

# drop virgin plants, they have no analoge in 1st experiment
la <- la[la$block!='V',]


# x = la %>% group_by(date, treatment) %>% summarize(mn = mean(total_leaf_area_cm2))
# ggplot(x, aes(x=date, y=mn, color=treatment)) + geom_line() + geom_point()
# 
# x = la %>% group_by(date, block) %>% summarize(mn = mean(total_leaf_area_cm2))
# ggplot(x, aes(x=date, y=mn, color=block)) + geom_line() + geom_point()


## TODO: The treatment column here is not quite right, esp. for the last date (12/12). 
## Might be better to add an irrigation colum (could get from merging with 'comb' above), and could
## then even calculate a cummulative irrigation. This should be as/more informative as 'treatment'
## An easier fix might be to just fix the treatment column (but then we only have n=1 day of virgin plants)
table(la$date, la$treatment, la$block)

# Subset leaf area to roughly the same # of days since planting as in 1st experiment
as.Date('2019-09-03') - as.Date('2019-07-11') # 55 days in 1st exp.
la$day <- as.numeric(la$date - as.Date('2019-09-09')) + 1

# I'll use the first 2 leaf area dates (the first 59 days) to model leaf area growth in each treatment
la <- la[la$day <= 59, ]

# For 1st 2 measurements only! Ignore 2nd treatment since it had no time to take effect.
# change treatment to contrast full_drought vs. moderate/no drought.
# (if we keep all 3 treatments, there are no significant diffs between them in regression.)
la$treatment[la$block=='D'] <- 'dry'
la$treatment[la$block!='D'] <- 'wet'

# change to m2
la$total_leaf_area_m2 <- la$total_leaf_area_cm2 / 1e4
la$total_leaf_area_cm2 <- NULL


ggplot(la, aes(x=day, y=total_leaf_area_m2, color = block)) + geom_point() +
  geom_smooth(method = 'lm')

# # add zero points
# grid <- unique(la[,c('block','pot_id','treatment')])
# grid$total_leaf_area_m2 <- 0
# grid$day <- 1
# grid$date <- as.Date('2019-09-09')
# la <- rbind(grid, la)

# merge leaf area to irrigation data
# la <- merge(la[ , c('day','date','block','total_leaf_area_m2')],
#              comb[ , c('block','treatment','date','irrig','irrig_cumsum','irrig_cummean',
#                        'stress_index')], 
#              by=c('date','block'), all.x = TRUE)
# la <- unique(la)

# merge to temp summary
la <- merge(la, tempSummaryExp2, by='date', all.x = TRUE)





summary(lm(total_leaf_area_m2 ~ day, la))
summary(lm(total_leaf_area_m2 ~ mgdd_cumsum, la))
summary(lm(total_leaf_area_m2 ~ day + treatment, la))
summary(lm(total_leaf_area_m2 ~ mgdd_cumsum + treatment, la))

# SAve the best model to use in first experiment
summary(lm(total_leaf_area_m2 ~ max_temp_cummean + block, la))
summary(lm(total_leaf_area_m2 ~ mean_temp_cummean, la))

summary(lm(total_leaf_area_m2 ~ day, la))
summary(lm(total_leaf_area_m2 ~ mgdd_cumsum , la))
summary(lm(total_leaf_area_m2 ~ mgdd_cumsum + irrig_cummean, la))

summary(lm(total_leaf_area_m2 ~ poly(mgdd_cumsum,1) + irrig_cumsum, la))


# looks like no treatment differences; however, treatments changed
summary(lm(total_leaf_area_m2 ~ day + irrig_cummean, la))
summary(lm(total_leaf_area_m2 ~ day + irrig_cumsum, la))
summary(lm(total_leaf_area_m2 ~ day, la))

summary(lm(total_leaf_area_m2 ~ day, la))

# examine, looks ok
ggplot(la, aes(x=date, y=total_leaf_area_m2, color=block)) + geom_point() # + geom_smooth(method = 'lm')
ggplot(la, aes(x=date, y=total_leaf_area_m2, color=treatment)) + geom_point()
# looks like no treatment differences; however, treatments changed
summary(lm(total_leaf_area_m2 ~ day + treatment, la))
summary(lm(total_leaf_area_m2 ~ day, la))

# merge temp data to LA data, keep only rows with LA
alldat <- merge(tempSummaryExp2, la, by='date', all.y = TRUE)




# Doesn't matter which gdd or mgdd, high or low. R2 = 0.33
summary(lm(total_leaf_area_m2 ~ gdd_cumsum , alldat ))

summary(lm(total_leaf_area_m2 ~ mgdd_cumsum, alldat ))


# add quadratic, r2=0.54
summary(lm(total_leaf_area_m2 ~ poly(day, 2), alldat ))
summary(lm(total_leaf_area_m2 ~ poly(gdd_cumsum, 2), alldat ))

# add cum irrigation
summary(lm(total_leaf_area_m2 ~ poly(mgdd_cumsum, 2) + treatment, alldat ))
summary(lm(total_leaf_area_m2 ~ poly(gdd_cumsum, 2) + irrig_cumsum, alldat ))

ggplot(alldat, aes(x=mgdd_cumsum, y=total_leaf_area_m2, color=treatment)) + geom_point() 

#  However, "days since planting" predicts as well as GDD, since they are perfectly linearly related.
alldat$day <- alldat$date - as.Date('2019-09-09') + 1
summary(lm(gdd_cumsum ~ day, alldat ))
summary(lm(total_leaf_area_m2 ~ poly(day, 2) + treatment, alldat))


## NOTE: Tempearture data only available from 8/23 to 8/31 for 1st experiment
# seeds planted on 7/11.



### ---- I was playing around with using weather-station data here, probably not worth the time....
# 
# clim <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/FC_climate_data_NOAA.csv',
#                  stringsAsFactors = F)
# 
# 
# # filter out dry bulb temp
# clim <- subset(clim, DailyMaximumDryBulbTemperature != '' &
#                   DailyMinimumDryBulbTemperature != '' &
#                   DailyAverageDryBulbTemperature != '', 
#                 select = c('DATE','DailyMaximumDryBulbTemperature','DailyMinimumDryBulbTemperature',
#                            'DailyAverageDryBulbTemperature'))
# 
# # convert date
# names(clim)[names(clim) =='DATE'] <- 'date'
# clim$date <- as.Date(substr(clim$date, 1, 10))
# 
# # change temps to numeric and convert to celcius
# # NOTE: some values have an "s" and these become NA, not sure but probably a flag.
# for(i in c('DailyMaximumDryBulbTemperature','DailyMinimumDryBulbTemperature', 'DailyAverageDryBulbTemperature')) {
#   clim[[i]] <- as.numeric(clim[[i]])
#   clim[[i]] <- (clim[[i]] - 32) * 5/9
# }
# 
# length(which(complete.cases(clim)))
# 
# 
# # merge climate data to greenhouse temp data
# allTemp <- merge(tempSummary, clim, by='date', all = TRUE)
# 
# plot(allTemp$max_temp_high, allTemp$DailyMaximumDryBulbTemperature)
# cor(allTemp$max_temp_high, allTemp$DailyMaximumDryBulbTemperature, use = 'complete.obs')
# 
# plot(allTemp$min_temp, allTemp$DailyMinimumDryBulbTemperature)
# cor(allTemp$min_temp, allTemp$DailyMinimumDryBulbTemperature, use = 'complete.obs')
# 
# plot(allTemp$DailyAverageDryBulbTemperature, allTemp$gdd_high)
# cor(allTemp$DailyAverageDryBulbTemperature, allTemp$gdd_high, use = 'complete.obs')
# summary(lm(gdd_high ~ poly(DailyAverageDryBulbTemperature,2), allTemp[]))
