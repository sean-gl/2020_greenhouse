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


# read in temperature/RH/PAR data
edat <- read.csv('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/RH_temp_PAR_logger_data/rh_15.csv',
                  stringsAsFactors = FALSE)
edat$by15 <- as.POSIXct(edat$by15, tz='GMT')
edat$date <- date(edat$by15)


# omit 1st and last day since we don't have full 24-hours of data
edat <- subset(edat, !date %in% as.Date(c('2019-10-21', '2019-12-12')))

# calculate daily max/min temperatures at 75% canopy height ("low") and ~1m above canopy("high")
tempSummary <- edat %>% 
  group_by(date) %>% 
  summarise(max_temp_high = max(sht1_high_temp), min_temp_high = min(sht1_high_temp),
            max_temp_low = max(sht2_low_temp), min_temp_low = min(sht2_low_temp))

# now calculate simple mean temp using daily max/mean
tempSummary$mean_temp_high <- rowMeans(tempSummary[,c('max_temp_high','min_temp_high')], na.rm = TRUE)
tempSummary$mean_temp_low <- rowMeans(tempSummary[,c('max_temp_low','min_temp_low')], na.rm = TRUE)
summary(tempSummary)

# calculate GDD (note: T_base = 50 F = 10 C)
tempSummary$gdd_high <- ifelse(tempSummary$max_temp_high >= 10, tempSummary$mean_temp_high - 10, 0)
tempSummary$gdd_low <- ifelse(tempSummary$max_temp_low >= 10, tempSummary$mean_temp_low - 10, 0)

# calculate modified GDD
tempSummary$max_temp_high_mod <- ifelse(tempSummary$max_temp_high > 30, 30, tempSummary$max_temp_high)
tempSummary$min_temp_high_mod <- ifelse(tempSummary$min_temp_high < 10, 10, tempSummary$min_temp_high)
tempSummary$max_temp_low_mod <- ifelse(tempSummary$max_temp_low > 30, 30, tempSummary$max_temp_low)
tempSummary$min_temp_low_mod <- ifelse(tempSummary$min_temp_low < 10, 10, tempSummary$min_temp_low)
tempSummary$mean_temp_high_mod <- rowMeans(tempSummary[,c('max_temp_high_mod','min_temp_high_mod')], na.rm = TRUE)
tempSummary$mean_temp_low_mod <- rowMeans(tempSummary[,c('max_temp_low_mod','min_temp_low_mod')], na.rm = TRUE)

tempSummary$mgdd_high <- ifelse(tempSummary$max_temp_high_mod > 10, tempSummary$mean_temp_high_mod - 10, 0)
tempSummary$mgdd_low <- ifelse(tempSummary$max_temp_low_mod > 10, tempSummary$mean_temp_low_mod - 10, 0)

plot(tempSummary$date, tempSummary$mgdd_high, type='l')
lines(tempSummary$date, tempSummary$mgdd_low,col='blue')


# Subset to columns of interest in modeling leaf area
tempSummary2 <- select(tempSummary, date, mgdd_low, mgdd_high, gdd_low, gdd_high)

# Extend data back in time, using mean value
missingdates <- c(seq.Date(as.Date('2019-09-09'), as.Date('2019-10-23'), by=1),
                  as.Date('2019-12-12'))
missingdates <- data.frame(date=missingdates, 
                           mgdd_low=mean(tempSummary2$mgdd_low),
                           mgdd_high=mean(tempSummary2$mgdd_high),
                           gdd_low=mean(tempSummary2$gdd_low),
                           gdd_high=mean(tempSummary2$gdd_high))
tempSummary2 <- merge(tempSummary2, missingdates, all = TRUE)

# add cumsum column
tempSummary2$mgdd_low_cumsum <- cumsum(tempSummary2$mgdd_low)
tempSummary2$mgdd_high_cumsum <- cumsum(tempSummary2$mgdd_high)
tempSummary2$gdd_low_cumsum <- cumsum(tempSummary2$gdd_low)
tempSummary2$gdd_high_cumsum <- cumsum(tempSummary2$gdd_high)


# Read in MASTER data set 15-min
comb <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/combined_data/combdat_plant_level.rds')
comb$date <- date(comb$by15)

# get mean leaf area by date & treatment
# NOTE: The mean() function is used for convenience, this is really just tabling by date.
la <- comb %>% group_by(date, treatment, block) %>% 
  summarise(mean_leaf_area_m2 = mean(mean_plant_leaf_area_m2, na.rm=T))

# examine, looks ok
ggplot(la, aes(x=date, y=mean_leaf_area_m2, color=block)) + geom_line()

alldat <- merge(tempSummary2, la, by='date', all = TRUE)

# remove dates with missing leaf area
alldat <- alldat[!is.na(alldat$mean_leaf_area_m2), ]


# Doesn't matter which gdd or mgdd, high or low. R2 = 0.33
summary(lm(mean_leaf_area_m2 ~ poly(mgdd_low_cumsum, 2) + treatment, alldat ))
summary(lm(mean_leaf_area_m2 ~ mgdd_high_cumsum + treatment, alldat ))
summary(lm(mean_leaf_area_m2 ~ gdd_low_cumsum + treatment, alldat ))
summary(lm(mean_leaf_area_m2 ~ gdd_high_cumsum + treatment, alldat ))

# add quadratic, r2=0.54
summary(lm(mean_leaf_area_m2 ~ poly(mgdd_low_cumsum, 2) + treatment, alldat ))
ggplot(alldat, aes(x=mgdd_low_cumsum, y=mean_leaf_area_m2, color=treatment)) + geom_point()

## NOTE: Tempearture data only available from 8/23 to 8/31 for 1st experiment
# seeds planted on 7/11.


clim <- read.csv('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/FC_climate_data_NOAA.csv',
                 stringsAsFactors = F)


# filter out dry bulb temp
clim <- subset(clim, DailyMaximumDryBulbTemperature != '' &
                  DailyMinimumDryBulbTemperature != '' &
                  DailyAverageDryBulbTemperature != '', 
                select = c('DATE','DailyMaximumDryBulbTemperature','DailyMinimumDryBulbTemperature',
                           'DailyAverageDryBulbTemperature'))

# convert date
names(clim)[names(clim) =='DATE'] <- 'date'
clim$date <- as.Date(substr(clim$date, 1, 10))

# change temps to numeric and convert to celcius
# NOTE: some values have an "s" and these become NA, not sure but probably a flag.
for(i in c('DailyMaximumDryBulbTemperature','DailyMinimumDryBulbTemperature', 'DailyAverageDryBulbTemperature')) {
  clim[[i]] <- as.numeric(clim[[i]])
  clim[[i]] <- (clim[[i]] - 32) * 5/9
}

length(which(complete.cases(clim)))


# merge climate data to greenhouse temp data
allTemp <- merge(tempSummary, clim, by='date', all = TRUE)

plot(allTemp$max_temp_high, allTemp$DailyMaximumDryBulbTemperature)
cor(allTemp$max_temp_high, allTemp$DailyMaximumDryBulbTemperature, use = 'complete.obs')

plot(allTemp$min_temp_high, allTemp$DailyMinimumDryBulbTemperature)
cor(allTemp$min_temp_high, allTemp$DailyMinimumDryBulbTemperature, use = 'complete.obs')

plot(allTemp$DailyAverageDryBulbTemperature, allTemp$gdd_high)
cor(allTemp$DailyAverageDryBulbTemperature, allTemp$gdd_high, use = 'complete.obs')
summary(lm(gdd_high ~ poly(DailyAverageDryBulbTemperature,2), allTemp[]))
