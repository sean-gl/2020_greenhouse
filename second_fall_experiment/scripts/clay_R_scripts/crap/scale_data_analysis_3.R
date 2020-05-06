
rm(list = ls())
wd <- "/home/sean/Documents/Clay/greenhouse/2019 greenhouse data/experiment2/scale_output/"
setwd(wd)

require(ggplot2)
require(lubridate)
require(plyr) 
require(dplyr)
require(tidyr)

###
### ------Read and process data
###

# read in files
files <- dir(wd)
files <- files[grepl('.csv', files)]
files

datList <- lapply(files, function(x) read.csv(x, header = F, skipNul = F, stringsAsFactors = F))
names(datList) <- files
for(i in 1:length(datList)) {
  datList[[i]][,4] <- files[i]
  colnames(datList[[i]]) <- c('scale','timestamp','weight','file')
  datList[[i]]$timestamp <- as.POSIXct(datList[[i]]$timestamp, format="%Y-%m-%d %H:%M:%S", tz = 'MST')
}

### ------Preliminary combine data and check for duplicate rows
scaledat <- do.call(rbind, datList)
rownames(scaledat) <- NULL
anyDuplicated(scaledat[,c('scale','timestamp')])

### ------ Fix time zone issues with certain csv files

# NOTE: This file has a "jump back" 1 hour for daylight savings time
head(datList$`scale_output_wet_moderate_11-04.csv`)
tail(datList$`scale_output_wet_moderate_11-04.csv`)
x = datList$`scale_output_wet_moderate_11-04.csv`
x$d = c(0, diff(x$timestamp))
which(x$d == -3570) # time set back 1 hour here (3600 seconds minus 30 seconds = 3570)
x$timestamp[57737:nrow(x)] <- x$timestamp[57737:nrow(x)] + 3600
x[57720:57750,]
x$d <- NULL
datList$`scale_output_wet_moderate_11-04.csv` <- x
rm(x)
head(datList$`scale_output_wet_moderate_11-04.csv`)
tail(datList$`scale_output_wet_moderate_11-04.csv`)

# Re-Combine all data
scaledat <- do.call(rbind, datList)
rownames(scaledat) <- NULL

## Need to convert pre-baseline data from MDT to MST by subtracting 1 hour
# timestamps before 2019-10-23 17:00 MDT are the following files:
MDT_files <- c('scale_output_wet_moderate_10-08.csv',
               'scale_output_wet_moderate_10-22.csv',
               'scale_output_wet_moderate_10-23_MDT.csv',
               'scale_output_dry_10-4.csv',
               'scale_output_dry_10-17.csv',
               'scale_output_dry_10-23.csv')

head(scaledat[scaledat$file == "scale_output_wet_moderate_10-23_MDT.csv", ])

ind <- scaledat$file %in% MDT_files
scaledat$timestamp[ind] <- scaledat$timestamp[ind] - 3600
dups <- duplicated(scaledat[,c('scale','timestamp')])
which(dups)
View(scaledat[dups,])

# remove one duplicated row
scaledat <- scaledat[!dups, ]

head(scaledat[scaledat$file == "scale_output_wet_moderate_10-23_MDT.csv", ])



### ------ Change data types and further data "massaging"
scaledat$scale <- factor(scaledat$scale)
# NOTE: these are blocks, not treatments!!
scaledat[scaledat$scale %in% c(1:4, 15), 'treatment'] <- 'W' 
scaledat[scaledat$scale %in% 5:8, 'treatment'] <- 'M' 
scaledat[scaledat$scale %in% c(9, 10, 12, 14, 16), 'treatment'] <- 'D' 
scaledat$treatment <- factor(scaledat$treatment)
scaledat$date <- date(scaledat$timestamp)
scaledat$hour <- hour(scaledat$timestamp)
scaledat$minute <- minute(scaledat$timestamp)
scaledat$timeofday <- scaledat$hour * 60 + scaledat$minute

# remove row with no scale number
which(is.na(scaledat$scale))
scaledat[is.na(scaledat$scale),] 
scaledat <- scaledat[!is.na(scaledat$scale), ]

# remove erroneous dates
summary(scaledat$date)
# these are bad data (I checked original csv file, date is wrong there), remove them
ind <- scaledat$date == '2019-02-14'
scaledat <- scaledat[!ind, ]


# order by timestamp and scale
scaledat <- scaledat[with(scaledat, order(scale, timestamp)), ] 


# add relative_weight (relative to starting weight)
for(s in unique(scaledat$scale)) {
  initial_wt <- scaledat$weight[scaledat$scale == s][1]
  print(initial_wt)
  scaledat$rel_weight[scaledat$scale == s] <- scaledat$weight[scaledat$scale == s] - initial_wt
}


# Table dates adn treatments
with(scaledat[scaledat$date <= '2019-11-11', ], table(date, treatment))

###
### Plotting
###

# plot all data
ggplot(scaledat, aes(x=timestamp, y=rel_weight, color=scale)) +
  geom_point() +
  facet_wrap(~treatment) +
  scale_x_datetime(date_labels = "%H", breaks = '12 hours') +
  NULL
  # ylim

### plot subsets

# date range
subscale <- subset(scaledat, date >= '2019-11-24')

# keep only every 5 minutes
rowsToKeep <- seq(1, nrow(scaledat), 10)
subscale <- scaledat[rowsToKeep, ]
subscale <- subset(subscale, date >= '2019-11-04')

ggplot(subscale, aes(x=timestamp, y=weight, color=scale)) +
  # geom_point() +
  geom_line() +
  facet_wrap(~treatment) +
  ylim(c(9,15)) +
  scale_x_datetime(date_labels = "%d", breaks = '1 day')



#### fit a line and calculate slope every 10 minutes (n=20 readings)

testdat <-  subset(scaledat, date >= '2019-11-15' & scale == 10)
# plot(weight~timestamp, testdat)
min_interval = 30
s <- seq(1, nrow(testdat), min_interval*2)
s2 <- testdat$timestamp[s]
head(s)
head(s2)
z <- lapply(s, function(x) {
  mod = lm(weight ~ timestamp, testdat[x:(x+(min_interval*2)-1), ])
  slope = coef(mod)[2]
  rsq = summary(mod)$adj.r.squared
  dat = data.frame(m=slope, arsq=rsq, timestamp = testdat$timestamp[x])
  return(dat)
})

d = do.call(rbind, z)
rm(z)
d$m = abs(d$m * 3600) # convert to L/hr

ggplot(d, aes(x=timestamp, y=m)) +
  geom_point() +
  geom_line() +
  ylim(c(0, 0.15)) +
  # ylim(c(-4e-5, 1e-5)) +
  # scale_x_datetime(date_labels = "%H", breaks = '4 hours') +
  scale_x_datetime(date_labels = "%d", breaks = '1 day') +
  ggtitle('W-10 (drought) transpiration rate, L/hr', subtitle = 'Nov. 14-19')


plot(s2, z, ylim = c(-4e-5, 1e-5))

zpos = abs(z)
zpos = zpos / max(zpos, na.rm = T) 

plot(s2, zpos)

# plot data between 6 and 9 pm only
scaleSub <- subset(scaledat, hour >= 17 & hour <= 20 & date == '2019-10-28')
scaleSub <- subset(scaledat, hour >= 17 & hour <= 20)
ggplot(scaleSub, aes(x=timestamp, y=rel_weight, color=scale)) +
  geom_point() +
  facet_wrap(~treatment) +
  scale_x_datetime(date_labels = "%H", breaks = '1 hours') +
  NULL




### 
### Analysis: Water given each day
###



# calculate max - min during 1-hr interval surrounding watering

waterGiven <- ddply(scaledat, .(date, scale, treatment), function(x) {
  # For dates < 10/28, water was given at 19:00 MST. For dates >= 10/28, water was at 20:05 MST.
  if(x$date < '2019-10-28') {
    w <- subset(x, timeofday >= 17*60 + 30 & timeofday <= 18*60 + 30)
  } else {
    if(x$date < '2019-11-27') {
      w <- subset(x, timeofday >= 18*60 + 35 & timeofday <= 19*60 + 35)
    } else {
      w <- subset(x, timeofday >= 19*60 + 35 & timeofday <= 20*60 + 35)
    }
  }
  return(setNames(max(w$weight) - min(w$weight), 'water_given'))
})

# summarize by treatment
waterGiven %>%
  filter(water_given != '-Inf') %>% 
  group_by(treatment) %>%
  summarize(mean_water_given = mean(water_given), sd_water_given = sd(water_given)) 

# summarize by scale
waterGiven %>%
  filter(water_given != '-Inf') %>% 
  group_by(scale) %>%
  summarize(mean_water_given = mean(water_given), sd_water_given = sd(water_given)) 



### Compute water used daily, between 2 am and 8 pm
head(scaledat)
scaleWide <- subset(scaledat, date >= "2019-11-15")
# scaleWide <- scaledat
scaleWide <- scaleWide[ , c('scale','timestamp','weight')]
scaleWide <- spread(scaleWide, key = scale, value = weight)
head(scaleWide)
formalArgs(SummarizeTimeseries)
scaleSummary <- SummarizeTimeseries(dat = scaleWide, 
                                    tsCol = 'timestamp',
                                    # measCols = as.character(c(1:10, 12, 14:16)),
                                    measCols = as.character(c(1:10, 12, 14)),
                                    interval = 15,
                                    fillGaps = TRUE)


# dat = scaleWide 
# tsCol = 'timestamp'
# measCols = as.character(c(1:10, 12, 14:16))
# interval = 15
# fillGaps = TRUE

# GET 5-MINUTE MEAN VALUES
scaleMeans <- scaleSummary$means
View(scaleMeans)
View(scaleSummary$sampleSizes)

# NOTE THIS IS A  ROUGH APPROXIMATION...NEEDTO REFINE....
dailyUseWide <- ddply(scaleMeans, .(date), function(x) {
  # startingValues <- x[x$interval == 60*4, as.character(c(1:10, 12, 14:16))]
  # endingValues <- x[x$interval == 60*19, as.character(c(1:10, 12, 14:16))]
  startingValues <- x[x$interval == 60*4, as.character(c(1:10, 12, 14))]
  endingValues <- x[x$interval == 60*19, as.character(c(1:10, 12, 14))]
  return(startingValues - endingValues)
})


# convert to long for plotting
# dailyUse <- gather(dailyUseWide, 'scale', 'water_use', 2:15)
dailyUse <- gather(dailyUseWide, 'scale', 'water_use', as.character(c(1:10, 12, 14)))
dailyUse$treatment[dailyUse$scale %in% c(1:4, 15)] <- 'W'
dailyUse$treatment[dailyUse$scale %in% 5:8] <- 'M'
dailyUse$treatment[dailyUse$scale %in% c(9,10,12,14,16)] <- 'D'

ggplot(dailyUse, aes(x = date, y = water_use, color = scale, group = scale)) +
  geom_line() + 
  geom_point() + 
  facet_wrap(~treatment) +
  scale_x_date(date_labels = "%m-%d", breaks = '7 days') 

dailyUseMeans <- ddply(dailyUse, .(date, treatment), function(x) {
 setNames(mean(x$water_use, na.rm = T), 'mean_water_use')
})

ggplot(dailyUseMeans, aes(x=date, y=mean_water_use, color=treatment)) +
  geom_line() + geom_point() +
  scale_x_date(date_labels = "%m-%d", breaks = '7 days') 

dailyUseMeans %>% 
  filter(date >= "2019-11-11" & date != '2019-11-18') %>% 
  group_by(treatment) %>% 
  summarize(mean = mean(mean_water_use, na.rm = T))


# convert WaterGiven to wide format
waterGivenWide <- waterGiven %>% 
  filter(date >= '2019-11-04') %>% 
  select(date, scale, water_given) %>% 
  spread(key = 'scale', value = 'water_given')

givenMinusUsed <- waterGivenWide - dailyUseWide
givenMinusUsed$date <- waterGivenWide$date
givenMinusUsedLong <- gather(givenMinusUsed, 'scale', 'given_used', 2:ncol(givenMinusUsed))

givenMinusUsedLong$treatment[givenMinusUsedLong$scale %in% c(1:4, 15)] <- 'dry'
givenMinusUsedLong$treatment[givenMinusUsedLong$scale %in% 5:8] <- 'moderate'
givenMinusUsedLong$treatment[givenMinusUsedLong$scale %in% c(9,10,12,14,16)] <- 'wet'


ggplot(givenMinusUsedLong, aes(x = date, y = given_used, color = scale)) +
  geom_line() +
  geom_point() +
  facet_wrap(~treatment)


#### OLD CODE .....









# plot date range
subdat <- scaledat[scaledat$timestamp > '2019-10-01 00:00' & scaledat$timestamp < '2019-10-05 20:00', ]
subdat <- scaledat[scaledat$timestamp > '2019-08-29 02:00' & scaledat$timestamp < '2019-08-30 19:10', ]

# see data from 7 am to 7 pm only
subdat <- scaledat[scaledat$hour >= 7 & scaledat$hour <= 19, ]

ggplot(subdat, aes(x=timestamp, y=weight, color=scale)) +
  geom_point() +
  facet_wrap(~treatment) +
  scale_x_datetime(date_labels = "%H", breaks = '2 hours')  +
  scale_y_continuous(limits = c(10, 15))




### ----- Analysis: Calculate daily minima, this is indicative of biomass gain (??)

# get daily min weights by scale
daily_min <- ddply(scaledat, .(date, scale), function(x) setNames(min(x$weight), 'weight'))

# plot them
ggplot(daily_min, aes(x=date, y=weight, color=scale)) +
  geom_line() 

# get daily differences in min weight
ddply(daily_min, .(scale), function(x) diff(x$weight))

## Takeaway: Hard to detect any biomass differences between days


## max minus min (this should be indicative of the amount of water given each day)
daily_maxmin <- ddply(scaledat, .(date, scale), function(x) setNames(max(x$weight) - min(x$weight), 'weight_diff'))

# plot them
ggplot(daily_maxmin, aes(x=date, y=weight_diff, color=scale)) +
  geom_line() 

require(dplyr)
# filter out dates with erratic data
daily_maxmin %>% filter(date < '2019-09-23' & date > '2019-09-17') %>% group_by(scale) %>% summarise(Mean=mean(weight_diff))
x = daily_maxmin %>% filter(date < '2019-09-23' & date > '2019-09-17') %>% group_by(scale) 
boxplot(x$weight_diff ~ x$scale)
x$trt <- 'none'
x$trt[x$scale %in% 1:4] <- 'wet'
x$trt[x$scale %in% 5:8] <- 'mod'
x$trt[x$scale %in% 9:14] <- 'dry'
boxplot(x$weight_diff ~ x$trt)

### ----- Analysis: 
