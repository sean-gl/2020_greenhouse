
rm(list = ls())
setwd("/home/wmsru/Documents/Clay/greenhouse/2019 greenhouse data/experiment2/scale_output/pre-baseline/")

require(ggplot2)
require(lubridate)
require(xts) # for time series objects
require(plyr)
# require(ggfortify) # to plot xts objects using ggplot

###
### Read and process data
###

dat1 <- read.csv('scale_output_dry_10-23.csv', header = F)
dat2 <- read.csv('scale_output_wet_moderate_10-22.csv', header = F, skipNul = F)
sdat <- rbind(dat1, dat2)

# or read just one pi
# sdat <- read.csv('scale_output_wet_moderate_10-15.csv', header = F, skipNul = F)

colnames(sdat) <- c('scale','timestamp','weight')
sdat$scale <- factor(sdat$scale)
sdat$timestamp <- as.POSIXct(sdat$timestamp, format="%Y-%m-%d %H:%M:%S")
sdat[sdat$scale %in% 1:4, 'treatment'] <- 'wet' 
sdat[sdat$scale %in% 5:8, 'treatment'] <- 'wet' 
sdat[sdat$scale %in% c(9, 10, 12, 14), 'treatment'] <- 'wet' 
sdat$treatment <- factor(sdat$treatment)
sdat$date <- date(sdat$timestamp)
sdat$hour <- hour(sdat$timestamp)
sdat$minute <- minute(sdat$timestamp)
sdat$timeofday <- sdat$hour * 60 + sdat$minute

# remove row with no scale number
which(is.na(sdat$scale))
sdat[is.na(sdat$scale),] 
sdat <- sdat[!is.na(sdat$scale), ]


###
### Plotting
###

# plot all data
ggplot(sdat, aes(x=timestamp, y=weight, color=scale)) +
  geom_point() +
  facet_wrap(~treatment) +
  scale_x_datetime(date_labels = "%H", breaks = '2 hours') +
  ylim(9, 15)
# remove data before 8/26 (has issues)
# sdat <- subset(sdat, date > "2019-08-25")

# plot date range
subdat <- sdat[sdat$timestamp > '2019-10-01 00:00' & sdat$timestamp < '2019-10-05 20:00', ]
subdat <- sdat[sdat$timestamp > '2019-08-29 02:00' & sdat$timestamp < '2019-08-30 19:10', ]

# see data from 7 am to 7 pm only
subdat <- sdat[sdat$hour >= 7 & sdat$hour <= 19, ]

ggplot(subdat, aes(x=timestamp, y=weight, color=scale)) +
  geom_point() +
  facet_wrap(~treatment) +
  scale_x_datetime(date_labels = "%H", breaks = '2 hours')  +
  scale_y_continuous(limits = c(10, 15))




### ----- Analysis: Calculate daily minima, this is indicative of biomass gain (??)

# get daily min weights by scale
daily_min <- ddply(sdat, .(date, scale), function(x) setNames(min(x$weight), 'weight'))

# plot them
ggplot(daily_min, aes(x=date, y=weight, color=scale)) +
  geom_line() 

# get daily differences in min weight
ddply(daily_min, .(scale), function(x) diff(x$weight))

## Takeaway: Hard to detect any biomass differences between days


## max minus min (this should be indicative of the amount of water given each day)
daily_maxmin <- ddply(sdat, .(date, scale), function(x) setNames(max(x$weight) - min(x$weight), 'weight_diff'))

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