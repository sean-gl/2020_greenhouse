
rm(list = ls())
setwd("/home/sean/Documents/Clay/greenhouse/greenhouse_scale_and_leaftemp_data/leaf thermistor data")


### --- Dry treatment data

dry <- read.csv('leaf_thermistor_dry_east_09-03.TXT', header = F)
head(dry)

names(dry) <- c('timestamp', 'Vin', 1:8)
head(dry$timestamp)
head(as.POSIXct(dry$timestamp, format="%Y/%m/%d %H:%M:%S"))
dry$timestamp <-as.POSIXct(dry$timestamp, format="%Y/%m/%d %H:%M:%S")

require(tidyr)
dry <- gather(dry[ , -2], key='thermistor', value='temp', 2:9)
dry$thermistor <- factor(dry$thermistor)
head(dry)

# add factor for top/bottom leaf
dry$position <- ifelse(dry$thermistor %in% c(1, 3, 5, 7), 'top', 'bottom')

require(ggplot2)

ggplot(dry, aes(x=timestamp, y=temp, color=thermistor)) + 
  geom_point() +
  facet_wrap(~position) +
  ylim(20, 35)

subdat <- subset(dry, timestamp > '2019-09-02 00:00')
ggplot(subdat, aes(x=timestamp, y=temp, color=thermistor)) + 
  geom_point() 



### --- Wet treatment data

wet <- read.csv('leaf_thermistor_wet_west_09-03.TXT', header = F)
head(wet)

names(wet) <- c('timestamp', 'Vin', 1:8)
head(wet$timestamp)
head(as.POSIXct(wet$timestamp, format="%Y/%m/%d %H:%M:%S"))
wet$timestamp <-as.POSIXct(wet$timestamp, format="%Y/%m/%d %H:%M:%S")

require(tidyr)
wet <- gather(wet[ , -2], key='thermistor', value='temp', 2:9)
wet$thermistor <- factor(wet$thermistor)
head(wet)

require(ggplot2)

ggplot(wet, aes(x=timestamp, y=temp, color=thermistor)) + 
  geom_point() 

subdat <- subset(wet, timestamp > '2019-09-02 00:00')
ggplot(subdat, aes(x=timestamp, y=temp, color=thermistor)) + 
  geom_point() 


### ---- Combine and Compare Wet and Dry

wetdry <- wet
wetdry$thermistor