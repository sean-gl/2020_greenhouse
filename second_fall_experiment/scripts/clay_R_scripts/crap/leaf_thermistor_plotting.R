
rm(list = ls())
setwd("/home/sean/Documents/Clay/greenhouse/greenhouse_leaf_thermistor_data")
ard <- read.csv('leaf_thermistor_wet_dry_south_08_21-23.TXT', header = F)
head(ard)

names(ard) <- c('timestamp', 'Vin', paste0('t',1:8))
head(ard$timestamp)
head(as.POSIXct(ard$timestamp, format="%Y/%m/%d %H:%M:%S"))
ard$timestamp <-as.POSIXct(ard$timestamp, format="%Y/%m/%d %H:%M:%S")

require(tidyr)
ard <- gather(ard[ , -2], key='thermistor', value='temp', 2:9)
ard$thermistor <- factor(ard$thermistor)
head(ard)

require(ggplot2)

ggplot(ard, aes(x=timestamp, y=temp, color=thermistor)) + 
  geom_point() 

subdat <- subset(ard, timestamp > '2019-08-02 00:00')
ggplot(subdat, aes(x=timestamp, y=temp, color=thermistor)) + 
  geom_point() 

# remove outlier
which(ard$temp == max(ard$temp))
ard <- ard[c(1:4, 6:nrow(ard)), ]

# examine peak temperature period
which(ard$temp == max(ard$temp))

ard[51,]
subdat <- subset(ard, timestamp > '2019-08-01 15:10' & timestamp < '2019-08-01 16:00')
# subdat <- subset(ard, timestamp > '2019-08-01 17:00' & timestamp < '2019-08-01 18:00')
ggplot(subdat, aes(x=timestamp, y=temp, color=thermistor)) + 
  geom_point() 
