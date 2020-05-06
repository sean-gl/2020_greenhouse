
rm(list = ls())
setwd("/home/sean/Documents/Clay/greenhouse/scale_and_leaftemp_data/USB_stick_clone/experiment2/scale_data")

require(ggplot2)
require(lubridate)
require(xts) # for time series objects
require(plyr)
# require(ggfortify) # to plot xts objects using ggplot

###
### Read and process data
###

dat1 <- read.csv('scale_output_dry.csv', header = F)
dat2 <- read.csv('scale_output_wet_moderate.csv', header = F, skipNul = F)
sdat <- rbind(dat1, dat2)
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
  scale_x_datetime(date_labels = "%H", breaks = '2 hours') 

# remove data before 8/26 (has issues)
# sdat <- subset(sdat, date > "2019-08-25")

# plot a single day
subdat <- sdat[sdat$timestamp > '2019-08-25 19:00' & sdat$timestamp < '2019-08-26 20:00', ]
subdat <- sdat[sdat$timestamp > '2019-08-29 02:00' & sdat$timestamp < '2019-08-30 19:10', ]

# see data from 7 am to 7 pm only
subdat <- sdat[sdat$hour >= 7 & sdat$hour <= 19, ]

ggplot(subdat, aes(x=timestamp, y=weight, color=scale)) +
  geom_point() +
  facet_wrap(~treatment) +
  scale_x_datetime(date_labels = "%H", breaks = '2 hours') 


###
### Analysis: Daily transpiration calculation
###

# convert to xts object
# sdat_xts <- as.xts(sdat, order.by = sdat$timestamp)

# --- Method 1: Total water loss, 7 am to when water turns on

sdat_sub <- subset(sdat, timeofday >= 7*60 & timeofday <= 19*60)

# sdat_xts_sub <- sdat_xts["T07:00/T19:00"]

ggplot(sdat_sub, aes(x=timestamp, y=weight, color=scale)) +
  geom_point() +
  facet_wrap(~treatment) +
  scale_x_datetime(date_labels = "%H", breaks = '2 hours') 

# calculate daily water loss (in kg/L)
# NOTE: This is wrong! This calculates amount of water given
water_loss <- ddply(sdat_sub, .(date, scale, treatment), function(x) setNames(max(x$weight) - min(x$weight), 'waterloss'))

water_loss_dailymean <- ddply(water_loss, .(date), function(x) mean(x$waterloss))

# remove 1st 2 days, moderate data has issues
water_loss <- subset(water_loss, date > '2019-08-23')

ggplot(water_loss, aes(x=date, y=waterloss, color=scale)) +
  geom_point() +
  facet_wrap(~treatment)

ggplot(water_loss, aes(x=treatment, y=waterloss, color=treatment)) +
  geom_boxplot()
  

## Analysis: Hourly RWC (relative water content)

head(sdat)

# first get water content when fully saturated (after pot is fully drained)
# plants were watered at 7 pm 
# I'll choose the weight at 1 am as indicative of field capacity

fieldcap <- sdat[sdat$hour==1 & sdat$minute==0, ]
colnames(fieldcap)[colnames(fieldcap) %in% 'weight'] <- 'fieldcap'
fieldcap <- fieldcap[ , c('scale', 'date', 'fieldcap')]

# there could be 2 measurements each day, so take mean
fieldcap <- ddply(fieldcap, .(scale, date), function(x) setNames(mean(x$fieldcap), 'fieldcap'))

# merge to all data
sdat2 <- merge(sdat, 
               fieldcap,
               by = c('scale', 'date'))

# next, find amount of water (kg = L) given each day (for each pot)
sdat_sub2 <- subset(sdat, timeofday >= 18.5*60 & timeofday <= 19.5*60)

# plot data
ggplot(sdat_sub2, aes(x=timestamp, y=weight, color=scale)) +
  geom_point() +
  facet_wrap(~treatment) +
  scale_x_datetime(date_labels = "%H", breaks = '2 hours') 

# calculate kg of water applied daily 
water_applied <- ddply(sdat_sub2, .(date, scale, treatment), function(x) setNames(max(x$weight) - min(x$weight), 'water_applied'))
# relevant water applied is from previous day
water_applied$date <- water_applied$date - 1 

# subset to data between 1 am and 6:30 pm each day
sdat2_sub <- subset(sdat2, timeofday >= 1*60 & timeofday <= 18.5*60)

# subset to hourly values for each scale (at 3 minutes after hour to match Garret's data)
sdat3_sub <- sdat2_sub[sdat2_sub$minute==3, ]

sdat4_sub <- merge(sdat3_sub, water_applied)

# this isn't really relative water content but should be proportional to it.
sdat4_sub$prop_water_loss <- (sdat4_sub$fieldcap - sdat4_sub$weight) / sdat4_sub$water_applied

# as before, there could be 2 measurements each day, so take mean
sdat4_sub <- ddply(sdat4_sub, .(scale, date, hour, treatment), function(x) setNames(mean(x$prop_water_loss), 'prop_water_loss'))

ggplot(sdat4_sub, aes(x=timestamp, y=prop_water_loss, color=scale)) +
  geom_point() +
  facet_wrap(~treatment) +
  scale_x_datetime(date_labels = "%H", breaks = '2 hours')                    


### Now look at Garret's BS sensor data

### 1. "A" sensors (moderate treatment)
gh_bs_A <- read.csv("/home/sean/Documents/Clay/Garrett Soils Data/fwdgreenhousedata/GreenHouse_A/GH_matric_A_BS_60.csv", 
                  header=TRUE, sep=",", row.names=NULL)
gh_bs_A$POSIX_time <- as.POSIXct(gh_bs_A$TIMESTAMP, format = "%m/%e/%Y %H:%M") 

head(gh_bs_A)

# add date column for merge to scale data
gh_bs_A$date <- date(gh_bs_A$POSIX_time)
# change hour column to match scale data
gh_bs_A$hour <- hour(gh_bs_A$POSIX_time)

# subset scale data to moderate trt and then get trt means
scale_moderate <- subset(sdat4_sub, treatment == 'mod')
scale_mod_means <- ddply(scale_moderate, .(date, hour, treatment), 
                         function(x) setNames(mean(x$prop_water_loss), 'prop_water_loss'))

# merge scale data (prop_water_loss) to BS sensor data 
# Note: some rows lost in merge, since dates down't overlap (and some hours have no prop_water_loss calculated)
merge_moderate <- merge(gh_bs_A, 
                        scale_mod_means,
                        by=c('date','hour'))

# plot start voltage vs. prop water loss
plot(merge_moderate$BS_s.1. ~ merge_moderate$prop_water_loss)
plot(merge_moderate$BS_s.2. ~ merge_moderate$prop_water_loss)
plot(merge_moderate$BS_s.3. ~ merge_moderate$prop_water_loss)
plot(merge_moderate$BS_s.4. ~ merge_moderate$prop_water_loss)
plot(merge_moderate$BS_s.5. ~ merge_moderate$prop_water_loss)

# plot end voltage vs. prop water loss
plot(merge_moderate$BS_e.1. ~ merge_moderate$prop_water_loss)
plot(merge_moderate$BS_e.3. ~ merge_moderate$prop_water_loss)
plot(merge_moderate$BS_e.4. ~ merge_moderate$prop_water_loss)
plot(merge_moderate$BS_e.5. ~ merge_moderate$prop_water_loss)

# plot diff voltage vs. prop water loss
plot(merge_moderate$BS_diff.1. ~ merge_moderate$prop_water_loss)
plot(merge_moderate$BS_diff.3. ~ merge_moderate$prop_water_loss)
plot(merge_moderate$BS_diff.4. ~ merge_moderate$prop_water_loss)
plot(merge_moderate$BS_diff.5. ~ merge_moderate$prop_water_loss)

# plot start vs. end voltage
# for all sensors (except 1) there is a clear linear relationship
plot(merge_moderate$BS_s.1. ~ merge_moderate$BS_e.1.)
plot(merge_moderate$BS_s.3. ~ merge_moderate$BS_e.3.)
plot(merge_moderate$BS_s.4. ~ merge_moderate$BS_e.4.)
plot(merge_moderate$BS_s.5. ~ merge_moderate$BS_e.5.)

# plot start vs. diff voltage
plot(merge_moderate$BS_s.1. ~ merge_moderate$BS_diff.1.)
plot(merge_moderate$BS_s.3. ~ merge_moderate$BS_diff.3.)
plot(merge_moderate$BS_s.4. ~ merge_moderate$BS_diff.4.)
plot(merge_moderate$BS_s.5. ~ merge_moderate$BS_diff.5.)

# plot end vs. diff voltage
plot(merge_moderate$BS_e.1. ~ merge_moderate$BS_diff.1.)
plot(merge_moderate$BS_e.3. ~ merge_moderate$BS_diff.3.)
plot(merge_moderate$BS_e.4. ~ merge_moderate$BS_diff.4.)
plot(merge_moderate$BS_e.5. ~ merge_moderate$BS_diff.5.)

# fits for each sensor. Best fit is with quadratic panel temp term. Logrithm doesn't really matter.

mod1.1 <- lm(prop_water_loss ~ BS_e.1., data = merge_moderate)
summary(mod1.1)
mod1.2 <- lm(prop_water_loss ~ BS_e.1. + poly(merge_moderate$PTemp, 2), data = merge_moderate)
summary(mod1.2)

# use starting voltage for s2 since no ending voltages
mod2.1 <- lm(prop_water_loss ~ BS_s.2., data = merge_moderate)
summary(mod2.1)
mod2.2 <- lm(prop_water_loss ~ BS_s.2. + poly(merge_moderate$PTemp, 2), data = merge_moderate)
summary(mod2.2)

mod3.1 <- lm(prop_water_loss ~ BS_e.3., data = merge_moderate)
summary(mod3.1)
mod3.2 <- lm(prop_water_loss ~ BS_e.3. + poly(merge_moderate$PTemp, 2), data = merge_moderate)
summary(mod3.2)

mod4.1 <- lm(prop_water_loss ~ BS_e.4., data = merge_moderate)
summary(mod4.1)
mod4.2 <- lm(prop_water_loss ~ BS_e.4. + poly(merge_moderate$PTemp, 2), data = merge_moderate)
summary(mod4.2)

mod5.1 <- lm(prop_water_loss ~ BS_e.5., data = merge_moderate)
summary(mod1)
mod5.2 <- lm(prop_water_loss ~ BS_e.5. + poly(merge_moderate$PTemp, 2), data = merge_moderate)
summary(mod5.2)

confint(mod3.2)
confint(mod4.2)
confint(mod5.2)



### 1. "D" sensors (fully watered treatment)
gh_bs_D <- read.csv("/home/sean/Documents/Clay/Garrett Soils Data/fwdgreenhousedata/GreenHouse_D/GH_matric_D_BS_60.csv", 
                  header=TRUE, sep=",", row.names=NULL)
gh_bs_D$POSIX_time <- as.POSIXct(gh_bs_D$TIMESTAMP, format = "%m/%e/%Y %H:%M") 

head(gh_bs_D)

# add date column for merge to scale data
gh_bs_D$date <- date(gh_bs_D$POSIX_time)
# change hour column to match scale data
gh_bs_D$hour <- hour(gh_bs_D$POSIX_time)

# subset scale data to moderate trt and then get trt means
scale_wet <- subset(sdat4_sub, treatment == 'wet')
scale_wet_means <- ddply(scale_wet, .(date, hour, treatment), 
                         function(x) setNames(mean(x$prop_water_loss), 'prop_water_loss'))

# merge scale data (prop_water_loss) to BS sensor data 
# Note: some rows lost in merge, since dates down't overlap (and some hours have no prop_water_loss calculated)
merge_wet <- merge(gh_bs_D, 
                   scale_wet_means,
                   by=c('date','hour'))

# plot start voltage vs. prop water loss
plot(merge_wet$BS_s.1. ~ merge_wet$prop_water_loss)
plot(merge_wet$BS_s.2. ~ merge_wet$prop_water_loss)
plot(merge_wet$BS_s.3. ~ merge_wet$prop_water_loss)
plot(merge_wet$BS_s.4. ~ merge_wet$prop_water_loss)
plot(merge_wet$BS_s.5. ~ merge_wet$prop_water_loss)

# plot end voltage vs. prop water loss
plot(merge_wet$BS_e.1. ~ merge_wet$prop_water_loss)
plot(merge_wet$BS_e.2. ~ merge_wet$prop_water_loss)
plot(merge_wet$BS_e.3. ~ merge_wet$prop_water_loss)
plot(merge_wet$BS_e.4. ~ merge_wet$prop_water_loss)
plot(merge_wet$BS_e.5. ~ merge_wet$prop_water_loss)

# plot diff voltage vs. prop water loss
plot(merge_wet$BS_diff.1. ~ merge_wet$prop_water_loss)
plot(merge_wet$BS_diff.2. ~ merge_wet$prop_water_loss)
plot(merge_wet$BS_diff.3. ~ merge_wet$prop_water_loss)
plot(merge_wet$BS_diff.4. ~ merge_wet$prop_water_loss)
plot(merge_wet$BS_diff.5. ~ merge_wet$prop_water_loss)

# plot start vs. end voltage
plot(merge_wet$BS_s.1. ~ merge_wet$BS_e.1.)
plot(merge_wet$BS_s.2. ~ merge_wet$BS_e.2.)
plot(merge_wet$BS_s.3. ~ merge_wet$BS_e.3.)
plot(merge_wet$BS_s.4. ~ merge_wet$BS_e.4.)
plot(merge_wet$BS_s.5. ~ merge_wet$BS_e.5.)


# fits for each sensor. Best fit is with quadratic panel temp term. Logrithm doesn't really matter.
mod_wet_1.1 <- lm(prop_water_loss ~ BS_e.1., data = merge_wet)
summary(mod_wet_1.1)
mod_wet_1.2 <- lm(prop_water_loss ~ BS_e.1. + poly(merge_wet$PTemp, 2), data = merge_wet)
summary(mod_wet_1.2)

mod_wet_2.1 <- lm(prop_water_loss ~ BS_e.2., data = merge_wet)
summary(mod_wet_2.1)
mod_wet_2.2 <- lm(prop_water_loss ~ BS_e.2. + poly(merge_wet$PTemp, 2), data = merge_wet)
summary(mod_wet_2.2)

mod_wet_3.1 <- lm(prop_water_loss ~ BS_e.3., data = merge_wet)
summary(mod_wet_3.1)
mod_wet_3.2 <- lm(prop_water_loss ~ BS_e.3. + poly(merge_wet$PTemp, 2), data = merge_wet)
summary(mod_wet_3.2)

mod_wet_4.1 <- lm(prop_water_loss ~ BS_e.4., data = merge_wet)
summary(mod_wet_4.1)
mod_wet_4.2 <- lm(prop_water_loss ~ BS_e.4. + poly(merge_wet$PTemp, 2), data = merge_wet)
summary(mod_wet_4.2)

mod_wet_5.1 <- lm(prop_water_loss ~ BS_e.5., data = merge_wet)
summary(mod_wet_5.1)
mod_wet_5.2 <- lm(prop_water_loss ~ BS_e.5. + poly(merge_wet$PTemp, 2), data = merge_wet)
summary(mod_wet_5.2)

coef(mod2.2); coef(mod3.2); coef(mod4.2); coef(mod5.2)
coef(mod_wet_2.2); coef(mod_wet_3.2); coef(mod_wet_4.2); coef(mod_wet_5.2)
