
rm(list = ls())

require(dplyr); require(tidyr); require(ggplot2)

wd <- "/home/sean/Documents/Clay/greenhouse/2019 greenhouse data/experiment2/leaf_thermistor_data/"
setwd(wd)

files <- dir(wd)
files <- files[grepl('THERM', files)]
files
files[1]
x <- read.csv(files[1], skip = 2, header = F, stringsAsFactors = F)
x <- read.csv(files[1], header = F, stringsAsFactors = F)
head(x)

# omit rows without data
datList <- lapply(files, function(x) {
  d <- read.csv(x, header = F, stringsAsFactors = F)
  noData <- grepl('timestamp', d[,1]) | grepl('treatment', d[,1])
  d <- d[!noData,]
  d$file <- x
  return(d)
})

thermdat <- do.call(rbind, datList)

thermdat$block <- NA
thermdat$block[grepl('wet', thermdat$file) | grepl('_W_', thermdat$file)] <- 'w'
thermdat$block[grepl('moderate', thermdat$file) | grepl('_M_', thermdat$file)] <- 'm'
thermdat$block[grepl('dry', thermdat$file) | grepl('_D_', thermdat$file)] <- 'd'

# thermistors position alternates bottom/top
colnames(thermdat) <- c('timestamp','Vin','1_lower','1_upper','2_lower','2_upper',
                        '3_lower','3_upper', '4_lower','4_upper','file','block')
head(thermdat)

thermdat$timestamp <- as.POSIXct(thermdat$timestamp, format="%Y/%m/%d %H:%M:%S")

# change temperature columns to numeric
for(col in c('1_lower','1_upper','2_lower','2_upper','3_lower','3_upper', '4_lower','4_upper')) {
  thermdat[,col] <- as.numeric(thermdat[,col])
}

# change to factors
thermdat$block <- as.factor(thermdat$block)

# add date
thermdat$date <- lubridate::date(thermdat$timestamp)

# order by timestamp and block
thermdat <- thermdat[with(thermdat, order(timestamp, block)), ]

#### -------------- IMPORTANT: REMOVING MANY ROWS HERE!! -----------------

# keep only every 20th row
thermdat <- thermdat[seq(1, nrow(thermdat), 20), ]




# calculate mean temperature for each row, lower and upper canopy
thermdat$lower_mean <- rowMeans(thermdat[ , grep('lower', colnames(thermdat))])
thermdat$upper_mean <- rowMeans(thermdat[ , grep('upper', colnames(thermdat))])

# Look only at means
thermdat_means <- thermdat %>% select(timestamp, date, lower_mean, upper_mean, block)
# convert to a format for ggplot
thermdat_means <- gather(thermdat_means, key = position, value = temperature, c('lower_mean','upper_mean'))

thermdat_means$position <- as.factor(thermdat_means$position)


# remove outliers (bad data)
thermdat_means <- subset(thermdat_means, temperature < 35 & temperature > 10)

upper <- subset(thermdat_means, position == 'upper_mean')
lower <- subset(thermdat_means, position == 'lower_mean')

# subset date range
# upper <- subset(upper, timestamp >= "2019-11-24")
upper <- subset(upper, date == "2019-11-29")

lower <- subset(lower, date == "2019-11-29")

# Plot upper
ggplot(upper, aes(x=timestamp, y=temperature, group=block, color=block)) +
  geom_line() +
  # scale_x_datetime(date_labels = "%d", breaks = '1 day')
  scale_x_datetime(date_labels = "%H", breaks = '4 hours')

# Plot lower
ggplot(lower, aes(x=timestamp, y=temperature, group=block, color=block)) +
  geom_line() +
  # scale_x_datetime(date_labels = "%d", breaks = '1 day')
  scale_x_datetime(date_labels = "%H", breaks = '4 hours')

### TODO:
## sumarize by date
# upper$date <- date(upper$timestamp)
# lower$date <- date(lower$timestamp)
# upperDaily <- ddply(upper, .(date, block), function(x) {
#   x <- subset(x, hour())
#   setNames(mean(x$temperature, na.rm = T), 'mean_temperature')
# })
# lowerDaily <- ddply(lower, .(date, block), function(x) {
#   setNames(mean(x$temperature, na.rm = T), 'mean_temperature')
# })
# 
# head(upperDaily)

### OLDER FANCIER CODE BELOW.....IS IT WORKING?



# read in WET data
wet_files <- files[grepl('THERM_wet', files)]
datList <- lapply(wet_files, function(x) read.csv(x, skip = 1, header = T, stringsAsFactors = F))
names(datList) <- wet_files
for(i in 1:length(datList)) {
  datList[[i]]$file <- wet_files[i]
}
thermdat_wet <- do.call(rbind, datList)
rownames(thermdat_wet) <- NULL

# read in MODERATE data
moderate_files <- files[grepl('THERM_moderate', files)]
datList <- lapply(moderate_files, function(x) read.csv(x, skip = 1, header = T, stringsAsFactors = F))
names(datList) <- moderate_files
for(i in 1:length(datList)) {
  datList[[i]]$file <- moderate_files[i]
}
thermdat_moderate<- do.call(rbind, datList)
rownames(thermdat_moderate) <- NULL

# read in DRY data
dry_files <- files[grepl('THERM_dry', files)]
datList <- lapply(dry_files, function(x) read.csv(x, skip = 1, header = T, stringsAsFactors = F))
names(datList) <- dry_files
for(i in 1:length(datList)) {
  datList[[i]]$file <- dry_files[i]
}
thermdat_dry <- do.call(rbind, datList)
rownames(thermdat_dry) <- NULL


### Remove bogus header rows
ind <- grepl('treatment', thermdat_dry$timestamp) | grepl('timestamp', thermdat_dry$timestamp)
thermdat_dry <- thermdat_dry[!ind, ]
ind <- grepl('treatment', thermdat_wet$timestamp) | grepl('timestamp', thermdat_wet$timestamp)
thermdat_wet <- thermdat_wet[!ind, ]
ind <- grepl('treatment', thermdat_moderate$timestamp) | grepl('timestamp', thermdat_moderate$timestamp)
thermdat_moderate <- thermdat_moderate[!ind, ]



### Reshape data (wide to long)
# dry
thermdat_dry_long <- gather(thermdat_dry, pot_position, temperature, 3:10)
pot_pos <- strsplit(thermdat_dry_long$pot_position, '_')
thermdat_dry_long$pot <- sapply(pot_pos, function(x) x[1])
thermdat_dry_long$position <- sapply(pot_pos, function(x) x[2])
thermdat_dry_long$pot_position <- NULL
thermdat_dry_long$treatment <- 'dry'

# moderate
thermdat_moderate_long <- gather(thermdat_moderate, pot_position, temperature, 3:10)
pot_pos <- strsplit(thermdat_moderate_long$pot_position, '_')
thermdat_moderate_long$pot <- sapply(pot_pos, function(x) x[1])
thermdat_moderate_long$position <- sapply(pot_pos, function(x) x[2])
thermdat_moderate_long$pot_position <- NULL
thermdat_moderate_long$treatment <- 'moderate'

# wet
thermdat_wet_long <- gather(thermdat_wet, pot_position, temperature, 3:10)
pot_pos <- strsplit(thermdat_wet_long$pot_position, '_')
thermdat_wet_long$pot <- sapply(pot_pos, function(x) x[1])
thermdat_wet_long$position <- sapply(pot_pos, function(x) x[2])
thermdat_wet_long$pot_position <- NULL
thermdat_wet_long$treatment <- 'wet'


### Finally, combine data from all treatments (long format)
thermdat_long <- rbind(thermdat_dry_long, thermdat_wet_long, thermdat_moderate_long)
head(thermdat_long)

# format columns
thermdat_long$timestamp <- as.POSIXct(thermdat_long$timestamp, format="%Y/%m/%d %H:%M:%S")
for(col in c('Vin', 'temperature')) {
  thermdat_long[[col]] <- as.numeric(thermdat_long[[col]])
}
for(col in c('file', 'pot', 'position', 'treatment')) {
  thermdat_long[[col]] <- as.factor(thermdat_long[[col]])
}

# remove obviously bad measurements
thermdat_long <- subset(thermdat_long, temperature < 35 & temperature > 0)


ggplot(thermdat_long, aes(x=timestamp, y=temperature, color=pot)) +
  geom_point() +
  facet_wrap(~treatment) +
  scale_x_datetime(date_labels = "%H", breaks = '12 hours')

thermdat_long %>% 
  group_by(treatment, position) %>% 
  summarize(mean_temp=mean(temperature), min_temp=min(temperature), max_temp=max(temperature))
















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