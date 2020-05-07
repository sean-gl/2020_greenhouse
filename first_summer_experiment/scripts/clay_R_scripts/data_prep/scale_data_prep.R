# Clay Bliss
# Script to pre-process scale data from Gleason's 2019 greenhouse experiment.
# Data is 30-second interval. 

# -----------------------------------

rm(list = ls())


### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')
Sys.getenv('TZ') # make sure it got set

# load pacakges
require(lubridate)
require(plyr)
require(tidyr)
require(ggplot2)

wd <- "/home/sean/github/2020_greenhouse/first_summer_experiment/data/scale_output/read_only/"
setwd(wd)

###
### ------Read and process data
###

### Experiment 1 data ----

# read in files
setwd(wd)
files <- dir(wd)
files <- files[grepl('.csv', files)]
files

datList <- lapply(files, function(x) read.csv(x, header = F, skipNul = F, stringsAsFactors = F))
names(datList) <- files
for(i in 1:length(datList)) {
  datList[[i]][,4] <- files[i]
  colnames(datList[[i]]) <- c('scale','timestamp','weight','file')
  datList[[i]]$timestamp <- as.POSIXct(datList[[i]]$timestamp, format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
}

### ------Preliminary combine data and check for duplicate rows
scaledat<- do.call(rbind, datList)
rownames(scaledat) <- NULL

# Check for duplicates and remove them.
dups <- duplicated(scaledat[,c('scale','timestamp')])
table(scaledat$file[dups]) # a bunch of duplicates in this file, remove them
scaledat <- scaledat[!dups, ]
# check
dups <- duplicated(scaledat[,c('scale','timestamp')])
any(dups)

### NOTE: Clock was set to MST, change to GMT (no daylight savings) for consistency with 2nd fall experiment
head(scaledat$timestamp)
scaledat$timestamp <- scaledat$timestamp - 3600
head(scaledat$timestamp)


# remove rows with no scale number
which(is.na(scaledat$scale))
scaledat[is.na(scaledat$scale),] 
scaledat <- scaledat[!is.na(scaledat$scale), ]

# Order data by timestamp and scale number
scaledat <- scaledat[order(scaledat$timestamp, scaledat$scale),  ]


### ------ Change data types and further data "massaging"
scaledat$scale <- factor(scaledat$scale)



# Assign treatments
# NOTE: These didn't go into effect until 8/24, irrig. at 18:00 MST
scaledat[scaledat$scale %in% 1:4, 'treatment'] <- 'moderate_drought'
scaledat[scaledat$scale %in% 5:8, 'treatment'] <- 'full_drought'
scaledat[scaledat$scale %in% c(9, 10, 12, 14), 'treatment'] <- 'well_watered'
scaledat$date <- lubridate::date(scaledat$timestamp)

# reset rownames
rownames(scaledat) <- NULL


### Examine data
ggplot(scaledat, aes(x=timestamp, y=weight, color=scale)) + geom_line() + facet_grid(~treatment)



# first, add all pot IDs regardless of date.
table(scaledat$treatment, scaledat$scale)
scaledat$plant_id <- sapply(as.character(scaledat$scale), function(x) switch(x,
                                                                             '1'='M-2', '2'='M-1','3'='M-3','4'='M-4',
                                                                             '5'='D-3','6'='D-4','7'='D-1','8'='D-2',
                                                                             '9'='W-4','10'='W-1','12'='W-2','14'='W-3'))
table(scaledat$plant_id, scaledat$scale)

# add block
scaledat$block <- as.factor(substr(scaledat$plant_id, 1, 1))


# Checking irrigation amounts/times
sub = subset(scaledat, treatment == 'full_drought' & date <= '2019-08-25')
sub = subset(scaledat, treatment == 'moderate_drought' & date >= '2019-08-20') 

ggplot(sub, aes(x=timestamp, y=weight, color=plant_id)) + geom_line() + scale_x_datetime(breaks = '1 day')

# add minutes column
scaledat$minutes <- hour(scaledat$timestamp)*60 + minute(scaledat$timestamp)

# calculate irrigation by scale
irrig <- ddply(scaledat, .(date, block, plant_id, scale), function(x){
  y <- subset(x, minutes >= 17*60 & minutes <= 19*60)
  # y <- x
  setNames(max(y$weight, na.rm = T) - min(y$weight, na.rm = T), 'irrig')
})

sub = subset(irrig, date >= '2019-08-23')
ggplot(sub, aes(x=date, y=irrig, color=plant_id)) + geom_line() + geom_point()

sub = subset(irrig, block=='D')
ggplot(sub, aes(x=date, y=irrig, color=plant_id)) + geom_line() + geom_point()

sub = subset(irrig, block=='W')
ggplot(sub, aes(x=date, y=irrig, color=plant_id)) + geom_line() + geom_point()


#### Save the compiled file before doing more work!
saveRDS(scaledat, "/home/sean/github/2020_greenhouse/first_summer_experiment/data/scale_output/scale_data_compiled_raw_long.rds")

# read back in
# scaledat <- readRDS("/home/sean/github/2020_greenhouse/first_summer_experiment/data/scale_output/scale_data_compiled_raw_long.rds")



### ---- Aggregate (15 minutes) and Flag 

# source the function 'aggAndFlag'
source("/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/data_prep/flagging_functions.R")

# First, subset data to 8/24 and beyond (start of treatments was 8/24)
scaledat <- subset(scaledat, date >= '2019-08-24')

# note: this function creates flags 
aggDat <- aggAndFlag(df = scaledat,
                     timeCol = 'timestamp', 
                     idCols = c('scale','plant_id','block','treatment'),
                     measCol = 'weight',
                     threshVec = c(.03, .04, .05),
                     interval = 15,
                     diffFn = maxFn)
table(aggDat$flag)

# rename columns
colnames(aggDat)[colnames(aggDat) %in% 'mean'] <- 'mean_weight_kg'
colnames(aggDat)[colnames(aggDat) %in% 'roundTime'] <- 'by15'
  
# SAve aggregated and flagged dataframe
saveRDS(aggDat, "/home/sean/github/2020_greenhouse/first_summer_experiment/data/scale_output/scale_data_long_aggflag.rds")

# read back in
# aggDat <- readRDS("/home/sean/github/2020_greenhouse/first_summer_experiment/data/scale_output/scale_data_long_aggflag.rds")

### Add "manual" flags

### Manual flag for irrigation 

# add hour and date columns
aggDat$date <- lubridate::date(aggDat$by15)
aggDat$hour <- lubridate::hour(aggDat$by15)



# create flags "irrig1", "irrig2", "irrig3" for 1st 3 hours after irrigation
# irrigation was at 18:00 each day (MST)
for(i in 1:3) {
  aggDat$flag[aggDat$hour == 18+i-1] <- paste0('irrig', i)
}

table(aggDat$flag)


### Finally, save file with all flags
saveRDS(aggDat, "/home/sean/github/2020_greenhouse/first_summer_experiment/data/scale_output/scale_data_long_aggflag.rds")






