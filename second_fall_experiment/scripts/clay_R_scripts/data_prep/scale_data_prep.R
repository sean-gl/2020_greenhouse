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

wd <- "/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/read_only/"
setwd(wd)

###
### ------Read and process data
###

### Experiment 2 data ----

# read in files
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
scaledat <- do.call(rbind, datList)
rownames(scaledat) <- NULL

# NOTE: these duplicate rows will be fixed below...time zone issue.
anyDuplicated(scaledat[,c('scale','timestamp')])



### ------ Fix time zone issues with certain csv files


# NOTE: This file has a "jump back" 1 hour for daylight savings time
head(datList$`scale_output_wet_moderate_11-04.csv`)
tail(datList$`scale_output_wet_moderate_11-04.csv`)
x = datList$`scale_output_wet_moderate_11-04.csv`
x$d = c(0, diff(x$timestamp))
which(x$d == -3570) # time set back 1 hour here (3600 seconds minus 30 seconds = 3570)
x[57720:57750,]
x$timestamp[57737:nrow(x)] <- x$timestamp[57737:nrow(x)] + 3600
x[57720:57750,]
x$d <- NULL
datList$`scale_output_wet_moderate_11-04.csv` <- x
rm(x)
head(datList$`scale_output_wet_moderate_11-04.csv`)
tail(datList$`scale_output_wet_moderate_11-04.csv`) # should be 8 am now

# Re-Combine all data
scaledat <- do.call(rbind, datList)
rownames(scaledat) <- NULL

# remove the huge list 
rm(datList)

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
scaledat[dups,]
# remove one duplicated row 
scaledat <- scaledat[!dups, ]

# verify that data is now 1 hour earlier than above
head(scaledat[scaledat$file == "scale_output_wet_moderate_10-23_MDT.csv", ])

# remove rows with no scale number
which(is.na(scaledat$scale))
scaledat[is.na(scaledat$scale),] 
scaledat <- scaledat[!is.na(scaledat$scale), ]

### Subset Maria's scale data (to send to her)
# maria_scaledat <- subset(scaledat, scale %in% 15:16)
# maria_scaledat$file <- NULL
# write.csv(maria_scaledat, "/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/maria_scale_data_raw_compiled.csv", row.names = F)

# Order data by timestamp and scale number
scaledat <- scaledat[order(scaledat$timestamp, scaledat$scale),  ]


### ------ Change data types and further data "massaging"
scaledat$scale <- factor(scaledat$scale)

# NOTE: these are blocks, not treatments!!
scaledat[scaledat$scale %in% c(1:4, 15), 'block'] <- 'W'
scaledat[scaledat$scale %in% 5:8, 'block'] <- 'M'
scaledat[scaledat$scale %in% c(9, 10, 12, 14, 16), 'block'] <- 'D'
scaledat$block <- factor(scaledat$block)
scaledat$date <- lubridate::date(scaledat$timestamp)
# scaledat$hour <- lubridate::hour(scaledat$timestamp)
# scaledat$minute <- lubridate::minute(scaledat$timestamp)
# scaledat$timeofday <- scaledat$hour * 60 + scaledat$minute




# remove erroneous dates
summary(scaledat$date)
# these are bad data (I checked original csv file, date is wrong there), remove them
ind <- scaledat$date == '2019-02-14'
scaledat[ind,]
scaledat <- scaledat[!ind, ]

# reset rownames
rownames(scaledat) <- NULL

### Clay added on 4/20/20: A couple scales (6, 8) data was offset, since a logger box fell on them.
### Let's fix this...

x <- subset(scaledat, scale %in% c(6,8) & date >= '2019-11-15' & date <= '2019-11-18')
ggplot(x, aes(x=timestamp, y=weight, color=scale)) + geom_point()

x <- subset(scaledat, scale %in% c(6,8) & date == '2019-11-18')
ggplot(x, aes(x=timestamp, y=weight, color=scale)) + geom_point()

# looks like the box fell on 11/16, before noon...
y <- subset(scaledat, scale %in% c(6,8) & timestamp >= '2019-11-16 10:30' & 
              timestamp <= '2019-11-16 10:45')
ggplot(y, aes(x=timestamp, y=weight, color=scale)) + geom_point()
plot(y$timestamp, y$weight); identify(y$timestamp, y$weight, labels = y$timestamp)
# first measurement after box fell
start <- as.POSIXct('2019-11-16 10:38:23', tz='GMT')
# get the shift in weight
diff(y[y$scale==6,'weight']) # adjust by 0.096 + 0.025 = 0.121
diff(y[y$scale==8,'weight']) # adjust by 0.173

# now find end
y <- subset(scaledat, scale %in% c(6,8) & timestamp >= '2019-11-18 16:00' & 
              timestamp <= '2019-11-18 16:30')
ggplot(y, aes(x=timestamp, y=weight, color=scale)) + geom_point()
plot(y$timestamp, y$weight); identify(y$timestamp, y$weight, labels = y$timestamp)
# last meas. after box fell
end <- as.POSIXct('2019-11-18 16:24:01', tz='GMT')

# adjust scale 6 
ind <- with(scaledat, scale == 6 & timestamp >= start & timestamp <= end)
scaledat$weight[ind] <-  scaledat$weight[ind] - 0.121
ind <- with(scaledat, scale == 8 & timestamp >= start & timestamp <= end)
scaledat$weight[ind] <-  scaledat$weight[ind] - 0.173

# Check the plots 
y <- subset(scaledat, scale %in% c(6,8) & timestamp >= '2019-11-18 16:00' & 
              timestamp <= '2019-11-18 16:30')
ggplot(y, aes(x=timestamp, y=weight, color=scale)) + geom_point()
# it's not perfect but better than before!



## Add plant IDs...
# Note: D, M blocks did not change plant IDs, but W block did (when virgin plants were swapped in) 
# Note: Did not keep track of plant IDs on Maria's scales (15, 16)

# first, add all pot IDs regardless of date.
scaledat$plant_id <- sapply(as.character(scaledat$scale), function(x) switch(x,
    '1'='W-6', '2'='W-7','3'='W-10','4'='W-11','5'='M-6','6'='M-7','7'='M-10','8'='M-11',
    '9'='D-6','10'='D-7','12'='D-10','14'='D-11','15'='border_plant_full_drought','16'='border_plant_well_watered'))
# check work
table(scaledat$plant_id, scaledat$scale)

# find times each scale was swapped out with a virgin
x <- subset(scaledat, timestamp >= '2019-11-27 13:00' & timestamp <= '2019-11-27 17:00')
s <- 1
d <- x[x$scale == s, ]
# plot(d$timestamp, d$weight, main = s, ylim = c(10, 13))
# identify(d$timestamp, d$weight, labels = d$timestamp)
# 15:20 is transition time for scale 1 (old plant W-6)
ind <- which(scaledat$plant_id == 'W-6' & scaledat$timestamp >= '2019-11-27 15:20')
head(scaledat[ind,]); tail(scaledat[ind,])
scaledat[ind, 'plant_id'] <- 'W-26'

s <- 2
d <- x[x$scale == s, ]
# plot(d$timestamp, d$weight, main = s, ylim = c(10, 13))
# identify(d$timestamp, d$weight, labels = d$timestamp, offset = 1)
# 14:20 is transition time for scale 2 (old plant W-7)
ind <- which(scaledat$plant_id == 'W-7' & scaledat$timestamp >= '2019-11-27 14:20')
head(scaledat[ind,]); tail(scaledat[ind,])
scaledat[ind, 'plant_id'] <- 'W-28'

s <- 3
d <- x[x$scale == s, ]
# plot(d$timestamp, d$weight, main = s, ylim = c(10, 13))
# identify(d$timestamp, d$weight, labels = d$timestamp, offset = 1)
# 15:40 is transition time for scale 3 (old plant W-10)
ind <- which(scaledat$plant_id == 'W-10' & scaledat$timestamp >= '2019-11-27 15:40')
head(scaledat[ind,]); tail(scaledat[ind,])
scaledat[ind, 'plant_id'] <- 'W-25'

s <- 4
d <- x[x$scale == s, ]
# plot(d$timestamp, d$weight, main = s, ylim = c(10, 13))
# identify(d$timestamp, d$weight, labels = d$timestamp, offset = 1)
# 14:40 is transition time for scale 4
ind <- which(scaledat$plant_id == 'W-11' & scaledat$timestamp >= '2019-11-27 14:40')
head(scaledat[ind,]); tail(scaledat[ind,])
scaledat[ind, 'plant_id'] <- 'W-27'

# check work
table(scaledat$plant_id, scaledat$scale)

### Add Current Treatment Column (based on date only, not time)

scaledat$treatment <- NA
# baseline (pre-treatments)
scaledat$treatment[scaledat$date <= '2019-10-24'] <- 'well_watered'
# treatment 1
scaledat$treatment[scaledat$date > '2019-10-24' & scaledat$date <= '2019-11-04' & scaledat$block == 'W'] <- 'well_watered'
scaledat$treatment[scaledat$date > '2019-10-24' & scaledat$date <= '2019-11-04' & scaledat$block == 'M'] <- 'moderate_drought'
scaledat$treatment[scaledat$date > '2019-10-24' & scaledat$date <= '2019-11-04' & scaledat$block == 'D'] <- 'full_drought'
# treatment 2
scaledat$treatment[scaledat$date > '2019-11-04' & scaledat$date <= '2019-11-27' & scaledat$block == 'W'] <- 'full_drought'
scaledat$treatment[scaledat$date > '2019-11-04' & scaledat$date <= '2019-11-27' & scaledat$block == 'M'] <- 'moderate_drought'
scaledat$treatment[scaledat$date > '2019-11-04' & scaledat$date <= '2019-11-27' & scaledat$block == 'D'] <- 'well_watered'
# treatment 3 (final)
scaledat$treatment[scaledat$date > '2019-11-27' & scaledat$block == 'W'] <- 'virgin_drought'
scaledat$treatment[scaledat$date > '2019-11-27' & scaledat$block == 'M'] <- 'well_watered'
scaledat$treatment[scaledat$date > '2019-11-27' & scaledat$block == 'D'] <- 'full_drought'
# convert to factor
scaledat$treatment <- as.factor(scaledat$treatment)
table(scaledat$treatment, useNA = 'always')



#### Save the compiled file before doing more work!
saveRDS(scaledat, "/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_compiled_raw_long.rds")

# read back in
# scaledat <- readRDS("/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_compiled_raw_long.rds")



### ---- Aggregate (15 minutes) and Flag 

# source the function 'aggAndFlag'
source("/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/data_prep/flagging_functions.R")

# note: this function creates flags 
aggDat <- aggAndFlag(df = scaledat,
                     timeCol = 'timestamp', 
                     idCols = c('scale','plant_id','treatment','block'),
                     measCol = 'weight',
                     threshVec = c(.03, .04, .05),
                     interval = 15,
                     diffFn = maxFn)

# rename column
colnames(aggDat)[colnames(aggDat) %in% 'mean'] <- 'mean_weight_kg'

# SAve aggregated and flagged dataframe
saveRDS(aggDat, "/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_long_aggflag.rds")

# read back in
# aggDat <- readRDS("/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_long_aggflag.rds")

### Add "manual" flags

### Manual flag for irrigation 

# add hour and date columns
aggDat$date <- lubridate::date(aggDat$roundTime)
aggDat$hour <- lubridate::hour(aggDat$roundTime)

# create flags "irrig1", "irrig2", "irrig3" for 1st 3 hours after irrigation
for(i in 1:3) {
  aggDat$flag[aggDat$date < '2019-10-23' &
                aggDat$hour == 0+i-1] <- paste0('irrig', i)
  aggDat$flag[aggDat$date >= '2019-10-23' &
                aggDat$date < '2019-10-28' &
                aggDat$hour == 18+i-1] <- paste0('irrig', i)
  aggDat$flag[aggDat$date >= '2019-10-28' &
                aggDat$date < '2019-11-27' &
                aggDat$hour == 19+i-1] <- paste0('irrig', i)
  aggDat$flag[aggDat$date >= '2019-11-27' &
                aggDat$hour == 20+i-1] <- paste0('irrig', i)
}

table(aggDat$flag)


### Now add some flags for dates/times with KNOWN bad data 
aggDat$flag[aggDat$date == '2019-09-30' & aggDat$hour %in% 12:23] <- 'man'
aggDat$flag[aggDat$date == '2019-10-01' & aggDat$hour %in% 10:23] <- 'man'
aggDat$flag[aggDat$date == '2019-10-21' & aggDat$block == 'M'] <- 'man'
aggDat$flag[aggDat$date == '2019-10-21' & aggDat$scale %in% c(4, 10)] <- 'man'



## PLOT DATA
# b <- 'W'
# d <- '2019-12-12'
# sub <- subset(aggDat, date == d & block == b)
# fl <- subset(sub, flag != 0)
# ggplot(sub, aes(x=roundTime, y=mean_weight_kg, color=scale)) +
#   geom_point()
# ggplot(fl, aes(x=roundTime, y=mean_weight_kg, color=scale)) +
#   geom_point()

# ggplot(sub, aes(x=roundTime, y=mean_weight_kg, color=flag)) +
#   geom_point() +
#   facet_wrap(~scale)
#   
# 
# subraw <- subset(scaledat, date == d & block == b)
# ggplot(subraw, aes(x=timestamp, y=weight, color=scale)) +
#   geom_point()


### Finally, save file with all flags
saveRDS(aggDat, "/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_long_aggflag.rds")









### TODO

# ### Experiment 1 data ----
# 
# # read in files
# wd <- "/home/wmsru/Documents/Clay/greenhouse/2019 greenhouse data/experiment1/scale_data/"
# setwd(wd)
# files <- dir(wd)
# files <- files[grepl('.csv', files)]
# files
# 
# datList <- lapply(files, function(x) read.csv(x, header = F, skipNul = F, stringsAsFactors = F))
# names(datList) <- files
# for(i in 1:length(datList)) {
#   datList[[i]][,4] <- files[i]
#   colnames(datList[[i]]) <- c('scale','timestamp','weight','file')
#   datList[[i]]$timestamp <- as.POSIXct(datList[[i]]$timestamp, format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
# }
# 
# ### ------Preliminary combine data and check for duplicate rows
# scaledat_exp1 <- do.call(rbind, datList)
# rownames(scaledat_exp1) <- NULL
# 
# # NOTE: these duplicate rows will be fixed below...time zone issue.
# anyDuplicated(scaledat_exp1[,c('scale','timestamp')])
# 
# dups <- duplicated(scaledat_exp1[,c('scale','timestamp')])
# View(scaledat_exp1[dups,])
