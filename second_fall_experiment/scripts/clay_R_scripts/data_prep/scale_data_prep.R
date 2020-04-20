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

z=subset(scaledat, scale==3 & date(timestamp) >= '2019-11-02' & date(timestamp) <= '2019-11-04')
plot(weight~timestamp,z, type='l')
z2=subset(x, scale==1 & date(timestamp) >= '2019-11-02' & date(timestamp) <= '2019-11-04')
points(weight~timestamp, z2, col='red')



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

#### Save the compiled file before doing more work!
saveRDS(scaledat, "/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_compiled_raw_long.rds")

# read back in
scaledat <- readRDS("/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_compiled_raw_long.rds")

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


### Save again, then convert to Wide format and save separately
saveRDS(scaledat, "/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_raw_compiled_long.rds")

# read data back in
# scaledat <- readRDS("/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_raw_compiled_long.rds")


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
# use 3-hour window
window <- 3
# add hour and date columns
aggDat$date <- lubridate::date(aggDat$roundTime)
aggDat$hour <- lubridate::hour(aggDat$roundTime)

aggDat$flag[aggDat$date < '2019-10-23' &
                       aggDat$hour %in% 0:(0+window-1)] <- 'irrig'
aggDat$flag[aggDat$date >= '2019-10-23' &
                       aggDat$date < '2019-10-28' &
                       aggDat$hour %in% 18:(18+window-1)] <- 'irrig'
aggDat$flag[aggDat$date >= '2019-10-28' &
                       aggDat$date < '2019-11-27' &
                       aggDat$hour %in% 19:(19+window-1)] <- 'irrig'
aggDat$flag[aggDat$date >= '2019-11-27' &
                       aggDat$hour %in% 20:(20+window-1)] <- 'irrig'
table(aggDat$flag)


### Now add some flags for dates/times with KNOWN bad data 
aggDat$flag[aggDat$date == '2019-09-30' & aggDat$hour %in% 12:23] <- 'man'
aggDat$flag[aggDat$date == '2019-10-01' & aggDat$hour %in% 10:23] <- 'man'
aggDat$flag[aggDat$date == '2019-10-21' & aggDat$block == 'M'] <- 'man'
aggDat$flag[aggDat$date == '2019-10-21' & aggDat$scale %in% c(4, 10)] <- 'man'



## PLOT DATA
b <- 'W'
d <- '2019-12-12'
sub <- subset(aggDat, date == d & block == b)
# fl <- subset(sub, flag != 0)
# ggplot(sub, aes(x=roundTime, y=mean_weight_kg, color=scale)) +
#   geom_point()
# ggplot(fl, aes(x=roundTime, y=mean_weight_kg, color=scale)) +
#   geom_point()

ggplot(sub, aes(x=roundTime, y=mean_weight_kg, color=flag)) +
  geom_point() +
  facet_wrap(~scale)
  

subraw <- subset(scaledat, date == d & block == b)
ggplot(subraw, aes(x=timestamp, y=weight, color=scale)) +
  geom_point()


### Finally, save file with all flags
saveRDS(aggDat, "/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_long_aggflag.rds")








#### OLD CODE BELOW, NOT USED ......................



# rename file before converting to wide format
scaledat_long <- scaledat
scaledat_long <- subset(scaledat_long, select = c(scale, date, timestamp, weight))
# scaledat_long$weight <- scaledat_long$weight + rnorm(nrow(scaledat_long), 0, .5)

# In order to save as wide format, we must round the times to nearest measurement interval (30 seconds)
scaledat_long$timestamp <- lubridate::round_date(scaledat_long$timestamp, unit = '30 seconds')

### Need to deal with duplicated rows in order to convert to WIDE format below...
dups_ind <- duplicated(scaledat_long[ ,c('timestamp','scale')])
length(which(dups_ind))
dups2_ind <- duplicated(scaledat_long[ ,c('timestamp','scale')], fromLast = TRUE)
length(which(dups2_ind))

dups <- scaledat_long[dups_ind, ]
dups2 <- scaledat_long[dups2_ind, ]

# TODO: NOTE: these have similar weights but not exactly same -- average them? need to make function...
ind1 <- which(dups_ind)
ind2 <- which(dups2_ind)
scaledat_long[ind1[1],]
scaledat_long[ind2[1],]

# calculate means of duplicated rows
means <- data.frame(mean_weight=rep(NA, length(ind1)))
means$ind1 <- ind1 # original row number in scaledat_long df, for 1st duplicate row of pair
means$ind2 <- ind2 # original row number in scaledat_long df, for 2nd duplicate row of pair
for(i in 1:nrow(dups)) {
  s <- dups$scale[i]
  ts <- dups$timestamp[i]
  wt1 <- dups$weight[i]
  ind <- which(dups2$scale == s & dups2$timestamp == ts)
  wt2 <- dups2$weight[ind]
  means$diff_weight[i] <- wt1 - wt2
  means$mean_weight[i] <- mean(wt1, wt2)
}

# Replace the duplicate values in the original data frame
for(i in 1:nrow(means)) {
  mn <- means[i, 'mean_weight']
  # replace 1st duplicate row with mean weight
  scaledat_long[means[i, 'ind1'], 'weight'] <- mn
  # replace 2nd dup row with NA (then do complete cases below)
  scaledat_long[means[i, 'ind2'], ] <- NA
}

# check that NA rows are correct
scaledat_long_test <- scaledat_long[1:5,]
NArows <- unname(apply(scaledat_long, 1, function(x) all(is.na(x))))
identical(which(NArows), means$ind2) # should be TRUE

# drop NA rows
scaledat_long <- scaledat_long[!NArows, ]

# check for duplicates again (shouldn't be any)
dups_ind_new <- duplicated(scaledat_long[ ,c('timestamp','scale')])
which(dups_ind_new)


### Convert long format to WIDE and save RDS
scaledat_wide <- tidyr::spread(scaledat_long, key = scale, value = weight)

saveRDS(scaledat_wide, "/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_30second_wide.rds")

# subset and save for Maria
# maria_scaledat_wide <- subset(scaledat_wide, date >= '2019-11-08')
# maria_scaledat_wide <- maria_scaledat_wide %>% select(-one_of(c('date', 'hour','by15')))
# write.csv(maria_scaledat_wide, "/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_30second_wide_maria.csv", row.names = F)
# read data back in 
# scaledat_wide <- readRDS("/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_30second_wide.rds")



##### ----------- Filter the data (omit erroneous data) ----------------

### test a small subset
# scaledat_wide$hour <- lubridate::hour(scaledat_wide$timestamp)
# scale <- '7'
# testdat <- subset(scaledat_wide[,c('timestamp','date','hour', scale)], date == '2019-11-08' & hour < 19 & hour > 0)
# testdat$diff <- c(diff(testdat[,scale]), NA)
# plot(testdat$timestamp, testdat[,scale], main = scale)
# plot(testdat$timestamp, testdat$diff)
# 
# threshold <- 0.015
# ind <- abs(testdat$diff) > threshold
# length(ind[which(ind)])
# # percent of observations above/below threshold
# length(ind[which(ind)]) / nrow(testdat) * 100
# which(ind)
# testdat[which(ind),]
# errors <- testdat[ind, ]
# plot(testdat$timestamp, testdat[,scale], main = scale)
# points(errors$timestamp, errors[,scale], col = 'red')


### Dates with funky stuff going on:

# 11/8: SCALE 2 REALLY STRANGE, DID THIS GET ZEROED OR MOVED TODAY?
# scales 1,2,4, others...Note strange drop at 12:59 (scales 4, 6, 7...)


### round timestamps to nearest 15 minutes
# testdat$by15 <- lubridate::floor_date(testdat$timestamp, unit = '15 minutes')
# # get 15-min means
# require(plyr)
# x <- ddply(testdat, .(by15), function(x) mean(x[,scale], na.rm = TRUE))
# x$diff <- c(NA, diff(x$V1))
# plot(x$by15, x$V1)
# plot(x$by15, x$diff, ylim = c(-.01, 0.01))


### Add column to indicate watering times

# Use this code to plot data to check watering times
# s = as.character(5)
# x=subset(scaledat_wide, date >= '2019-12-10' & date <='2019-12-12')
# plot(x$timestamp, x[ ,s])
# times <- identify(x$timestamp, x[,s], labels = x$timestamp)
# x[times, 'timestamp']





### Create difference matrix for all scales
diffs <- apply(scaledat_wide[ , as.character(c(1:10, 12, 14:16))], 2, diff)
diffs <- rbind(NA, diffs)
diffs <- as.data.frame(diffs, col.names = as.character(c(1:10, 12, 14:16)))
diffs <- cbind(scaledat_wide[ , c('date','timestamp')], diffs)


### Get 15-minute mean weights 
scaledat_wide$by15 <- lubridate::floor_date(scaledat_wide$timestamp, unit = '15 minutes')

weights15 <- ddply(scaledat_wide[ , c('by15', as.character(c(1:10, 12, 14:16)))], .(by15), 
                   colwise(mean, na.rm = TRUE))

# test the above 
# mean(scaledat_wide[scaledat_wide$by15==unique(scaledat_wide$by15)[5], '5'], na.rm = T)
# length(unique(scaledat_wide$by15))

### Get 15-minute max diffs
diffs$by15 <- lubridate::floor_date(diffs$timestamp, unit = '15 minutes')
diffs$timediff <- c(NA, as.numeric(diff(diffs$timestamp))) 
absMax <- function(x) max(abs(x), na.rm = T)
absmaxdiff15 <- ddply(diffs[ , c('by15', as.character(c(1:10, 12, 14:16)))],
                      .(by15), 
                      colwise(absMax))
unique(warnings())
absmaxdiff15$timediff <- c(NA, as.numeric(diff(absmaxdiff15$by15))) 



### Create a dataframe of flags, if the max diff > a threshold 
### and if the timestep is correct (15 minutes)...not a gap!
flags <- apply(absmaxdiff15[ , as.character(c(1:10, 12, 14:16))], 2, function(x) {
  sapply(1:length(x), function(i) {
    if(absmaxdiff15[i, 'timediff'] == 15 & !is.na(absmaxdiff15[i, 'timediff'])) {
      
      if(x[i] >= 0.04) {
        return(3)
      } else if(x[i] >= 0.03) {
        return(2)
      } else if(x[i] >= 0.01) {
        return(1)
      } else return(0)
      
    } else return(0)
  })
})

dim(flags)
head(flags)
table(as.numeric(flags))

### Second set of flags for overall absolute increase (not 1st order) over 15-minute period.
max_overall <- function(x) {
  out <- max(x, na.rm = T) - min(x, na.rm = T)
  if(is.infinite(out)) out <- NaN
  return(out)
}
absmaxdiff15_overall <- ddply(scaledat_wide[ , c('by15', as.character(c(1:10, 12, 14:16)))],
                              .(by15), 
                              colwise(max_overall))

summary(absmaxdiff15_overall)
absmaxdiff15_overall$timediff <- c(NA, as.numeric(diff(absmaxdiff15_overall$by15))) 

flags_overall <- apply(absmaxdiff15_overall[ , as.character(c(1:10, 12, 14:16))], 2, function(x) {
   sapply(1:length(x), function(i) {
    if(absmaxdiff15_overall[i, 'timediff'] == 15 & !is.na(absmaxdiff15_overall[i, 'timediff']) &
       !is.na(x[i])) {
      
      if(x[i] >= 0.05) {
        return(3)
      } else if(x[i] >= 0.04) {
        return(2)
      } else if(x[i] >= 0.03) {
        return(1)
      } else return(0)
      
    } else return(0)
  })
})

### Create irrigation flags

# Create key date-times when watering time was changed
# irrig_times <- as.POSIXct(c('2019-09-14 24:00', 
#                             '2019-10-23 18:00',
#                             '2019-10-28 19:00',
#                             '2019-11-27 20:00'), tz = 'GMT')

# 3-hour window (allow for drainage)
window <- 3

weights15$hour <- lubridate::hour(weights15$by15)

weights15$irrig_flag <- FALSE
weights15$irrig_flag[weights15$date < '2019-10-23' &
                       weights15$hour %in% 0:(0+window-1)] <- TRUE
weights15$irrig_flag[weights15$date >= '2019-10-23' &
                       weights15$date < '2019-10-28' &
                       weights15$hour %in% 18:(18+window-1)] <- TRUE
weights15$irrig_flag[weights15$date >= '2019-10-28' &
                       weights15$date < '2019-11-27' &
                       weights15$hour %in% 19:(19+window-1)] <- TRUE
weights15$irrig_flag[weights15$date >= '2019-11-27' &
                       weights15$hour %in% 20:(20+window-1)] <- TRUE


### Plot some flagged data

# subset of data with flagged values 
weights15$date <- lubridate::date(weights15$by15)
flag_value <- 3
scale <- as.character(2)
ss <- weights15[, c('by15', 'date', scale, 'irrig_flag')]
table(ss[flags[,scale] == flag_value, 'date'])
ss_flagged <- ss[flags[,scale] == flag_value,]
# ss_flagged <- ss
testdate <- '2019-11-06'
ss <- subset(ss, date == testdate)
ss_flagged <- subset(ss_flagged, date == testdate & irrig_flag == F)

plot(ss$by15, ss[,scale], main=paste(testdate, scale, flag_value, sep = ', '))
points(ss_flagged$by15, ss_flagged[,scale], col='red', pch = 16)

## Repeat above with second set of "absolute" flags
# subset of data with flagged values 
weights15$date <- lubridate::date(weights15$by15)
flag_values <- 2:3
scale <- as.character(5)
ss <- weights15[, c('by15', 'date', scale, 'irrig_flag')]
table(ss[flags_overall[,scale] %in% flag_values, 'date'])
ss_flagged <- ss[flags_overall[,scale] %in% flag_values |
                   ss$irrig_flag == TRUE,]
testdate <- '2019-10-21'
ss <- subset(ss, date == testdate)
ss_flagged <- subset(ss_flagged, date == testdate)

plot(ss$by15, ss[,scale], main=paste(testdate, scale, flag_values, sep = ', '))
points(ss_flagged$by15, ss_flagged[,scale], col='red', pch = 16)


### Plot raw data on date thermistors were checked and compare to flags
dat <- subset(scaledat_long, date >= '2019-11-20' & date <= '2019-12-05' &
                scale %in% 2)
ggplot(dat, aes(x=timestamp, y=weight, color=scale)) + geom_line() +
  ylim(c(10,12)) 


### ---- Finally, Compile Flags into dataframe and save flags + 15-minute means.

saveRDS(weights15, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_compiled_15min_means.rds')

flags_overall_2 <- as.data.frame(flags_overall)
flags_overall_2$by15 <- weights15$by15
saveRDS(flags_overall_2, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_compiled_15min_means_FLAGS.rds')



### Now calculate transpiration at 15-minute time steps

et15 <- apply(weights15[ , as.character(c(1:10, 12, 14:16))], 2, diff)
et15 <- as.data.frame(rbind(NA, et15))
et15 <- cbind(weights15$by15, et15)
colnames(et15)[1] <- 'by15'
et15$date <- lubridate::date(et15$by15)
et15$hour <- lubridate::hour(et15$by15)

# change sign 
et15[ , as.character(c(1:10, 12, 14:16))] <- -(et15[ , as.character(c(1:10, 12, 14:16))])

# convert to long for plotting
et15_long <- tidyr::gather(et15, 'scale', 'ET', -c(by15, hour, date))

# remove scales 15, 16
et15_long <- subset(et15_long, !scale %in% 15:16)

# add blocks
et15_long[et15_long$scale %in% c(1:4, 15), 'block'] <- 'W' 
et15_long[et15_long$scale %in% 5:8, 'block'] <- 'M' 
et15_long[et15_long$scale %in% c(9, 10, 12, 14, 16), 'block'] <- 'D' 

et15_long_sub <- subset(et15_long, date(by15) == '2019-12-04' & 
                          scale %in% 9:14)
ggplot(et15_long_sub, aes(x=by15, y=ET, color=scale)) +
  geom_point() + geom_line() + ylim(c(-.005, .03))


et15_long_sub <- subset(et15_long, hour > 5 & hour < 18)
large_values <- subset(et15_long_sub, ET > 0.03)
with(large_values, table(date, scale))
with(et15_long_sub, table(date, scale))

et_den <- density(subset(et15_long, hour > 5 & hour < 19)[['ET']], na.rm = T)
et_den
plot(et_den)


### Check some dates with lots of errors (don't worry about before I covered pots on 10-21).

dat <- subset(et15_long_sub, date(by15) == '2019-11-09' & 
                  as.numeric(scale) %in% 1:4)
ggplot(dat, aes(x=by15, y=ET, color=scale)) +
  geom_point() + geom_line() + NULL


### Plot all data
ggplot(et15_long, aes(x=by15, y=ET, color=scale)) +
  geom_point() + geom_line()


### Calculate mean hourly ET by block and plot these
block_means <- ddply(et15_long, .(date, hour, block), function(x) {
  valid_weights <- x$ET[!is.nan(x$ET) & !is.na(x$ET)]
  mn <- mean(valid_weights)
  n <- length(valid_weights)
  data.frame(mean_ET = mn, n = n)
})

# create timestamp at top of hour
block_means$timestamp <- as.POSIXct(paste0(block_means$date, ' ', block_means$hour, ':00'), tz='GMT')

# subset to midday (hour == 13)
hr <- 11
block_means_midday <- subset(block_means, hour == hr)
ggplot(block_means_midday, aes(x=timestamp, y=mean_ET, color=block)) +
  geom_point() + geom_line() + scale_x_datetime(date_breaks = '1 week') +
  ggtitle(hr)
  # geom_jitter(aes(color=block)) 
  # facet_wrap(~block)

# Convert to GMT to match Sean's data...MOVE THIS TO END...
# old_timestamp <- scaledat$timestamp
# new_timestamp <- lubridate::with_tz(scaledat$time, 'GMT')
# head(old_timestamp)
# head(new_timestamp)
# scaledat$timestamp <- new_timestamp













##### OLD CODE BELOW.....




### Get 15-minute averages

scaleWide <- subset(scaledat, date >= "2019-11-15")
scaleWide <- scaleWide[ , c('scale','timestamp','weight')]
scaleWide <- tidyr::spread(scaleWide, key = scale, value = weight)
head(scaleWide)
formalArgs(SummarizeTimeseries)
source("/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/data_prep/timeSeriesAvg_fun.R")
scaleSummary <- SummarizeTimeseries(dat = scaleWide, 
                                    tsCol = 'timestamp',
                                    # measCols = as.character(c(1:10, 12, 14:16)),
                                    measCols = as.character(c(1:10, 12, 14)),
                                    interval = 15,
                                    fillGaps = TRUE)


# 15 min. mean values
scaleMeans <- scaleSummary$means

head(scaleMeans)

# 15 min. mean values sample sizes
scaleSS <- scaleSummary$sampleSizes


### Calculate diffs. for each scale


## TODO: Filter by sample size threshold first!

# calculate matrix of 1st differences for each column in ScaleMeans
diffMat <- apply(scaleMeans[ , as.character(c(1:10, 12, 14))], 2, diff)

# add back columns
transpiration <- data.frame(timestamp=scaleMeans$timestamp[2:nrow(scaleMeans)])
transpiration <- cbind(transpiration, diffMat)






















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
