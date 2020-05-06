
rm(list = ls())

### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')
Sys.getenv('TZ') # make sure it got set

require(dplyr); require(tidyr); require(ggplot2)

wd <- "/home/sean/github/2020_greenhouse/first_summer_experiment/data/leaf_thermistor_data/read_only/"
setwd(wd)

# read in files
files <- dir(wd)
files <- files[grepl('.TXT', files)]
files
# Note: For these 2 files I'm not 100% sure if they are assigned to the correct block, but it was 
# before treatments began anyway, so not super important:
# "leaf_thermistor_dry_east_08_21-23.TXT" and "leaf_thermistor_wet_west_08_21-23.TXT"

# read in all csv files into a list of df
datList <- lapply(files, function(x) {
  d <- read.csv(x, header = F, stringsAsFactors = F)
  d$file <- x
  return(d)
})

# combine list of df into 1 df
thermdat <- do.call(rbind, datList)


### ----- LIST OF DATA QUALITY CHECKS ------


### Back to the data...

# Assign block based on file name
thermdat$block <- NA
thermdat$block[grepl('wet', thermdat$file)] <- 'W'
thermdat$block[grepl('moderate', thermdat$file)] <- 'M'
thermdat$block[grepl('dry', thermdat$file)] <- 'D'
table(thermdat$block)

head(thermdat)

### Convert to long format after some processing...
# omit 1st column (this is the reference voltage which we don't need)
thermdat[,2] <- NULL
colnames(thermdat)[1:9] <- c('timestamp', 1:8)
thermdat$timestamp <- as.POSIXct(thermdat$timestamp, format="%Y/%m/%d %H:%M:%S", tz='GMT')
head(thermdat$timestamp)
# subtract 1 hour from timestamps to convert from MDT to "GMT"
thermdat$timestamp <- thermdat$timestamp - 3600

thermLong <- tidyr::gather(thermdat, key = 'thermistor', value = 'temp_C', -c(timestamp, file, block))
table(thermLong$thermistor)

# add plant ID
thermLong$plant_id <- NA

thermLong$plant_id[thermLong$block == 'D' & thermLong$thermistor %in% 1:2] <- 'D-2'
thermLong$plant_id[thermLong$block == 'D' & thermLong$thermistor %in% 3:4] <- 'D-1'
thermLong$plant_id[thermLong$block == 'D' & thermLong$thermistor %in% 5:6] <- 'D-3'
thermLong$plant_id[thermLong$block == 'D' & thermLong$thermistor %in% 7:8] <- 'D-4'

thermLong$plant_id[thermLong$block == 'M' & thermLong$thermistor %in% 1:2] <- 'M-2'
thermLong$plant_id[thermLong$block == 'M' & thermLong$thermistor %in% 3:4] <- 'M-1'
thermLong$plant_id[thermLong$block == 'M' & thermLong$thermistor %in% 5:6] <- 'M-3'
thermLong$plant_id[thermLong$block == 'M' & thermLong$thermistor %in% 7:8] <- 'M-4'

thermLong$plant_id[thermLong$block == 'W' & thermLong$thermistor %in% 1:2] <- 'W-2'
thermLong$plant_id[thermLong$block == 'W' & thermLong$thermistor %in% 3:4] <- 'W-1'
thermLong$plant_id[thermLong$block == 'W' & thermLong$thermistor %in% 5:6] <- 'W-3'
thermLong$plant_id[thermLong$block == 'W' & thermLong$thermistor %in% 7:8] <- 'W-4'

# check work
table(thermLong$plant_id)
table(thermLong$thermistor, thermLong$plant_id)

# add thermistor position column
thermLong$position <- NA
thermLong$position[thermLong$thermistor %in% c(1,3,5,7)] <- 'top'
thermLong$position[thermLong$thermistor %in% c(2,4,6,8)] <- 'bottom'
table(thermLong$thermistor, thermLong$position, useNA = 'a')
table(thermLong$plant_id, thermLong$position, useNA = 'a')

# add thermistor "canopy_position" column 
# NOTE: This column is redundant with "position" column, since plants were large enough to 
# place thermistors where they did not need to be moved later on, as in 2nd experiment.

thermLong$canopy_position[thermLong$position=='top'] <- 'upper'
thermLong$canopy_position[thermLong$position=='bottom'] <- 'lower'
table(thermLong$canopy_position, useNA = 'a')


# convert temperature to numeric
thermLong$temp_C <- as.numeric(thermLong$temp_C)

# remove "file" column before saving
thermLong$file <- NULL


### --- Fix some data with know issues ----

# this thermistor was broken, and fixed on 8/24 but then wired to the wrong plant! Let's just not use its data, since I'm not
# even sure from my notes what canopy position it was in, and I think the other plant already had 2 thermistors on
# it anyway.
ind <- thermLong$plant_id=='W-1' & thermLong$canopy_position=='upper'
which(ind)
with(thermLong, table(plant_id, canopy_position, useNA = 'a'))
thermLong <- thermLong[!ind, ]


### Save raw data in LONG format
saveRDS(thermLong, "/home/sean/github/2020_greenhouse/first_summer_experiment/data/leaf_thermistor_data/leaf_thermistor_data_raw_compiled_long.rds")

# thermLong <- readRDS("/home/sean/github/2020_greenhouse/first_summer_experiment/data/leaf_thermistor_data/leaf_thermistor_data_raw_compiled_long.rds")




### Do 15-minute aggregation and re-save
thermLong$by15 <- lubridate::ceiling_date(thermLong$timestamp, unit = '15 minutes')
aggTherm <- plyr::ddply(thermLong, .variables = c('by15', 'block', 'thermistor',
                                                  'plant_id', 'position', 'canopy_position'), 
                        function(x) {
                          setNames(mean(x$temp_C, na.rm = T), 'mean_leaftemp_C')
                        })
### Save aggregated data
saveRDS(aggTherm, "/home/sean/github/2020_greenhouse/first_summer_experiment/data/leaf_thermistor_data/leaf_thermistor_data_15min_agg.rds")

# aggTherm <- readRDS("/home/sean/github/2020_greenhouse/first_summer_experiment/data/leaf_thermistor_data/leaf_thermistor_data_15min_agg.rds")



# --- Flag some data that is known to be bad. 
# NOTE: I checked the thermistors only on 9/3, after spider mites were in full effect.
# So I have no way of knowing how long thermistor data was bad before 9/3. 
# I will conservatively assume here that data was ok through 8/28, since I was in the greenhouse on 8/24 and 
# Garret didn't see spider mites until late on the week of 8/28 (probably Thrusday or Friday)

# set timestamp index
ind1 <- aggTherm$by15 >= as.POSIXct('2019-08-29 00:00')

# Now, flag individual thermistors:
# Flags: "ok" = data good; "thermistor bad" = thermistor in bad position; 
# "leaf dead" = leaf entirely dead, including under thermistor
# "
aggTherm$flag <- 0

# D block
aggTherm$flag[ind1 & aggTherm$plant_id=='D-1' & aggTherm$position=='bottom'] <- 4
aggTherm$flag[ind1 & aggTherm$plant_id=='D-1' & aggTherm$position=='top'] <- 3
aggTherm$flag[ind1 & aggTherm$plant_id=='D-2' & aggTherm$position=='bottom'] <- 3
aggTherm$flag[ind1 & aggTherm$plant_id=='D-2' & aggTherm$position=='top'] <- 4
aggTherm$flag[ind1 & aggTherm$plant_id=='D-3' & aggTherm$position=='bottom'] <- 1
aggTherm$flag[ind1 & aggTherm$plant_id=='D-3' & aggTherm$position=='top'] <- 1
aggTherm$flag[ind1 & aggTherm$plant_id=='D-4' & aggTherm$position=='bottom'] <- 1
aggTherm$flag[ind1 & aggTherm$plant_id=='D-4' & aggTherm$position=='top'] <- 1

# W block
aggTherm$flag[ind1 & aggTherm$plant_id=='W-1' & aggTherm$position=='bottom'] <- 4
aggTherm$flag[ind1 & aggTherm$plant_id=='W-1' & aggTherm$position=='top'] <- 3
aggTherm$flag[ind1 & aggTherm$plant_id=='W-2' & aggTherm$position=='bottom'] <- 3
aggTherm$flag[ind1 & aggTherm$plant_id=='W-2' & aggTherm$position=='top'] <- 0
aggTherm$flag[ind1 & aggTherm$plant_id=='W-3' & aggTherm$position=='bottom'] <- 1
aggTherm$flag[ind1 & aggTherm$plant_id=='W-3' & aggTherm$position=='top'] <- 1
aggTherm$flag[ind1 & aggTherm$plant_id=='W-4' & aggTherm$position=='bottom'] <- 1
aggTherm$flag[ind1 & aggTherm$plant_id=='W-4' & aggTherm$position=='top'] <- 1

# M block
aggTherm$flag[ind1 & aggTherm$plant_id=='M-1' & aggTherm$position=='bottom'] <- 1
aggTherm$flag[ind1 & aggTherm$plant_id=='M-1' & aggTherm$position=='top'] <- 1
aggTherm$flag[ind1 & aggTherm$plant_id=='M-2' & aggTherm$position=='bottom'] <- 1
aggTherm$flag[ind1 & aggTherm$plant_id=='M-2' & aggTherm$position=='top'] <- 1
aggTherm$flag[ind1 & aggTherm$plant_id=='M-3' & aggTherm$position=='bottom'] <- 1
aggTherm$flag[ind1 & aggTherm$plant_id=='M-3' & aggTherm$position=='top'] <- 1
aggTherm$flag[ind1 & aggTherm$plant_id=='M-4' & aggTherm$position=='bottom'] <- 1
aggTherm$flag[ind1 & aggTherm$plant_id=='M-4' & aggTherm$position=='top'] <- 2

table(aggTherm$block, aggTherm$flag, useNA = 'a')



## Examine distribution of temperature data and create flags for obviously high/low temperatures
plot(density(aggTherm$mean_leaftemp_C, na.rm = T), xlim=c(-10, 50))

# try 40 degrees for high thershould...
hi <- with(aggTherm, mean_leaftemp_C >= 40 & !is.na(mean_leaftemp_C))
View(aggTherm[hi,]) 
# Lots of believable data between 40-44 degrees, looks like 45 C is a reasonable cutoff for obviously bad data

# VERDICT: USE > 45 C AS HIGH TEMPERATURE FLAG THRESHOLD.
hi <- with(aggTherm, mean_leaftemp_C >= 45 & !is.na(mean_leaftemp_C))
summary(aggTherm$mean_leaftemp_C[hi])

# now, see what is not believable for a low temp...
low <- with(aggTherm, mean_leaftemp_C < 0 & !is.na(mean_leaftemp_C))
length(which(low))
# there are many values below zero but only 1 between 0 and 10
# NOTE: values < 0 indicate faulty wiring of thermistor.
low <- with(aggTherm, mean_leaftemp_C > 0 & mean_leaftemp_C <= 10 & !is.na(mean_leaftemp_C))
length(which(low))
low <- with(aggTherm, mean_leaftemp_C > 10 & mean_leaftemp_C <= 18 & !is.na(mean_leaftemp_C))
length(which(low))
View(aggTherm[low,]) # these all look reasonable, so a good cutoff is temp < 10

# VERDICT: USE < 10 C AS LOW TEMPERATURE FLAG THRESHOLD.
low <- with(aggTherm, mean_leaftemp_C <= 10 & !is.na(mean_leaftemp_C))
summary(aggTherm$mean_leaftemp_C[low])




length(which(hi)) /nrow(aggTherm) * 100 # 0.2% of observations > 45 deg C
length(which(low)) /nrow(aggTherm) * 100 # 3 % of observations < 10 deg C

aggTherm$temperature_flag <- 'none'
aggTherm$temperature_flag[hi] <- '>= 45 C'
aggTherm$temperature_flag[low] <- '<= 10 C'

### Re-save FLAGGED data (both 15-minute means plus flags)
aggTherm$date <- NULL
saveRDS(aggTherm, "/home/sean/github/2020_greenhouse/first_summer_experiment/data/leaf_thermistor_data/leaf_thermistor_data_15min_agg_flagged.rds")



# ---- NOTE: Did not run this code, will aggregate data later on after combining ....


# # read back in
# # aggTherm <- readRDS("/home/sean/github/2020_greenhouse/first_summer_experiment/data/leaf_thermistor_data/leaf_thermistor_data_raw_compiled_long_flagged.rds")
# 
# # Aggregate by block/treatment means
# by15Flagged <- plyr::ddply(aggTherm, .variables = c('by15', 'block', 'thermistor', 'treatment',
#                                                      'plant_id', 'position', 'canopy_position',
#                                                      'flag','temperature_flag'), 
#                            function(x) {
#                              setNames(mean(x$mean_leaftemp_C, na.rm = T), 'mean_leaftemp_C')
#                            })
# 
# saveRDS(by15Flagged, "/home/sean/github/2020_greenhouse/first_summer_experiment/data/leaf_thermistor_data/leaf_thermistor_data_15min_agg_flagged.rds")
