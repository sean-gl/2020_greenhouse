
rm(list = ls())

### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')
Sys.getenv('TZ') # make sure it got set

require(dplyr); require(tidyr); require(ggplot2)

wd <- "/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/leaf_thermistor_data/read_only/"
setwd(wd)

# read in files
files <- dir(wd)
files <- files[grepl('.TXT', files)]
files

# read in all csv files into a list of df
datList <- lapply(files, function(x) {
  d <- read.csv(x, header = F, stringsAsFactors = F)
  # omit rows without data (header rows can occur mid-file)
  noData <- grepl('timestamp', d[,1]) | grepl('treatment', d[,1])
  d <- d[!noData,]
  d$file <- x
  return(d)
})

# combine list of df into 1 df
thermdat <- do.call(rbind, datList)



### ----- LIST OF DATA QUALITY CHECKS ------

# 1. Check that block (W, M, D) in column headers (plant ids) matches file prefix, otherwise there is an issue.

headerList <- lapply(files, function(x) {
  d <- read.csv(x, header = F, stringsAsFactors = F)
  # omit rows without data (header rows can occur mid-file)
  noData <- grepl('timestamp', d[,1])
  d <- d[noData,]
  d$file <- x
  return(d)
})

# combine list of df into 1 df
headerdat <- do.call(rbind, headerList)
unique(headerdat$file)
headerdat$file_block <- NA
w_ind <- grepl('_W_', headerdat$file) | grepl('wet', headerdat$file)
m_ind <- grepl('_M_', headerdat$file) | grepl('moderate', headerdat$file)
d_ind <- grepl('_D_', headerdat$file) | grepl('dry', headerdat$file)
headerdat$file_block[w_ind] <- 'W'
headerdat$file_block[m_ind] <- 'M'
headerdat$file_block[d_ind] <- 'D'
head(headerdat)
# x = headerdat[1,3:10]

headerdat$plant_block <- as.character(apply(headerdat[,3:10], 1, function(x) {
  letter_vec <- sapply(x, function(y) {
    y <- gsub(' ', '', x)
    substr(y, 1, 1)
  })
  toupper(unique(as.character(letter_vec)))
}))

nomatch <- which(headerdat$plant_block != headerdat$file_block)
# NOTE: These are noted in spreadsheet, the column headers ("plant_block")
# are incorrect; these are from plants in block D. 
# Since the file names are correct we can just use those to assign blocks below.
headerdat[nomatch,]
unique(headerdat[nomatch, 'file'])

### Back to the data...

# Assign block based on file name
thermdat$block <- NA
thermdat$block[grepl('wet', thermdat$file) | grepl('_W_', thermdat$file)] <- 'W'
thermdat$block[grepl('moderate', thermdat$file) | grepl('_M_', thermdat$file)] <- 'M'
thermdat$block[grepl('dry', thermdat$file) | grepl('_D_', thermdat$file)] <- 'D'
table(thermdat$block)

head(thermdat)

### Convert to long format after some processing...
# omit 1st column (this is the reference voltage which we don't need)
thermdat[,2] <- NULL
colnames(thermdat)[1:9] <- c('timestamp', 1:8)
thermdat$timestamp <- as.POSIXct(thermdat$timestamp, format="%Y/%m/%d %H:%M:%S", tz='GMT')
head(thermdat$timestamp)

thermLong <- tidyr::gather(thermdat, key = 'thermistor', value = 'temp_C', -c(timestamp, file, block))
table(thermLong$thermistor)

# add plant ID
thermLong$plant_id <- NA

thermLong$plant_id[thermLong$block == 'D' & thermLong$thermistor %in% 1:2] <- 'D-6'
thermLong$plant_id[thermLong$block == 'D' & thermLong$thermistor %in% 3:4] <- 'D-7'
thermLong$plant_id[thermLong$block == 'D' & thermLong$thermistor %in% 5:6] <- 'D-10'
thermLong$plant_id[thermLong$block == 'D' & thermLong$thermistor %in% 7:8] <- 'D-11'

thermLong$plant_id[thermLong$block == 'M' & thermLong$thermistor %in% 1:2] <- 'M-6'
thermLong$plant_id[thermLong$block == 'M' & thermLong$thermistor %in% 3:4] <- 'M-7'
thermLong$plant_id[thermLong$block == 'M' & thermLong$thermistor %in% 5:6] <- 'M-10'
thermLong$plant_id[thermLong$block == 'M' & thermLong$thermistor %in% 7:8] <- 'M-11'

# Plants in this block were changed to virgins on 11/27
thermLong$plant_id[thermLong$block == 'W' & thermdat$timestamp < '2019-11-27 16:24' &
                     thermLong$thermistor %in% 1:2] <- 'W-6'
thermLong$plant_id[thermLong$block == 'W' & thermdat$timestamp < '2019-11-27 16:24'
                   & thermLong$thermistor %in% 3:4] <- 'W-7'
thermLong$plant_id[thermLong$block == 'W' & thermdat$timestamp < '2019-11-27 16:24'
                   & thermLong$thermistor %in% 5:6] <- 'W-10'
thermLong$plant_id[thermLong$block == 'W' & thermdat$timestamp < '2019-11-27 16:24'
                   & thermLong$thermistor %in% 7:8] <- 'W-11'

thermLong$plant_id[thermLong$block == 'W' & thermdat$timestamp >= '2019-11-27 16:24' &
                     thermLong$thermistor %in% 1:2] <- 'W-26'
thermLong$plant_id[thermLong$block == 'W' & thermdat$timestamp >= '2019-11-27 16:24'
                   & thermLong$thermistor %in% 3:4] <- 'W-28'
thermLong$plant_id[thermLong$block == 'W' & thermdat$timestamp >= '2019-11-27 16:24'
                   & thermLong$thermistor %in% 5:6] <- 'W-25'
thermLong$plant_id[thermLong$block == 'W' & thermdat$timestamp >= '2019-11-27 16:24'
                   & thermLong$thermistor %in% 7:8] <- 'W-27'

# check work
table(thermLong$plant_id)
table(thermLong$thermistor, thermLong$plant_id)

# add thermistor position column
thermLong$position <- NA
thermLong$position[thermLong$thermistor %in% c(1,3,5,7)] <- 'bottom'
thermLong$position[thermLong$thermistor %in% c(2,4,6,8)] <- 'top'
table(thermLong$thermistor, thermLong$position)
table(thermLong$plant_id, thermLong$position)

# add thermistor "canopy_position" column 
# this is more precise and supercedes the "position" column
# it takes into account when thermistors were moved as canopy grew and senesced.
# TODO: Update these with heights

thermLong$canopy_position <- NA

### First do "bottom" position thermistors...

# From start until 11/18, in lower canopy
thermLong$canopy_position[thermLong$timestamp <= '2019-11-18 10:02' &
                            thermLong$block == 'W' &
                            thermLong$position == 'bottom'] <- 'lower'
thermLong$canopy_position[thermLong$timestamp <= '2019-11-18 11:08' &
                            thermLong$block == 'M' &
                            thermLong$position == 'bottom'] <- 'lower'
thermLong$canopy_position[thermLong$timestamp <= '2019-11-18 12:09' &
                            thermLong$block == 'D' &
                            thermLong$position == 'bottom'] <- 'lower'

# From 11/18 to 11/26, in middle canopy
thermLong$canopy_position[thermLong$timestamp >= '2019-11-18 10:59' &
                            thermLong$timestamp <= '2019-11-26 11:00' &
                            thermLong$block == 'W' &
                            thermLong$position == 'bottom'] <- 'middle'
thermLong$canopy_position[thermLong$timestamp >= '2019-11-18 11:51' &
                            thermLong$timestamp <= '2019-11-26 12:14' &
                            thermLong$block == 'M' &
                            thermLong$position == 'bottom'] <- 'middle'
thermLong$canopy_position[thermLong$timestamp >= '2019-11-18 13:35' &
                            thermLong$timestamp <= '2019-11-26 12:59' &
                            thermLong$block == 'D' &
                            thermLong$position == 'bottom'] <- 'middle'

# After 11/26, in upper canopy
thermLong$canopy_position[thermLong$timestamp >= '2019-11-26 11:56' &
                            thermLong$block == 'W' &
                            thermLong$position == 'bottom'] <- 'upper'
thermLong$canopy_position[thermLong$timestamp >= '2019-11-26 12:52' &
                            thermLong$block == 'M' &
                            thermLong$position == 'bottom'] <- 'upper'
thermLong$canopy_position[thermLong$timestamp >= '2019-11-26 14:09' &
                            thermLong$block == 'D' &
                            thermLong$position == 'bottom'] <- 'upper'

### Next do "top" position thermistors...

# W block: Middle canopy from experiment start until 11/6 
thermLong$canopy_position[thermLong$timestamp <= '2019-11-06 12:01' &
                            thermLong$block == 'W' &
                            thermLong$position == 'top'] <- 'middle'
# After 11/6, W block moved to upper canopy
thermLong$canopy_position[thermLong$timestamp >= '2019-11-06 12:41' &
                            thermLong$block == 'W' &
                            thermLong$position == 'top'] <- 'upper'

# M, D blocks: Middle canopy from experiment start until 11/12
thermLong$canopy_position[thermLong$timestamp <= '2019-11-12 11:16' &
                            thermLong$block == 'M' &
                            thermLong$position == 'top'] <- 'middle'
thermLong$canopy_position[thermLong$timestamp <= '2019-11-12 12:14' &
                            thermLong$block == 'D' &
                            thermLong$position == 'top'] <- 'middle'
# After 11/12, M,D blocks moved to upper canopy
thermLong$canopy_position[thermLong$timestamp >= '2019-11-12 12:01' &
                            thermLong$block == 'M' &
                            thermLong$position == 'top'] <- 'upper'
thermLong$canopy_position[thermLong$timestamp >= '2019-11-12 12:45' &
                            thermLong$block == 'D' &
                            thermLong$position == 'top'] <- 'upper'

# ----- CHECK THE ABOVE WORK...
table(thermLong$canopy_position, useNA = 'always')
table(thermLong$canopy_position, thermLong$position, useNA = 'always')

# which are missing canopy_position?
ind <- which(is.na(thermLong$canopy_position))
length(ind)
# missing <- thermLong[ind, ]
# summary(missing$timestamp)
# table(missing$position)
# table(missing$block)

# all lower, middle canopy
x <- thermLong[thermLong$timestamp < '2019-11-06 12:00', ]
table(x$canopy_position, x$position) 

# all middle, upper canopy
x <- thermLong[thermLong$timestamp >= '2019-11-18 12:09' &
                 thermLong$timestamp < '2019-11-26 12:58', ]
table(x$canopy_position, x$position) 


# all upper canopy
table(thermLong$canopy_position)
x <- thermLong[thermLong$timestamp >= '2019-11-26 14:10', ]
table(x$canopy_position, x$position) 

# Check combinations
min(thermLong$timestamp); max(thermLong$timestamp)

# lower canopy 
x <- subset(thermLong, canopy_position == 'lower')
min(x$timestamp); max(x$timestamp)

x <- subset(thermLong, position == 'bottom' & canopy_position == 'lower')
min(x$timestamp); max(x$timestamp)

x <- subset(thermLong, position == 'bottom' & canopy_position == 'middle')
min(x$timestamp); max(x$timestamp)

x <- subset(thermLong, position == 'bottom' & canopy_position == 'upper')
min(x$timestamp); max(x$timestamp)

x <- subset(thermLong, position == 'top' & canopy_position == 'lower')
min(x$timestamp); max(x$timestamp)

x <- subset(thermLong, position == 'top' & canopy_position == 'middle')
min(x$timestamp); max(x$timestamp)

x <- subset(thermLong, position == 'top' & canopy_position == 'upper')
min(x$timestamp); max(x$timestamp)


# convert temperature to numeric
thermLong$temp_C <- as.numeric(thermLong$temp_C)

### Add Current Treatment Column (based on date only, not time)
thermLong$date <- lubridate::date(thermLong$timestamp)
thermLong$treatment <- NA
# baseline (pre-treatments)
thermLong$treatment[thermLong$date <= '2019-10-24'] <- 'well_watered'
# treatment 1
thermLong$treatment[thermLong$date > '2019-10-24' & thermLong$date <= '2019-11-04' & thermLong$block == 'W'] <- 'well_watered'
thermLong$treatment[thermLong$date > '2019-10-24' & thermLong$date <= '2019-11-04' & thermLong$block == 'M'] <- 'moderate_drought'
thermLong$treatment[thermLong$date > '2019-10-24' & thermLong$date <= '2019-11-04' & thermLong$block == 'D'] <- 'full_drought'
# treatment 2
thermLong$treatment[thermLong$date > '2019-11-04' & thermLong$date <= '2019-11-27' & thermLong$block == 'W'] <- 'full_drought'
thermLong$treatment[thermLong$date > '2019-11-04' & thermLong$date <= '2019-11-27' & thermLong$block == 'M'] <- 'moderate_drought'
thermLong$treatment[thermLong$date > '2019-11-04' & thermLong$date <= '2019-11-27' & thermLong$block == 'D'] <- 'well_watered'
# treatment 3 (final)
thermLong$treatment[thermLong$date > '2019-11-27' & thermLong$block == 'W'] <- 'virgin_drought'
thermLong$treatment[thermLong$date > '2019-11-27' & thermLong$block == 'M'] <- 'well_watered'
thermLong$treatment[thermLong$date > '2019-11-27' & thermLong$block == 'D'] <- 'full_drought'
# convert to factor
thermLong$treatment <- as.factor(thermLong$treatment)
table(thermLong$treatment, useNA = 'always')

# remove date column before saving
thermLong$date <- NULL

### Save raw data in LONG format
saveRDS(thermLong, "/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/leaf_thermistor_data/leaf_thermistor_data_raw_compiled_long.rds")

# thermLong <- readRDS("/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/leaf_thermistor_data/leaf_thermistor_data_raw_compiled_long.rds")

### Do 15-minute aggregation and re-save
thermLong$by15 <- lubridate::ceiling_date(thermLong$timestamp, unit = '15 minutes')
aggTherm <- plyr::ddply(thermLong, .variables = c('by15', 'block', 'treatment', 'thermistor',
                                                  'plant_id', 'position', 'canopy_position'), 
                        function(x) {
                          setNames(mean(x$temp_C, na.rm = T), 'mean_leaftemp_C')
                        })
### Save aggregated data
saveRDS(aggTherm, "/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/leaf_thermistor_data/leaf_thermistor_data_15min_agg.rds")

# aggTherm <- readRDS("/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/leaf_thermistor_data/leaf_thermistor_data_15min_agg.rds")

### ----- Read in Data Quality Spreadsheet to Use for Flagging ------
require(readODS)
meta <- read.ods("/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/metadata_thermistors_scales_greenhouse2019.ods",
                 sheet = 'thermistor_notes')
colnames(meta) <- meta[1,]
meta <- meta[-1,]
rownames(meta) <- NULL
colnames(meta) <- c('date','time_checked','plant_id','position','condition','action_taken',
                    'affected_data','downloaded_after','notes','time_checked_notdownloaded')

meta$affected_data[meta$affected_data == ''] <- 'no'
table(meta$affected_data)

# convert to a flag
meta$flag <- NA
meta$flag[meta$affected_data == 'no'] <- 0
meta$flag[meta$affected_data == 'only during sun -see notes'] <- 1
meta$flag[meta$affected_data == 'maybe (probably ok)'] <- 1
meta$flag[meta$affected_data == 'maybe'] <- 2
meta$flag[meta$affected_data %in% c('probably', 'probably (in sun)')] <- 3
meta$flag[meta$affected_data == 'yes'] <- 4

table(meta$flag)

# do stuff for merge
meta$plant_id <- toupper(meta$plant_id)
meta$position[meta$position == 'upper'] <- 'top'
meta$position[meta$position == 'lower'] <- 'bottom'
table(meta$position)

# add timestamp column
# x = paste0(meta$date[1], ' ', meta$time_checked[1])
# x
# as.POSIXct(x, format = "%m/%d/%Y %H:%M", tz = "GMT")
meta$timestamp <- as.POSIXct(paste0(meta$date, ' ', meta$time_checked), 
                             format = "%m/%d/%Y %H:%M", tz = "GMT")
head(meta$timestamp)
meta$date <- lubridate::date(meta$timestamp)
meta$plant_id <- as.factor(meta$plant_id)
meta$position <- as.factor(meta$position)

# sort by date then plant then posistion
meta <- meta[order(meta$date, meta$plant_id, meta$position), ]


# W-block plants were changed to virgins on 11/27 but were not checked that day, since 
# they were already checked on 11/26. So these timestamps need to be updated (moved 1 day ahead)
# in order to flagging to work

ind <- meta$date == '2019-11-26' & meta$plant_id %in% c('W-6','W-7','W-10','W-11')
meta[ind, 'timestamp'] <- '2019-11-27 14:30'
# ind <- thermLong$timestamp <= '2019-11-27 14:30' &
#   thermLong$timestamp >= '2019-11-26 10:59' & 
#   thermLong$plant_id %in% c('W-6','W-7','W-10','W-11')
# x=thermLong[ind,]

### Finally, add flags to "thermLong" dataframe...
thermLong$flag <- NA
# i=1
for(i in 1:nrow(meta)) {
  
  flag <- meta[i, 'flag']
  print(paste0("i=", i))
  # print(paste0("flag=", flag))
  plant <- meta[i, 'plant_id']
  pos <- meta[i, 'position']
  current <- meta[i, 'timestamp'] # current time thermistor checked
  sort_times <- sort(meta[meta$plant == plant & meta$position == pos, 'timestamp'])
  ind <- sort_times < current
  last <- sort_times[length(ind[ind])] #last time thermistor was checked
  
  # if 'last' is length zero this means this is the first time thermistor was checked.
  if(length(last) == 0) {
    
    d <- thermLong[thermLong$plant_id == plant &
                thermLong$position == pos &
                thermLong$timestamp <= current,]
    
    thermLong[thermLong$plant_id == plant &
                thermLong$position == pos &
                thermLong$timestamp <= current, 'flag'] <- flag
    
  } else if(length(last) == 1) {
    
    d <- thermLong[thermLong$plant_id == plant &
                     thermLong$position == pos &
                     thermLong$timestamp <= current &
                     thermLong$timestamp >= last,]
    
    thermLong[thermLong$plant_id == plant &
                thermLong$position == pos &
                thermLong$timestamp <= current &
                thermLong$timestamp >= last, 'flag'] <- flag
    
  } else { 
    print(paste0("i=", i, ". Problem: 'last' variable is of wrong length"))}
  
  if(nrow(d)==0) print("zero rows matching")
  
}

table(thermLong$flag, useNA = 'always') / nrow(thermLong)

# some NA flags, why?
NAflags <- thermLong[is.na(thermLong$flag),]
table(NAflags$plant_id)
table(lubridate::date(NAflags$timestamp), NAflags$plant_id)
table(lubridate::date(NAflags$timestamp), NAflags$plant_id, NAflags$position)


# M-10 thermistors statuses were not recorded on last day but I will give it a "1" flag 
# since other thermistors in this block had this status generally.
ind <- is.na(thermLong$flag) & 
  lubridate::date(thermLong$timestamp) >= '2019-12-06' &
  thermLong$plant_id == 'M-10'
thermLong[ind, 'flag'] <- 1


## Examine distribution of temperature data and create flags for obviously high/low temperatures
plot(density(thermLong$temp_C, na.rm = T), xlim=c(-10, 50))
hi <- thermLong$temp_C >= 40 & !is.na(thermLong$temp_C)
low <- thermLong$temp_C <= 10 & !is.na(thermLong$temp_C)

length(which(hi)) /nrow(thermLong) * 100 # 0.3% of observations > 40 deg C
length(which(low)) /nrow(thermLong) * 100 # 0.02 % of observations < 10 deg C

thermLong$temperature_flag <- 'none'
thermLong$temperature_flag[hi] <- '>= 40 C'
thermLong$temperature_flag[low] <- '<= 10 C'

### Re-save FLAGGED data (both raw and 15-minute re-aggregated)
thermLong$date <- NULL
saveRDS(thermLong, "/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/leaf_thermistor_data/leaf_thermistor_data_raw_compiled_long_flagged.rds")

# read back in
# thermLong <- readRDS("/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/leaf_thermistor_data/leaf_thermistor_data_raw_compiled_long_flagged.rds")

# Aggregate by block/treatment means
by15Flagged <- plyr::ddply(thermLong, .variables = c('by15', 'block', 'thermistor', 'treatment',
                                                     'plant_id', 'position', 'canopy_position',
                                                     'flag','temperature_flag'), 
                           function(x) {
                             setNames(mean(x$temp_C, na.rm = T), 'mean_leaftemp_C')
                           })

saveRDS(by15Flagged, "/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/leaf_thermistor_data/leaf_thermistor_data_15min_agg_flagged.rds")
