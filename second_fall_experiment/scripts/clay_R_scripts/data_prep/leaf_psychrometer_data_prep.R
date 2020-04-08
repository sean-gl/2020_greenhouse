# Clay Bliss
# Script to read, process, and examine leaf psychrometer data from Gleason's 2019 greenhouse experiment.
# Trying to find some useable data!

# -----------------------------------

rm(list = ls())


### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')
Sys.getenv('TZ') # make sure it got set


wd <- "/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/psychrometers/read_only/"
setwd(wd)

###
### ------Read and process data
###

### Experiment 2 data ----

# read in files
files <- dir(wd)
files <- files[grepl('.CSV', files)]
files

# TRY one file
# d = read.csv('PSY1I70E_no4.CSV', header = F, stringsAsFactors = F, skip = 15)
# d[1,]
# names(d) <- c('date','time','chamber_temp_C','dT_uV','wet_bulb_depression_uV',
#               'corrected_water_potential_MPa','intercept','slope','EDBO','correction_for_dT_MPa',
#               'correction_factor','internal_battery_voltage_V','internal_battery_temp_C','external_power_supply_present',
#               'external_power_supply_voltage_V','external_power_supply_current_mA','diagnostic_comment')
# d <- d[-1,] # replace 1st row as header
# 
# # remove mid-file header rows
# d <- d[d$date != 'Date', ]
# 
# # omit rows with no water potential reading
# ind <- is.na(d$corrected_water_potential_MPa) | d$corrected_water_potential_MPa == ''
# length(which(ind))
# d <- d[!ind, ]
# 
# # check for numeric columns and convert to numeric
# numcols <- apply(d, 2, function(x) !any(is.na(as.numeric(x)))); 
# names(numcols[numcols]) # these should be convertable to numeric
# names(numcols[!numcols]) # these are probably not
# for(col in names(numcols[numcols])) d[[col]] <- as.numeric(d[[col]])
# 
# summary(d)

# ---- Repeat above procedure on ALL files ---
datList <- lapply(files, function(x) {
  d <- read.csv(x, header = F, stringsAsFactors = F, skip = 15)
  # add psychrometer number by parsing from csv file names
  m <- regexpr('_no\\d', x)
  d$psychrometer_number <- as.numeric(substr(x, m+3, m+3))
  return(d)
})
psych <- do.call(rbind, datList)
table(psych$psychrometer_number, useNA = 'always') # check that all data have a psyrhometer no.
psych$psychrometer_number <- as.factor(psych$psychrometer_number)

names(psych) <- c('date','time','chamber_temp_C','dT_uV','wet_bulb_depression_uV',
              'corrected_water_potential_MPa','intercept','slope','EDBO','correction_for_dT_MPa',
              'correction_factor','internal_battery_voltage_V','internal_battery_temp_C','external_power_supply_present',
              'external_power_supply_voltage_V','external_power_supply_current_mA','diagnostic_comment','psychrometer_number')

# remove header rows
psych <- psych[psych$date != 'Date', ]

# omit rows with no water potential reading
ind <- is.na(psych$corrected_water_potential_MPa) | psych$corrected_water_potential_MPa == ''
psych <- psych[!ind, ]

# reset row names
rownames(psych) <- NULL

# check for numeric columns and convert to numeric
numcols <- apply(psych, 2, function(x) !any(is.na(as.numeric(x)))); 
(nc <- names(numcols[numcols])) # these should be convertable to numeric
(notnc <- names(numcols[!numcols]))# these are probably not

# hmm why are these not convertible to numeric?
x=as.numeric(psych$external_power_supply_voltage_V)
# View(psych[which(is.na(x)),]) # These say "USB"...change to NA
psych$external_power_supply_voltage_V[trimws(psych$external_power_supply_voltage_V) == 'USB'] <- NA
psych$external_power_supply_current_mA[trimws(psych$external_power_supply_current_mA) == 'USB'] <- NA
nc <- c(nc[nc != 'psychrometer_number'], 'external_power_supply_current_mA', 'external_power_supply_voltage_V')

# finally, convert columns to numeric!
for(col in nc) psych[[col]] <- as.numeric(psych[[col]])

# Examine the compiled dataset...looks good except dates/times and factor columns
summary(psych)


# --- Fix some more columns...

# date
psych$date <- as.Date(psych$date, format = '%d/%m/%Y')

# datetime
psych$datetime <- as.POSIXct(paste(psych$date, psych$time, sep = ' '), format = '%Y-%m-%d %H:%M:%S')

# drop "time" column, redundant
psych$time <- NULL

summary(psych)

# remove duplicate rows: There are many!
dups <- duplicated(psych) | duplicated(psych, fromLast = T)
length(which(dups))
psych <- psych[!dups,]

# convert column to factor
psych$external_power_supply_present <- as.factor(psych$external_power_supply_present)
table(psych$external_power_supply_present, useNA = 'always')

# ----- Save the cleaned file
saveRDS(psych, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/psychrometers/combined_psychrometer_data_raw.rds')



# ======> Now, let's try to find some useful data.

require(ggplot2)

# subset to dates on or after 1st pressure bomb measurements
psych_sub <- subset(psych, date >= '2019-11-04')
ggplot(psych_sub, aes(x=datetime, y=corrected_water_potential_MPa, color=psychrometer_number)) + 
  geom_point() 

# psych_sub <- subset(psych, corrected_water_potential_MPa < 0 & corrected_water_potential_MPa > -6)

# Notes on psychrometer data:
# 1. Sean was taking them between start of experiment and 11/20, then Clay took over on 11/21.
# 2. psychrometer "PSY-2" was not set to standard time (data 1 hour ahead.) 


#  examine ~ nov 4
x <- subset(psych_sub, date <= '2019-11-11')
ggplot(x, aes(x=datetime, y=corrected_water_potential_MPa, color=psychrometer_number)) + 
  geom_point() 
# verdict: seems NOT useful. Psychrometer #2 is on wet treatment plant, but much lower water potential. 

# examine  ~nov 19 week
x <- subset(psych_sub, date >= '2019-11-18' & date <= '2019-11-20')
ggplot(x, aes(x=datetime, y=corrected_water_potential_MPa, color=psychrometer_number)) + 
  geom_point() 
# verdict: Curve seems believable, but numbers seem too low for a wet treatment plant.
# Also, this was on plant D-2 (wet treatment) which wasn't on a scale. 
# MAYBE useful??

# examine  ~nov 21 to 22
x <- subset(psych_sub, date >= '2019-11-21' & date <= '2019-11-22')
ggplot(x, aes(x=datetime, y=corrected_water_potential_MPa, color=psychrometer_number)) + 
  geom_point() + geom_line() + ylim(c(-2, 0))

# examine  ~nov 23 to 24
x <- subset(psych_sub, date >= '2019-11-23' & date <= '2019-11-24')
ggplot(x, aes(x=datetime, y=corrected_water_potential_MPa, color=psychrometer_number)) + 
  geom_point() + geom_line() + ylim(c(-3, 0))
# might be something useful on 11/23 for psych # 1