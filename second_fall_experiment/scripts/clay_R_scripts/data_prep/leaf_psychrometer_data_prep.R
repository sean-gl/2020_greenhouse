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

# by15 (timestamp)
psych$by15 <- as.POSIXct(paste(psych$date, psych$time, sep = ' '), format = '%Y-%m-%d %H:%M:%S')

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


# TODO: PSYCHORMMETER 2 WAS SET 1 HOUR AHEAD OF STANDARD TIME, need to adjust. (See Sean's note
# on 11/4.)
psych$by15[psych$psychrometer_number==2] <- psych$by15[psych$psychrometer_number==2] - 3600

# ----- Save the cleaned file
saveRDS(psych, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/psychrometers/combined_psychrometer_data_raw.rds')

# read back in 
# psych <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/psychrometers/combined_psychrometer_data_raw.rds')

# ======> Now, let's try to find some useful data.

require(ggplot2)

# omit obviously bad values
ind <- which(psych$corrected_water_potential_MPa < -10); ind
psych <- psych[-ind, ]

# look at 1st experiment data
psych_exp1 <- subset(psych, date >= '2019-06-01' & date <= '2019-10-24' )
ggplot(psych_exp1, aes(x=by15, y=corrected_water_potential_MPa, color=psychrometer_number)) + 
  geom_point() 


# look at 2nd experiment data
psych_exp2 <- subset(psych, date >= '2019-09-09' & date <= '2019-12-12' )
ggplot(psych_exp2, aes(x=by15, y=corrected_water_potential_MPa, color=psychrometer_number)) + 
  geom_point() 

# Notes on psychrometer data for expiriment 2:
# 1. Sean was taking them between start of experiment and 11/20, then Clay took over on 11/21.
# 2. psychrometer "PSY-2" was not set to standard time (data 1 hour ahead.) 

# examine data before pressure bomb measurements
# psy-2 on W-11, psy-3 on D-7, psy-4 on M-10
x <- subset(psych_exp2, date <= '2019-11-03')
ggplot(x, aes(x=by15, y=corrected_water_potential_MPa, color=psychrometer_number)) + 
  geom_point() #+ ylim(c(-1, 0))
# verdict: These don't make sense, given treatment assignments...probably NOT useful.

#  examine ~ nov 4
x <- subset(psych_exp2, date <= '2019-11-11')
ggplot(x, aes(x=by15, y=corrected_water_potential_MPa, color=psychrometer_number)) + 
  geom_point() 
# verdict: seems NOT useful. Psychrometer #2 is on wet treatment plant, but much lower water potential. 

# examine  ~nov 19 week
x <- subset(psych_exp2, date >= '2019-11-18' & date <= '2019-11-20')
ggplot(x, aes(x=by15, y=corrected_water_potential_MPa, color=psychrometer_number)) + 
  geom_point() 
# verdict: Curve seems believable, but numbers seem too low for a wet treatment plant.
# Also, this was on plant D-2 (wet treatment) which wasn't on a scale. 
# MAYBE useful??
# ACtually, this was noted to have a broken thermistor on 11/20, so probably should NOT use.

# examine  ~nov 21 to 22
# After 11/21, the psychrometers were:"
# psy-1 on plant D-15, psy-2 on W-9 and psy-4 on M-10. 
# On 11/27 (with 3rd treatment start), psy-2 was moved to a virgin plant W-26.

# need to adjust readings to sensor calibration 2-pt check
# y=real; x=measured
y = c(-0.642, -4.64); x = c(-0.77, -6.025)
(m = (y[2]-y[1])/(x[2]-x[1]))
start = '2019-11-22'; end = '2019-11-25'
# note: this psychrometer on plant in D block from 11/21 to....end of experiment?
# on 11-22, moved it from midrib to lamina of same leaf.
d <- subset(psych, date >= start & date <= end & psychrometer_number == 1)
# d <- subset(psych_exp2, date == '2019-11-23')
d$corrected_water_potential_MPa <- m*d$corrected_water_potential_MPa - m*x[1] + y[1]
ggplot(d, aes(x=by15, y=corrected_water_potential_MPa, color=psychrometer_number)) + 
  geom_point() + geom_line() #+ ylim(c(-2, 0))


### ---------- Read in psi_leaf predictions ---------------
psileaf <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_psi_leaf/combined_data_predicted_psi_leaf.rds')

# subset to same block/period as psyrchometer
psi.D <- subset(psileaf, block=='D' & date >= start & date <= end)

# merge
d <- merge(psi.D[,c('by15','predicted_psi_leaf','mean_psi_MPa','line_PAR_west_umol_m2_s')], 
           d[d$psychrometer_number==1,], by='by15', all = T)

# plot predicted vs. LED
plot(corrected_water_potential_MPa ~ by15, data=d, type='b')
par(new = T)
plot(line_PAR_west_umol_m2_s ~ by15, data=d, type = 'b', col = 'blue',
     xaxt = "n", yaxt = "n", ylab = "", xlab = "")
axis(side = 4); mtext("PAR_west", side = 4, line = 0)

# plot predicted vs. psychormeter
plot(corrected_water_potential_MPa ~ by15, data=d, type='b', ylim = c(-6, 2))
lines(-predicted_psi_leaf ~ by15, data=d, type = 'b', col = 'red')

# look at actual pressure bomb data
x=subset(psileaf)

# examine  ~nov 24 to end of experiment
x <- subset(psych_exp2, date >= '2019-12-01' & date <='2019-12-02')
ggplot(x, aes(x=by15, y=corrected_water_potential_MPa, color=psychrometer_number)) + 
  geom_point() + geom_line() + ylim(c(-1, 0))
# verdict: looks like it's all crap; 