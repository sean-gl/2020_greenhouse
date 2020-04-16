
### Script to add LED contribution to PAR greenhouse data 2019
### LEDs contributed ~ 225 umol/m2*s at canopy height (see 'LED_PAR.R' script), and here:
### /home/wmsru/github/2020_greenhouse/second_fall_experiment/data/LED_PAR.ods

### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')

require(lubridate)

# read line quantum PAR data
lq <- read.csv('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/line_PAR_sensors/line_PAR_15.csv')
lq$by15 <- as.POSIXct(lq$by15, tz = 'GMT')

# add date & minutes columns
lq$date <- date(lq$by15)
lq$minutes <- hour(lq$by15)*60 + minute(lq$by15)

# general index for times between 03:20 and 19:20 (when LEDs were on)
ind <- with(lq, (minutes >= 3*60 + 20) & (minutes <= 19*60 + 20)) # TRUE = LEDs on.

# special indices for times when LEDs manually turned off (see greehouse logbook)
# TRUE = LEDS ON
ind1 <- !(with(lq, date == '2019-12-05' & (minutes >= 10*60 + 40) & (minutes <= 17*60 + 30)))
ind2 <- !(with(lq, date == '2019-12-10' & (minutes >= 16*60 + 50)))
ind3 <- !(with(lq, date == '2019-12-11' & (minutes <= 7*60 + 15)))

# combine all indices 
ind <- ind & ind1 & ind2 & ind3

# add LED contribution to indexed timestamps
lq$line_PAR_east_umol_m2_s_plusLED[!ind] <- lq$line_PAR_east_umol_m2_s[!ind]
lq$line_PAR_east_umol_m2_s_plusLED[ind] <- lq$line_PAR_east_umol_m2_s[ind] + 225
lq$line_PAR_west_umol_m2_s_plusLED[!ind] <- lq$line_PAR_west_umol_m2_s[!ind]
lq$line_PAR_west_umol_m2_s_plusLED[ind] <- lq$line_PAR_west_umol_m2_s[ind] + 225

# plot lq before (black) and after (red) adding LED
x=subset(lq, date >= '2019-12-04' & date <= '2019-12-12')
plot(x$by15, x$line_PAR_east_umol_m2_s_plusLED, type = 'l', col='red'); lines(x$by15, x$line_PAR_east_umol_m2_s)




# ---- repeat above but with rh instead of lq....


# read in other par data
rh <- read.csv('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/RH_temp_PAR_logger_data/rh_15.csv')
rh$by15 <- as.POSIXct(rh$by15, tz='GMT')

# add date & minutes columns
rh$date <- date(rh$by15)
rh$minutes <- hour(rh$by15)*60 + minute(rh$by15)

# general index for times between 03:20 and 19:20 (when LEDs were on)
ind <- with(rh, (minutes >= 3*60 + 20) & (minutes <= 19*60 + 20)) # TRUE = LEDs on.

# special indices for times when LEDs manually turned off (see greehouse logbook)
# TRUE = LEDS ON
ind1 <- !(with(rh, date == '2019-12-05' & (minutes >= 10*60 + 40) & (minutes <= 17*60 + 30)))
ind2 <- !(with(rh, date == '2019-12-10' & (minutes >= 16*60 + 50)))
ind3 <- !(with(rh, date == '2019-12-11' & (minutes <= 7*60 + 15)))

# combine all indices 
ind <- ind & ind1 & ind2 & ind3

# add LED contribution to indexed timestamps
rh$par1_n_plusLED[!ind] <- rh$par1_n[!ind]
rh$par1_n_plusLED[ind] <- rh$par1_n[ind] + 225
rh$par2_s_plusLED[!ind] <- rh$par2_s[!ind]
rh$par2_s_plusLED[ind] <- rh$par2_s[ind] + 225

# plot rh before (black) and after (red) adding LED
x=subset(rh, date >= '2019-12-04' & date <= '2019-12-12')
plot(x$by15, x$par1_n, type = 'l', col='red'); lines(x$by15, x$par1_n)


# ---- Looks good, now replace the original data columns with added PAR
lq$line_PAR_east_umol_m2_s <- lq$line_PAR_east_umol_m2_s_plusLED
lq$line_PAR_east_umol_m2_s_plusLED <- NULL 
lq$line_PAR_west_umol_m2_s <- lq$line_PAR_west_umol_m2_s_plusLED
lq$line_PAR_west_umol_m2_s_plusLED <- NULL
rh$par1_n <- rh$par1_n_plusLED; rh$par1_n_plusLED <- NULL
rh$par2_s <- rh$par2_s_plusLED; rh$par2_s_plusLED <- NULL

# remove "X" column (not sure where that came from), and date/minutes columns
rh <- subset(rh, select = -c(date, minutes, X))
lq <- subset(lq, select = -c(date, minutes, X))

# save the updated data files
write.csv(lq, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/line_PAR_sensors/line_PAR_15.csv',
          row.names = F)
write.csv(rh, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/RH_temp_PAR_logger_data/rh_15.csv',
          row.names = F)


# ----- bonus code....not used, except to show that the LQ par data is better quality overall.

# --- examine the 'rh' PAR data...baseline (night) LED is < 0, let's shift it upward.
x=subset(rh, date >= '2019-12-04' & date <= '2019-12-12')
x=subset(rh, date <= '2019-11-15'); y=subset(lq, date <= '2019-11-15')
x=subset(rh, date >= '2019-11-15'); y=subset(lq, date >= '2019-11-15')

x=rh
plot(x$by15, x$par1_n, type = 'l', col='red'); abline(c(0,0))
lines(x$by15, x$par2_s, type = 'l', col='blue'); abline(c(0,0))

# compare to lq par
plot(x$by15, x$par1_n, type = 'l', col='red'); abline(c(0,0))
lines(y$by15, y$line_PAR_east_umol_m2_s, type = 'l', col='blue'); abline(c(0,0))
lines(y$by15, y$line_PAR_west_umol_m2_s, type = 'l', col='green'); abline(c(0,0))
