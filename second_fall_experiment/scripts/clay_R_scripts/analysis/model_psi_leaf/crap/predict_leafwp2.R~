### Model fitting, Greenhouse experiment 2019
### Goal: Use measured parameters to predict water potential, build a model to fill in 
### missing data (so we can treat leaf water potential as a continuous variable)

rm(list=ls())
require(ggplot2)
require(plyr)
require(lubridate)
require(readODS)
require(tidyr)
require(dplyr)

### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')
Sys.getenv('TZ') # make sure it got set

### Read in data

# 1. leaf temperature
lt <- readRDS('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/leaf_thermistor_data/leaf_thermistor_data_15min_agg_flagged.rds')

# remove position column, not useful
lt$position <- NULL
colnames(lt)[colnames(lt)=='canopy_position'] <- 'position'
# change position categoies to match wind data
lt$position[lt$position=='lower'] <- 'bottom'
lt$position[lt$position=='upper'] <- 'top'

# 2. PAR
lq <- read.csv('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/line_PAR_sensors/line_PAR_15.csv')
lq$by15 <- as.POSIXct(lq$by15, tz = 'GMT')

# 3. RH, air temp, soil temp
rh <- read.csv('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/RH_temp_PAR_logger_data/rh_15.csv')
rh$by15 <- as.POSIXct(rh$by15, tz='GMT')
# remove soil temp columsn, these are imported below
rh <- rh %>% select(-contains('soil_t'))
soil_temp <- read.csv('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/RH_temp_PAR_logger_data/soil_temp_15.csv')
soil_temp$by15 <- as.POSIXct(soil_temp$by15, tz='GMT')

# 4. Wind sensors
wind <- read.csv('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/wind_sensor_data/wind_15.csv')
wind$by15 <- as.POSIXct(wind$by15, tz='GMT')

# convert to long format
windWide <- tidyr::spread(wind, 'position', 'wind_speed_m_s')
head(windWide)
colnames(windWide) <- c('by15','treatment','windspeed_bottom','windspeed_middle','windspeed_top')

# 5. Pressure bomb data
pb <- read.csv('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/pressure_bomb/pressure_bomb_15.csv')
pb$by15 <- as.POSIXct(pb$by15, tz='GMT')
# omit bad observation & missing observation
pb <- pb[pb$data_ok=='yes' & !is.na(pb$psi_MPa), ]
# get means by day and treatment/block
pb$block <- toupper(substr(pb$plant_id,1,1))
pb$date <- lubridate::date(pb$by15)
pbMeans <- ddply(pb, .(by15, block, treatment), function(x) {
  setNames(mean(x$psi_MPa), 'mean_psi_MPa')
})



### Merge air temp data to lt to compare

# filter data by flag 
lt_filter <- subset(lt, flag <= 2 & temperature_flag == 'none')
nrow(lt_filter)/nrow(lt)

# Aggregate by block
lt_block <- ddply(lt_filter, .(by15, block, treatment, position), function(x){
  setNames(mean(x$mean_leaftemp_C, na.rm = T), 'mean_leaftemp_C')
})
lat <- merge(lt_block, rh)
             # rh[ , c('by15', 'bmp_box_temp','sht1_high_temp','sht2_low_temp','am2320_high_temp')])
lat$date <- lubridate::date(lat$by15)
# sub <- subset(lat, position == 'top' & date > '2019-12-09' & date < '2019-12-11')
# ggplot(sub, aes(x=by15)) +
#   geom_line(aes(y=mean_leaftemp_C, colour = 'leaf')) +
#   # geom_line(aes(y=bmp_box_temp, colour = 'air (box)')) +
#   geom_line(aes(y=sht1_high_temp, colour = 'air sht1 high')) +
#   geom_line(aes(y=sht2_low_temp, colour = 'air sht2low')) +
#   geom_line(aes(y=am2320_high_temp, colour = 'air am2320high')) +
#   facet_grid(~block)

# convert to wide
lat_wide <- tidyr::spread(lat, 'position', 'mean_leaftemp_C')
names(lat_wide)[names(lat_wide) %in% c('bottom','middle','top')] <- c('leaftemp_bottom','leaftemp_middle','leaftemp_top')

### Calculate "altd" or difference between air and leaf temperature
# lat_wide$mean_airtemp_high <- rowMeans(lat_wide[,c('sht1_high_temp','am2320_high_temp')])
# lat_wide$mean_airtemp_hilo <- rowMeans(lat_wide[,c('sht1_high_temp','am2320_high_temp','sht2_low_temp')])
lat_wide$altd_bottom <- lat_wide$sht2_low_temp - lat_wide$leaftemp_bottom
lat_wide$altd_middle <- lat_wide$sht2_low_temp - lat_wide$leaftemp_middle
lat_wide$altd_top <- lat_wide$sht2_low_temp - lat_wide$leaftemp_top
lat_wide$temp_gradient <- lat_wide$sht1_high_temp - lat_wide$sht2_low_temp
# Calculate CUMMULATIVE daily differences in leaf/air temperature 

# b='D'
# x = lat[lat$date=='2019-12-10' & lat$block==b, ]
# z <- ddply(lat, .(date, block, treatment), function(x){
#   cumsum(x$tempdiff_airleaf) 
#   # plot(1:length(y),y,main=b)
#   # max(y)
#   # plot(1:(length(y)-1), diff(y))
# })

x <- split(lat_wide, lat_wide[,c('date','block')])

# Add cummulative sums of air to leaf temp diffs
y <- sapply(names(x), function(nm) {
  # cumsums that are zero until 5 am
  d <- x[[nm]]
  if(nrow(d) != 96) {
    cs_bottom <- cs_middle <- cs_top <- rep(NA, nrow(d))
  } else {
    dsub <- subset(d, hour(by15) >= 5)
    NA_fill <- rep(NA, nrow(d) - nrow(dsub))
    # NOTE: cumsum DOES NOT HANLDE NA
    cs_bottom <- c(NA_fill, cumsum(dsub$altd_bottom))
    cs_middle <- c(NA_fill, cumsum(dsub$altd_middle))
    cs_top <- c(NA_fill, cumsum(dsub$altd_top))
  }
  x[[nm]][,'cumsum_altd_bottom'] <<- cs_bottom
  x[[nm]][,'cumsum_altd_middle'] <<- cs_middle
  x[[nm]][,'cumsum_altd_top'] <<- cs_top
})


# Add cummulative sums for PAR
lq$date <- date(lq$by15)
x <- split(lq, lq$date)
y <- sapply(names(x), function(nm) {
  d <- x[[nm]]
  if(nrow(d) != 96) {
    cs_west <- cs_east <- rep(NA, nrow(d))
  } else {
    cs_west <- cumsum(d$line_PAR_west_umol_m2_s)
    cs_east <- cumsum(d$line_PAR_east_umol_m2_s)
  }
  x[[nm]][,'cumsum_PAR_west'] <<- cs_west 
  x[[nm]][,'cumsum_PAR_east'] <<- cs_east
})
lq2 <- do.call(rbind, x)


# combine by 15-min date and time
comb <- merge(pbMeans, lq2, by='by15')
comb <- merge(comb, soil_temp, by=c('by15','treatment'), all.x = T)
## NOTE: Lose some rows here, are there missing wind data? 
comb <- merge(comb, windWide, by=c('by15', 'treatment'), all.x = T)
comb <- merge(comb, lat_wide, by=c('by15', 'block','treatment','date'), all.x = T)

# check for any duplicated columsn in merges above
which(grepl('\\.x', names(comb)) | grepl('\\.y', names(comb)))

# add "time of day" column
comb$minutes <- 60*hour(comb$by15) + minute(comb$by15)


comb %>% group_by(treatment) %>% filter(!is.na(cumsum_altd_top)) %>% summarise(mean(cumsum_altd_top), n())

# add irrigation amount (ml)
comb$date <- date(comb$by15)
comb$irrig <- NA
comb$irrig[comb$date < "2019-11-05" & comb$treatment == 'well_watered'] <- 750
comb$irrig[comb$date >= "2019-11-05" & comb$treatment == 'well_watered'] <- 1000
comb$irrig[comb$treatment == 'moderate_drought'] <- 375
comb$irrig[comb$treatment %in% c('full_drought','virgin_drought')] <- 150
table(comb$irrig)
  
# calculate VPD_leaf based on leaf temperature
cor(comb$sht1_high_rh, comb$am2320_high_rh)
cor(comb$sht2_low_rh, comb$sht1_high_rh)
comb$rh_high_mean <- rowMeans(comb[ , c('sht1_high_rh','am2320_high_rh')])
comb$VPD_leaf <- (1 - (comb$rh_high_mean / 100)) * 0.61121 * exp((17.502 * comb$leaftemp_top) / (240.97 + comb$leaftemp_top)) 
summary(comb$VPD_leaf)


### Add days since treatment started
summary(comb$date)
comb$daysPostTrt <- NA
ind <- comb$date < '2019-11-05'
comb$daysPostTrt[ind] <- comb$date[ind] - as.Date('2019-10-25')
ind <- comb$date > '2019-11-04' & comb$date < '2019-11-28'
comb$daysPostTrt[ind] <- comb$date[ind] - as.Date('2019-11-05')
ind <- comb$date > '2019-11-27' 
comb$daysPostTrt[ind] <- comb$date[ind] - as.Date('2019-11-28')
summary(comb$daysPostTrt)

### ---- MODEL FITTING --------------

names(comb)
dat <- comb[, !names(comb) %in% c('date','by15','irrig','leaftemp_bottom',
                                  'leaftemp_middle','altd_bottom','altd_middle',
                                  'cumsum_altd_bottom','cumsum_altd_middle',
                                  'sht1_high_temp','sht2_low_temp')]
m1 <- lm(mean_psi_MPa ~ ., data=dat)
summary(m1)

names(comb)
m <- lm(mean_psi_MPa ~ minutes + block + treatment + leaftemp_top, data = comb)
m <- lm(mean_psi_MPa ~ minutes + block + treatment + leaftemp_top +
          altd_top, data = comb)
m <- lm(mean_psi_MPa ~ minutes + block + treatment + leaftemp_top +
          sht1_high_temp, data = comb)
summary(m)
m <- lm(mean_psi_MPa ~ block + treatment + leaftemp_top
          +cumsum_PAR_west, data = comb)
summary(m)

### Currently best linear model
m <- lm(mean_psi_MPa ~ minutes + treatment + block +
          leaftemp_top + windspeed_middle + bmp_box_temp +
          daysPostTrt, data = dat)
m <- lm(mean_psi_MPa ~ minutes + treatment + block +
          leaftemp_top + windspeed_middle + am2320_high_temp +
          daysPostTrt, data = dat)
summary(m)

plot(comb$mean_psi_MPa, comb$leaftemp_top)
plot(comb$mean_psi_MPa, comb$leaftemp_bottom)
yhat <- predict(m)
y <- comb$mean_psi_MPa[!is.na(comb$leaftemp_top)]
diffs <- y - yhat
summary(diffs)
mean(diffs^2) # test MSE
mean(abs(diffs)) # test MAD
plot(y, yhat)
abline(0,1,col='red')



###___________ lasso regression ______________####
###___________ lasso regression ______________####

# first subset to all possible independent variables and dependent variable
df2 <- subset(df, select=c("by15", "treatment", "position", "psi_MPa", 
                           "wind_speed_m_s", "soil_temp_C", "psi_MPa", 
                           "line_PAR_east_umol_m2_s", "line_PAR_west_umol_m2_s",
                           "par1_n", "par2_s", "pyr1_n", "pyr2_s", "am2320_high_temp", 
                           "am2320_high_rh", "sht1_high_temp", "sht1_high_rh", 
                           "sht2_low_temp", "sht2_low_rh", "bmp_box_temp", "bmp_box_atm_p")); nrow(df2)

# subset to... whatever...
df2 <- subset(df2, df$treatment == "well_watered")  ## CHANGE TREATMENT HERE...   

# there can't be any missing values
df2 <- subset(df2, complete.cases(df2)); nrow(df2)

# matrix "testvar" should include all possible predictor variables
testvar <- subset(df2, select=c("wind_speed_m_s", "soil_temp_C",  
                                "line_PAR_east_umol_m2_s", "line_PAR_west_umol_m2_s",
                                "par1_n", "par2_s", "pyr1_n", "pyr2_s", "am2320_high_temp", 
                                "am2320_high_rh", "sht1_high_temp", "sht1_high_rh", 
                                "sht2_low_temp", "sht2_low_rh", "bmp_box_temp", "bmp_box_atm_p")); nrow(testvar)
testvar <- as.matrix(scale(testvar)); nrow(testvar) # standardize matrix "testvar"

# give a dependent variable as a matrix
dep <- as.matrix(scale(df2$psi_MPa)); length(dep)
is.matrix(testvar); is.matrix(dep)


  ### RANDOM FOREST BOOSTING

# there can't be any missing values
# df <- comb[!is.na(comb$leaftemp_top), ]
df2 <- comb[, !names(comb) %in% c('by15','date',
                                  'cumsum_altd_bottom','cumsum_altd_middle','cumsum_altd_top')]
df2$block <- as.factor(df2$block)
# omit non-predictor vars
names(df2)
# replace NaN with NA (required for boosting)
for(i in 1:ncol(df2)) {
  ind <- is.nan(df2[,i])
  df2[ind, i] <- NA
}

# df[,c(grep('wind', names(df), value = T), 'leaftemp_bottom','leaftemp_middle','leaftemp_top')] <- NULL
# df2 <- df[complete.cases(df),]
  
  # subset into train and test dfs
  train_n <- round(0.7 * nrow(df2))
  test_n <- nrow(df2) - train_n
  train_ind <- sample(1:nrow(df2), train_n, replace = F)
  train_data <- df2[train_ind,]
  test_data <- df2[-train_ind,]
  
  ### Boosting
  require(gbm)
  
  m.boost <- gbm(mean_psi_MPa ~ .,
                 data = train_data,
                 distribution = 'gaussian',
                 n.trees = 5000,
                 interaction.depth = 1,
                 shrinkage = 0.01)
  # m.boost
  # summary(m.boost)
  yhat <- predict(m.boost, newdata = test_data, n.trees = 5000)
  y <- test_data$mean_psi_MPa
  diffs <- y-yhat
  mean(diffs^2) # test MSE
  mean(abs(diffs)) # test MAD
  summary(diffs)
preds <- data.frame(predicted_psi = yhat, actual_psi=test_data$mean_psi_MPa)
plot(predicted_psi ~ actual_psi, data=preds)
abline(0,1, col='red')
