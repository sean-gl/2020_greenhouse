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



### SECTION 1: Read data sets and do some processing  -----------------


# 1. leaf temperature
lt <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/data/leaf_thermistor_data/leaf_thermistor_data_15min_agg_flagged.rds')

# remove position column, not useful
lt$position <- NULL
colnames(lt)[colnames(lt)=='canopy_position'] <- 'position'
# change position categoies to match wind data
lt$position[lt$position=='lower'] <- 'bottom'
lt$position[lt$position=='upper'] <- 'top'

# filter data by flag 
lt_filter <- subset(lt, flag <= 2 & temperature_flag == 'none')
nrow(lt_filter)/nrow(lt)

# Aggregate by block
lt_block <- ddply(lt_filter, .(by15, block, treatment, position), function(x){
  setNames(mean(x$mean_leaftemp_C, na.rm = T), 'mean_leaftemp_C')
})


# 2. PAR
lq <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/line_PAR_sensors/line_PAR_15.csv')
lq$by15 <- as.POSIXct(lq$by15, tz = 'GMT')


# 3. RH, air temp, soil temp
rh <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/RH_temp_PAR_logger_data/rh_15.csv')
rh$by15 <- as.POSIXct(rh$by15, tz='GMT')

rh$par2_s <- NULL # REMOVE THIS VARIABLE, DATA ARE BAD

# remove soil temp columsn, these are imported below
rh <- rh %>% select(-contains('soil_t'))
soil_temp <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/RH_temp_PAR_logger_data/soil_temp_15.csv')
soil_temp$by15 <- as.POSIXct(soil_temp$by15, tz='GMT')

# 4. Wind sensors
wind <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/wind_sensor_data/wind_15.csv')
wind$by15 <- as.POSIXct(wind$by15, tz='GMT')
# convert to long format
windWide <- tidyr::spread(wind, 'position', 'wind_speed_m_s')
head(windWide)
colnames(windWide) <- c('by15','treatment','windspeed_bottom','windspeed_middle','windspeed_top')


# 5. Pressure bomb data
pb <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/pressure_bomb/pressure_bomb_15.csv')
pb$by15 <- as.POSIXct(pb$by15, tz='GMT')
# omit bad observation & missing observation
pb <- pb[pb$data_ok=='yes' & !is.na(pb$psi_MPa), ]
# get means by day and treatment/block
pb$block <- toupper(substr(pb$plant_id,1,1))
pb$date <- lubridate::date(pb$by15)
pbMeans <- ddply(pb, .(by15, block, treatment), function(x) {
  setNames(mean(x$psi_MPa), 'mean_psi_MPa')
})



### SECTION 2. Add more "derived" variables -----------------

# 1. Leaf-air temperature differences.
lat <- merge(lt_block, rh)
lat$date <- lubridate::date(lat$by15)
# convert to wide
lat_wide <- tidyr::spread(lat, 'position', 'mean_leaftemp_C')
names(lat_wide)[names(lat_wide) %in% c('bottom','middle','top')] <- c('leaftemp_bottom','leaftemp_middle','leaftemp_top')

# use lower (colder) sensor
lat_wide$altd_bottom <- lat_wide$sht2_low_temp - lat_wide$leaftemp_bottom
lat_wide$altd_middle <- lat_wide$sht2_low_temp - lat_wide$leaftemp_middle
lat_wide$altd_top <- lat_wide$sht2_low_temp - lat_wide$leaftemp_top


### Max daily soil temp 
soil_temp$date <- date(soil_temp$by15)

# maxDailySoilTemp <- ddply(soil_temp, .(date, treatment), function(x){
#   setNames(max(x$soil_temp_C, na.rm=T), 'max_soil_temp')
# })
# 
# # merge back to soil_temp df
# soil_temp <- merge(soil_temp, maxDailySoilTemp)

### Max daily soil temp (up to time of measurment)
# x <- subset(soil_temp, date == '2019-10-24' & treatment=='full_drought')
# head(x)

x <- split(soil_temp, list(soil_temp$date, soil_temp$treatment))
(nm = names(x)[1])
y <- sapply(names(x), function(nm) {
  d <- x[[nm]]
  d$cummax_soil_temp <- cummax(d$soil_temp_C)
  x[[nm]][,'cummax_soil_temp'] <<- d$cummax
})
soil_temp2 <- do.call(rbind, x)


### Mean daily PAR (above threshold of 1 PAR), based on avg of 2 LQ sensors
lq$date <- date(lq$by15)
lq$hour <- hour(lq$by15)
lq$mean_PAR <- rowMeans(lq[ , c('line_PAR_east_umol_m2_s', 'line_PAR_west_umol_m2_s')])

x <- split(lq, lq$date)
(nm = '2019-12-12')
# y <- sapply(names(x), function(nm) {
#   d <- x[[nm]]
#   ind <- d$mean_PAR > 1
#   d$cummean_PAR <- NA
#   d$cummean_PAR[ind] <- cummean(d$mean_PAR[ind])
#   d$cummean_PAR[!ind] <- 0
#   x[[nm]][,'cummean_PAR'] <<- d$cummean_PAR
# })  

y <- sapply(names(x), function(nm) {
  # print(nm)
  par <- x[[nm]]$mean_PAR
  ind <- par > 1
  out <- sapply(1:length(ind), function(i) sum(ind[1:i]))
  # end <- which(duplicated(out) & out !=0)[1]
  # if(!is.na(end)) out[end:length(out)] <- 0
  x[[nm]][,'par_length'] <<- out
})
lq2 <- do.call(rbind, x)


### SECTION 3. Merge datasets and add more variables ---------------------


# Use pressure bomb means
comb <- merge(pbMeans, lq2, by='by15')
# Alternatively, use raw data
# comb <- merge(pb[,c('date','by15','psi_MPa','treatment','block')], lq2)
comb <- merge(comb, soil_temp2, by=c('by15','treatment','date'), all.x = T)
comb <- merge(comb, windWide, by=c('by15', 'treatment'), all.x = T)
comb <- merge(comb, lat_wide, by=c('by15', 'block','treatment','date'), all.x = T)

# rename column to "mean" to match code below
names(comb)[names(comb)%in%'psi_MPa'] <- 'mean_psi_MPa'

# ### For multiple sensors, calculate means and delete original columns for simplicity
# comb[ , c('line_PAR_east_umol_m2_s', 'line_PAR_west_umol_m2_s')] <- NULL # these columns were already averaged above
# comb$mean_pyr <- rowMeans(comb[ , c('pyr1_n', 'pyr2_s')])
# comb[ , c('pyr1_n', 'pyr2_s')] <- NULL
# comb$mean_high_temp <- rowMeans(comb[,c('sht1_high_temp', 'am2320_high_temp')])
# comb[,c('sht1_high_temp', 'am2320_high_temp')] <- NULL

# check for any duplicated columsn in merges above
which(grepl('\\.x', names(comb)) | grepl('\\.y', names(comb)))

# add "minutes" (of day) column
comb$minutes <- 60*hour(comb$by15) + minute(comb$by15)

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

### LEDs on (y/n)?
# comb$LED_on <- 'y'
# comb$LED_on[comb$by15 %in% c(as.POSIXct('2019-12-10 18:00:00', tz='GMT'),
#                              as.POSIXct('2019-12-11 06:15:00', tz='GMT'))] <- 'n'




### SECTION 4. Model fitting ---------------------

### CURRENTLY THE BEST R2
m <- lm(mean_psi_MPa ~ minutes + treatment + block + daysPostTrt +
           windspeed_middle + bmp_box_temp + soil_temp_C, data = comb); summary(m)

m <- lm(mean_psi_MPa ~ minutes + irrig + block + 
          bmp_box_temp + leaftemp_top , data = comb); summary(m)

plot(mean_psi_MPa ~ leaftemp_top , data = comb, col = treatment, pch = 16)
plot(leaftemp_top ~ VPD_leaf, data=comb,col=treatment)
plot(mean_psi_MPa ~ VPD_leaf , data = comb, col = treatment, pch = 16)
plot(mean_psi_MPa ~ minutes , data = comb, col = treatment, pch = 16)

plot(mean_psi_MPa ~ am2320_high_temp, data = comb)
plot(mean_psi_MPa ~ sht1_high_rh, data = comb)

plot(mean_psi_MPa ~ par_length, data = comb)


m <- lm(mean_psi_MPa ~ poly(am2320_high_temp, 2), data = comb[!is.na(comb$am2320_high_temp), ]); summary(m)
m <- lm(mean_psi_MPa ~ poly(bmp_box_temp, 2), data = comb[!is.na(comb$bmp_box_temp), ]); summary(m)


# LEAFTEMP is best predictor!
m <- lm(mean_psi_MPa ~ leaftemp_top, data = comb); summary(m)
m <- lm(mean_psi_MPa ~ leaftemp_top + poly(par_length,1), data = comb); summary(m)

m <- lm(mean_psi_MPa ~ leaftemp_top + treatment + minutes, data = comb); summary(m)
m <- lm(mean_psi_MPa ~ leaftemp_top + treatment + poly(minutes, 2), data = comb); summary(m)
m <- lm(mean_psi_MPa ~ leaftemp_top + treatment + poly(minutes, 2) + block, data = comb); summary(m)
# use irrig instead of treatment, basically the same R^2 but simpler model
m <- lm(mean_psi_MPa ~ leaftemp_top + irrig + poly(minutes, 2) + block, data = comb); summary(m)

m <- lm(mean_psi_MPa ~ poly(cummean_PAR, 2), data = comb); summary(m)
m <- lm(mean_psi_MPa ~ poly(mean_PAR, 1), data = comb); summary(m)
m <- lm(mean_psi_MPa ~ poly(minutes, 2), data = comb); summary(m)


m <- lm(mean_psi_MPa ~ cummax_soil_temp, data = comb); summary(m)
m <- lm(mean_psi_MPa ~ poly(mean_PAR, 1), data = comb); summary(m)

m <- lm(mean_psi_MPa ~ minutes + treatment + block + daysPostTrt , data = comb); summary(m)

library(caret)
library(randomForest)
library(glmnet)

### 4.1 Lasso Regression

df <- comb

# first, run simple linear mr with a few key variables...
m1 <- lm(df$mean_psi_MPa ~ df$windspeed_top + df$soil_temp_C + df$line_PAR_east_umol_m2_s + df$am2320_high_temp +
           df$am2320_high_rh + df$sht1_high_temp + df$sht1_high_rh + df$bmp_box_atm_p)
summary(m1)


###___________ lasso regression ______________####
###___________ lasso regression ______________####

# first subset to all possible independent variables and dependent variable
# use rows where "leaftemp_top" is not NA
# 
df2 <- subset(df, !is.na(leaftemp_top), select = -c(by15, date, leaftemp_bottom, leaftemp_middle,
                                                    altd_bottom, altd_middle))

# there can't be any missing values
df2 <- subset(df2, complete.cases(df2)); nrow(df2)

# create model matrix for predictor variables
x <- model.matrix(mean_psi_MPa ~ ., df2)[,-1]
# create vector for response variable
y <- df2$mean_psi_MPa


# set.seed(512)
train.prop <- 0.7
train <- sample(1:nrow(df2), nrow(df2) * train.prop); length(train)
test <- -train

grid <- 10^seq(10, -2, length = 100) # grid of lambda values to use in lasso

lasso.mod <- glmnet(x[train, ],
                    y[train],
                    family = 'gaussian',
                    alpha = 1, # alpha = 1 for lasso 
                    standardize = TRUE,
                    lambda = grid)
# lambda = grid) 
plot(lasso.mod, xvar = 'lambda')
plot(lasso.mod, xvar = 'dev')
# coef(lasso.mod)

# now... to choose an appropriate value of lambda run k-fold cross-validation
# note that this will give you slightly different distributions each time it is run
# the number of variables incldued in the model are across the top of the plot... and lambda across the bottom
# we want a model that minimizes MSE (y axis), but is also regulated to avoid over-fitting
# the dotted vertical line to the right is the value of lambda (and numer of fitted variables)
# that give you MSE that is within one SE of the mean... as a way to apply regularization
# as such the methods uses "k-fold" cross validation to select a lambda vallue that is within
# 1 SE of the minimum SE... which is somewhat arbitrary... but a different value could 
# be chosen to aid interpretation, i.e., for a biological reason...
# you can see that the 4-parameter model is nearly always within 1SE of the mean MSE
lasso.cv <- cv.glmnet(x[train, ], y[train], family='gaussian', alpha=1, standardize=T); plot(lasso.cv)

(lambda.star <- lasso.cv$lambda.min)
lasso_pred <- predict(lasso.mod,
                      s = lambda.star,
                      newx = x[test, ])
mean((lasso_pred - y[test])^2)

# Now that we know lambda*, fit on *full* data set
full_fit_lasso <- glmnet(x, y, alpha = 1, lambda = grid)
summary(full_fit_lasso)
lasso_coeffs <- predict(full_fit_lasso,
                        # glmnet fit on full data
                        type = "coefficients", # return betas; not predictions
                        s = lambda.star)
lasso_coeffs
str(lasso_coeffs)
nzCoef <- lasso_coeffs@Dimnames[[1]][which(lasso_coeffs != 0)]
nzCoef <- nzCoef[nzCoef != '(Intercept)']

lasso_coeffs[order(lasso_coeffs)]
lasso_coeffs %>% magrittr::extract(. != 0)

# plot predictions vs data
plot(lasso_pred, y[test]); abline(0, 1, col='red')

fullmod <- lm(y ~ x[ , nzCoef])
summary(fullmod)

mod1 <- lm(y ~ x[,"treatmentwell_watered"]) 
# summary(mod1)
mse1 <- mean(mod1$residuals^2); mse1; extractAIC(mod1)
plot(y ~ x[,'treatmentwell_watered'])


mod2 <- lm(y ~ x[,c("treatmentwell_watered",'blockM')]) 
summary(mod2)
mse2<- mean(mod1$residuals^2); mse2; extractAIC(mod1)
plot(y ~ x[,'blockM'])

mod3 <- lm(y ~ x[,c("treatmentwell_watered",'blockM','windspeed_middle')]) 
summary(mod3)
mse3<- mean(mod1$residuals^2); mse3; extractAIC(mod1)
plot(y ~ x[,'windspeed_middle'])
