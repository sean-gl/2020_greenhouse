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

# merge leaf temp and "RH" (includes air temp, rh, and light data)
lat <- merge(lt_block, rh)
lat$date <- lubridate::date(lat$by15)
# convert to wide
lat_wide <- tidyr::spread(lat, 'position', 'mean_leaftemp_C')
names(lat_wide)[names(lat_wide) %in% c('bottom','middle','top')] <- c('leaftemp_bottom','leaftemp_middle','leaftemp_top')

# Since position changes, sometimes data isn't available at a given position.
# Let's add a couple variables to handle these cases.

# Highest position with available data
lat_wide$leaftemp_highest_avail <- apply(lat_wide, 1, function(x) {
  ind <- which(!is.na(x[c('leaftemp_bottom','leaftemp_middle','leaftemp_top')]))
  as.numeric(x[c('leaftemp_bottom','leaftemp_middle','leaftemp_top')][max(ind)])
})

# Mean of all position's data
lat_wide$leaftemp_mean <- rowMeans(lat_wide[,c('leaftemp_bottom','leaftemp_middle','leaftemp_top')], na.rm = T)


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

## Make some edits to data this day...
pb$by15[date(pb$by15)=='2019-11-15' & pb$treatment=='moderate_drought'] <- '2019-11-15 13:45:00 GMT'
# pb$by15[date(pb$by15)=='2019-11-15' & pb$treatment=='well_watered'] <- '2019-11-15 13:45:00 GMT'

# get means by day and treatment/block
pb$block <- toupper(substr(pb$plant_id,1,1))
pb$date <- lubridate::date(pb$by15)
pbMeans <- ddply(pb, .(by15, block, treatment), function(x) {
  setNames(mean(x$psi_MPa), 'mean_psi_MPa')
})



### SECTION 3. Merge datasets and add more variables ---------------------

comb <- merge(lq, soil_temp, by=c('by15')); nrow(comb)
comb <- merge(comb, windWide, by=c('by15', 'treatment'), all.x = T); nrow(comb)
comb <- merge(comb, lat_wide, by=c('by15', 'treatment')); nrow(comb)
### Merge in actual pressure bomb data
comb_xonly <- merge(comb, pbMeans, all.x = T)

# rename column to "mean" to match code below
# names(comb)[names(comb)%in%'psi_MPa'] <- 'mean_psi_MPa'

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
cor(comb$sht1_high_rh, comb$am2320_high_rh, use = 'complete.obs')
cor(comb$sht2_low_rh, comb$sht1_high_rh,  use = 'complete.obs')
comb$rh_high_mean <- rowMeans(comb[ , c('sht1_high_rh','am2320_high_rh')], na.rm = T)
comb$VPD_leaf <- (1 - (comb$rh_high_mean / 100)) * 0.61121 * exp((17.502 * comb$leaftemp_highest_avail) / (240.97 + comb$leaftemp_highest_avail)) 
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

# AS GOOD, could add windspeed_middle in, if possible.
# could use PAR length instead of minutes.
m2 <- lm(mean_psi_MPa ~ minutes + irrig + block + 
          bmp_box_temp + leaftemp_mean, data = comb); summary(m)
mean(m2$residuals^2)
mean(abs(m2$residuals))

### TRUNCATED REGRESSION (doesn't seem to work well...)
require(truncreg)
m.trunc <- truncreg(mean_psi_MPa ~ minutes + irrig + block + 
                      bmp_box_temp + leaftemp_mean, data = comb,
                    point = 0, direction = "left")

summary(m.trunc)

library(caret)
library(randomForest)
library(glmnet)

### 4.1 Lasso Regression

# Keep soil_tempe, windspeed and VPD
df2 <- subset(comb, select = -c(by15, date, leaftemp_bottom, leaftemp_middle))
                                # altd_bottom, altd_middle))

# Omit those variables so we have more complete cases
df2 <- subset(df2, select = -c(leaftemp_top, soil_temp_C, windspeed_bottom, windspeed_middle, windspeed_top))

# there can't be any missing values
df2 <- subset(df2, complete.cases(df2)); nrow(df2)

# create model matrix for predictor variables
x <- model.matrix(mean_psi_MPa ~ ., df2)[,-1]
# create vector for response variable
y <- df2$mean_psi_MPa

# set.seed(51)
# train.prop <- 0.5
# train <- sample(1:nrow(df2), nrow(df2) * train.prop); length(train)
# test <- -train

# lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, standardize = T, nlambda = 100)
# plot(lasso.mod, label = T)
# print(lasso.mod)
# plot(lasso.mod, xvar = 'dev')

### REPEAT THE CROSS-VALIDATION N TIMES, TO SEE WHICH VARIABLES ARE CONSISTENTLY IMPORTNAT
# list to store variables
nreps <- 10
nzcList <- list()
for(i in 1:nreps) {
  
  # CV using full dataset
  lasso.cv <- cv.glmnet(x, y, family='gaussian', alpha=1, nfolds=5, standardize=T)
  # plot(lasso.cv)
  lasso.cv$lambda.1se
  
  # Now that we know lambda, fit on *full* data set
  full_fit_lasso <- glmnet(x, y, alpha = 1, lambda = lasso.cv$lambda.1se)
  
  # summary(full_fit_lasso)
  lasso_coeffs <- predict(full_fit_lasso,
                          type = "coefficients", # return betas; not predictions
                          s = lasso.cv$lambda.1se)
  nzCoef <- lasso_coeffs@Dimnames[[1]][which(lasso_coeffs != 0)]
  nzCoef <- nzCoef[nzCoef != '(Intercept)']
  nzcList[[i]] <- nzCoef
}
z=unlist(nzcList)
b=sort(unique(unlist(nzcList)))
sapply(b, function(a) length(z[z == a]))

# THESE 7 VARIABLES SEEM MOST IMPORTANT:
# blockM, bMP_box_temp, irrig, leaftemp_mean, minutes, sht2_low_rh, "treatmentwell_watered" (somewhat)
# leaftemp_top and windspeed_top important only if using these variables (but results in smaller n)
# Or using "derived variables", par_length also important
# to a much lesser degree, "daysPostTrt" is also important

# CV using full dataset
lasso.cv <- cv.glmnet(x, y, family='gaussian', alpha=1, nfolds=5, standardize=T); plot(lasso.cv)
lasso.cv$lambda.1se

# Now that we know lambda, fit on *full* data set
full_fit_lasso <- glmnet(x, y, alpha = 1, lambda = lasso.cv$lambda.1se)

# summary(full_fit_lasso)
lasso_coeffs <- predict(full_fit_lasso,
                        type = "coefficients", # return betas; not predictions
                        s = lasso.cv$lambda.1se)
nzCoef <- lasso_coeffs@Dimnames[[1]][which(lasso_coeffs != 0)]
nzCoef <- nzCoef[nzCoef != '(Intercept)']
nzCoef

# predictions on full data set
lasso_pred_full <- predict(full_fit_lasso, s = lasso.cv$lambda.1se, newx = x)
mean((lasso_pred_full - y)^2)
mean(abs(lasso_pred_full - y))
plot(lasso_pred_full, y); abline(0, 1, col='red')

# Use all 7 variables (full model)
fullmod <- lm(y ~ x[ , nzCoef])
summary(fullmod)
mean(fullmod$residuals^2); mean(abs(fullmod$residuals))

# try omitting variables w/low p-values
mod1 <- lm(y ~ x[,"irrig"]) 
summary(mod1)
mean(mod1$residuals^2); mean(abs(mod1$residuals))

# Almost as good as full model but with only 3 variables.
mod2 <- lm(y ~ x[,c('bmp_box_temp','minutes','irrig')]) 
# using original data
mod2 <- lm(mean_psi_MPa ~ bmp_box_temp + minutes + irrig, df2)
summary(mod2)
mean(mod2$residuals^2); mean(abs(mod2$residuals))

mod2 <- lm(y ~ x[,c('bmp_box_temp','par_length','irrig','leaftemp_top')]) 
mod2 <- lm(y ~ x[,c('bmp_box_temp','par_length','irrig','blockM')]) 
summary(mod2)
mean(mod2$residuals^2); mean(abs(mod2$residuals))

### BEST SUBSETS REGRESSION

require(leaps)
colnames(x)
which(colnames(x) == 'irrig')
x2 <- x[ , -c(1:5)]
regfit.full <- regsubsets(x2, y, nvmax = 10)
rs <- summary(regfit.full)
rs$adjr2
rs




### RANDOM FOREST BOOSTING

# there can't be any missing values
# df <- comb[!is.na(comb$leaftemp_top), ]
# df2 <- comb[, !names(comb) %in% c('by15','date',
                                  # 'cumsum_altd_bottom','cumsum_altd_middle','cumsum_altd_top')]
df2 <- comb[!is.na(comb$windspeed_bottom),]
df2 <- df2[, !names(df2) %in% c('by15','date')]
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
# train_n <- round(0.7 * nrow(df2))
# test_n <- nrow(df2) - train_n
# train_ind <- sample(1:nrow(df2), train_n, replace = F)
# train_data <- df2[train_ind,]
# test_data <- df2[-train_ind,]

### Boosting
require(gbm)

m.boost <- gbm(mean_psi_MPa ~ .,
               data = df2,
               distribution = 'gaussian',
               n.trees = 50,
               interaction.depth = 1,
               shrinkage = 0.1,
               bag.fraction = 0.5,
               cv.folds = 5)

plot(1:length(m.boost$cv.error), m.boost$cv.error)
plot(1:length(m.boost$train.error), m.boost$train.error)

# m.boost
# summary(m.boost)
# yhat <- predict(m.boost, newdata = test_data, n.trees = 5000)
# y <- test_data$mean_psi_MPa
yhat <- predict(m.boost, newdata = df2, n.trees = 5000)
y <- df2$mean_psi_MPa
diffs <- y-yhat
mean(diffs^2) # test MSE
mean(abs(diffs)) # test MAD
summary(diffs)
# preds <- data.frame(predicted_psi = yhat, actual_psi=test_data$mean_psi_MPa)
preds <- data.frame(predicted_psi = yhat, actual_psi=df2$mean_psi_MPa)
plot(predicted_psi ~ actual_psi, data=preds)
abline(0,1, col='red')



### SECTION 5. Make predictions based on models -------------

# recombine data, this time without pressure bomb data
# summary(date(lq$by15))
# summary(date(soil_temp$by15))
# summary(date(windWide$by15))
# summary(date(lat_wide$by15))
# nrow(lq); nrow(soil_temp)

comb_xonly <- merge(lq, soil_temp, by=c('by15')); nrow(comb_xonly)
comb_xonly <- merge(comb_xonly, windWide, by=c('by15', 'treatment'), all.x = T); nrow(comb_xonly)
comb_xonly <- merge(comb_xonly, lat_wide, by=c('by15', 'treatment')); nrow(comb_xonly)

# add "minutes" (of day) column
comb_xonly$minutes <- 60*hour(comb_xonly$by15) + minute(comb_xonly$by15)

# add irrigation amount (ml)
comb_xonly$date <- date(comb_xonly$by15)
comb_xonly$irrig <- NA
comb_xonly$irrig[comb_xonly$date < "2019-11-05" & comb_xonly$treatment == 'well_watered'] <- 750
comb_xonly$irrig[comb_xonly$date >= "2019-11-05" & comb_xonly$treatment == 'well_watered'] <- 1000
comb_xonly$irrig[comb_xonly$treatment == 'moderate_drought'] <- 375
comb_xonly$irrig[comb_xonly$treatment %in% c('full_drought','virgin_drought')] <- 150
table(comb_xonly$irrig)

# calculate VPD_leaf based on leaf temperature
comb_xonly$rh_high_mean <- rowMeans(comb_xonly[ , c('sht1_high_rh','am2320_high_rh')])
comb_xonly$VPD_leaf <- (1 - (comb_xonly$rh_high_mean / 100)) * 0.61121 * exp((17.502 * comb_xonly$leaftemp_top) / (240.97 + comb_xonly$leaftemp_top)) 
summary(comb_xonly$VPD_leaf)

### Add days since treatment started
summary(comb_xonly$date)
comb_xonly$daysPostTrt <- NA
ind <- comb_xonly$date < '2019-11-05'
comb_xonly$daysPostTrt[ind] <- comb_xonly$date[ind] - as.Date('2019-10-25')
ind <- comb_xonly$date > '2019-11-04' & comb_xonly$date < '2019-11-28'
comb_xonly$daysPostTrt[ind] <- comb_xonly$date[ind] - as.Date('2019-11-05')
ind <- comb_xonly$date > '2019-11-27' 
comb_xonly$daysPostTrt[ind] <- comb_xonly$date[ind] - as.Date('2019-11-28')
summary(comb_xonly$daysPostTrt)

### Merge in actual pressure bomb data
comb_xonly <- merge(comb_xonly, pbMeans, all.x = T)

### Make predictions based on some models
# comb_xonly$yhat_m1 <- predict(m.trunc, newdata = comb_xonly)

# LINEAR MODEL (MANUAL SELECTION)
comb_xonly$yhat_m1 <- predict(m2, newdata = comb_xonly)

# LASSO PREDICTIONS
comb_xonly$yhat_lasso <- predict(mod2, newdata = comb_xonly)

# RANDOM FOREST BOOSTING
comb_xonly$yhat_rfboost <- predict(m.boost, newdata = comb_xonly, n.trees = 50)

summary(comb_xonly$yhat_m1)
plot(density(comb$mean_psi_MPa))
qqnorm(comb$mean_psi_MPa); qqline(comb$mean_psi_MPa)
plot(mod2)
plot(density(comb_xonly$yhat_m1))
plot(density(comb_xonly$yhat_m1[comb_xonly$treatment=='full_drought']))
plot(density(comb_xonly$yhat_m1[comb_xonly$treatment=='moderate_drought']))
plot(density(comb_xonly$yhat_m1[comb_xonly$treatment=='well_watered'])) # most the negative values are for well-watered...
plot(density(comb_xonly$yhat_m1[comb_xonly$treatment=='virgin_drought']))





### Plot the predicted psi_leaf

head(comb_xonly)
# plot 2nd treatments
sub <- subset(comb_xonly, date(by15) >= '2019-11-11' & date(by15) <= '2019-11-12')
# plot 3rd treatments
sub <- subset(comb_xonly, date(by15) >= '2019-12-01' & date(by15) <= '2019-12-12')
sub <- subset(comb_xonly, date(by15) == '2019-11-20')

ggplot(sub, aes(x=by15, y=yhat_m1, color=treatment)) + geom_line() +
  # geom_line(aes(x=by15, y=leaftemp_mean/10)) +
  geom_point(aes(x=by15, y=mean_psi_MPa), size=3)

ggplot(sub) +
  geom_line(aes(x=by15, y=yhat_rfboost, color=treatment)) +
  # geom_line(aes(x=by15, y=leaftemp_mean/10, color=treatment)) +
  geom_point(aes(x=by15, y=mean_psi_MPa, color=treatment), size=3)

