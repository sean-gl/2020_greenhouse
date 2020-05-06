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
lt <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/data/leaf_thermistor_data/leaf_thermistor_data_15min_agg_flagged.rds')

# remove position column, not useful
lt$position <- NULL
colnames(lt)[colnames(lt)=='canopy_position'] <- 'position'
# change position categoies to match wind data
lt$position[lt$position=='lower'] <- 'bottom'
lt$position[lt$position=='upper'] <- 'top'

# filter data by flag 
lt_filter <- subset(lt, flag <= 3 & temperature_flag == 'none')
nrow(lt_filter)/nrow(lt)

# Aggregate by block
lt_block <- ddply(lt_filter, .(by15, block, treatment, position), function(x){
  setNames(mean(x$mean_leaftemp_C, na.rm = T), 'mean_leaftemp_C')
})

# convert to wide
lt_block_wide <- tidyr::spread(lt_block, 'position', 'mean_leaftemp_C')
head(lt_block_wide)
colnames(lt_block_wide) <- c('by15','block','treatment','leaftemp_bottom','leaftemp_middle','leaftemp_top')

# lt_block_sub <- subset(lt_block, date(by15) > '2019-11-01' &
#                                 date(by15) < '2019-11-05')
# ggplot(lt_block_sub, aes(x=by15, y=mean_leaftemp_C, color=block)) +
#   geom_point() + geom_line() +
#   facet_grid(~position)

# 2. PAR
lq <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/line_PAR_sensors/line_PAR_15.csv')
lq$by15 <- as.POSIXct(lq$by15, tz = 'GMT')

# 3. RH and soil temperature
rh <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/RH_temp_PAR_logger_data/rh_15.csv')
rh$by15 <- as.POSIXct(rh$by15, tz='GMT')
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


### Merge air temp data to lt to compare
lat <- merge(lt_block,
             rh[ , c('by15', 'bmp_box_temp','sht1_high_temp')])
summary(lat)
lat$date <- lubridate::date(lat$by15)
require(ggplot2)
# 
# sub <- subset(lat, position == 'bottom' & date > '2019-11-01' & date < '2019-11-03')
# ggplot(sub, aes(x=by15)) +
#   geom_line(aes(y=mean_leaftemp_C, colour = 'leaf')) +
#   geom_line(aes(y=sht1_high_temp, colour = 'air')) +
#   facet_grid(~block)
  

####_______________________combine data to create continous PSI_leaf data______________###
####_______________________combine data to create continous PSI_leaf data______________###
####_______________________combine data to create continous PSI_leaf data______________###

# combine by 15-min date and time
comb <- merge(pbMeans, rh, by='by15')
comb <- merge(comb, lq, by='by15')
comb <- merge(comb, soil_temp, by=c('by15','treatment'), all.x = T)
## NOTE: Lose some rows here, are there missing wind data? 
comb <- merge(comb, windWide, by=c('by15', 'treatment'), all.x = T)
comb <- merge(comb, lt_block_wide, by=c('by15', 'block','treatment'), all.x = T)

### Since leaf temperature does not have complete cases, create a new column with mean leaf temp
### regardles of height in canopy
comb$leaftemp_mean <- rowMeans(comb[ , c('leaftemp_bottom', 'leaftemp_middle', 'leaftemp_top')], na.rm = T)

# air, leaf temp diff
comb$airleaf_diff <- comb$sht2_low_temp - comb$leaftemp_mean

# calculate VPD_leaf based on leaf temperature
cor(comb$sht1_high_rh, comb$am2320_high_rh)
cor(comb$sht2_low_rh, comb$sht1_high_rh)
comb$rh_high_mean <- rowMeans(comb[ , c('sht1_high_rh','am2320_high_rh')])
comb$VPD_leaf <- (1 - (comb$sht2_low_rh / 100)) * 0.61121 * exp((17.502 * comb$leaftemp_top) / (240.97 + comb$leaftemp_top)) 
# USE MEAN LEAFTEMP INSTEAD
comb$VPD_leaf <- (1 - (comb$sht2_low_rh / 100)) * 0.61121 * exp((17.502 * comb$leaftemp_mean) / (240.97 + comb$leaftemp_mean)) 

# use air temp to get VPD instead
comb$VPD_air <- (1 - (comb$rh_high_mean / 100)) * 0.61121 * exp((17.502 * comb$sht1_high_temp) / (240.97 + comb$sht1_high_temp)) 

summary(comb$VPD_leaf)
plot(comb$VPD_air, comb$VPD_leaf)

# add machine learning and lasso libraries
library(caret)
library(randomForest)
library(glmnet) 


### ---- Analysis (Model Building)

names(comb)
dat <- comb[, !names(comb) %in% c('date','by15','leaftemp_bottom',
                                  'leaftemp_middle','altd_bottom','altd_middle',
                                  'cumsum_altd_bottom','cumsum_altd_middle')]
### COMPLETE MODEL
m1 <- lm(mean_psi_MPa ~ ., data=dat)
summary(m1)

m <- lm(mean_psi_MPa ~ minutes + block + treatment +
          leaftemp_top + windspeed_middle + airleaf_diff + 
          line_PAR_east_umol_m2_s + line_PAR_west_umol_m2_s, data = dat)
summary(m)
m <- lm(mean_psi_MPa ~ minutes + block + treatment +
          leaftemp_top + windspeed_middle + airleaf_diff, data = dat)
summary(m)

### CURRENTLY THE BEST R2
m <- lm(mean_psi_MPa ~ minutes + treatment + block +
          leaftemp_top + windspeed_middle + bmp_box_temp, data = dat)
summary(m)


### OR THIS ONE (USING MEAN LEAF TEMP TO GET VPD_leaf)
m <- lm(mean_psi_MPa ~ minutes + block + 
          VPD_leaf + windspeed_middle + bmp_box_temp + irrig, data = dat)
summary(m)
m <- lm(mean_psi_MPa ~ minutes + block + 
          VPD_air + windspeed_middle + bmp_box_temp + irrig, data = dat)
summary(m)

m <- lm(mean_psi_MPa ~ minutes + block + treatment +
          leaftemp_top + windspeed_middle + bmp_box_temp , data = dat)
summary(m)

# first, run simple linear mr with a few key variables...
# m1 <- lm(mean_psi_MPa ~ par1_n + par2_s + pyr1_n + pyr2_s +
#            am2320_high_temp + am2320_high_rh + sht1_high_temp + sht1_high_rh +
#            sht2_low_temp + sht2_low_rh + line_PAR_east_umol_m2_s + line_PAR_west_umol_m2_s +
#            windspeed_bottom + windspeed_middle + windspeed_top + 
#            leaftemp_bottom + leaftemp_middle + leaftemp_top, data = comb)

## TOP
m1 <- lm(mean_psi_MPa ~ par1_n + par2_s + pyr1_n + pyr2_s +
           am2320_high_temp + am2320_high_rh + sht1_high_temp + sht1_high_rh +
           sht2_low_temp + sht2_low_rh + line_PAR_east_umol_m2_s + line_PAR_west_umol_m2_s +
           windspeed_top + leaftemp_top, data = comb)
m2 <- lm(mean_psi_MPa ~ par1_n + par2_s +
           sht2_low_temp + sht2_low_rh + line_PAR_east_umol_m2_s + line_PAR_west_umol_m2_s +
           windspeed_top + leaftemp_top, data = comb)
summary(m1); summary(m2)

### MIDDLE
m1 <- lm(mean_psi_MPa ~ par1_n + par2_s + pyr1_n + pyr2_s +
           am2320_high_temp + am2320_high_rh + sht1_high_temp + sht1_high_rh +
           sht2_low_temp + sht2_low_rh + line_PAR_east_umol_m2_s + line_PAR_west_umol_m2_s +
           windspeed_middle + leaftemp_middle, data = comb)

m2 <- lm(mean_psi_MPa ~ par1_n + par2_s + pyr1_n + pyr2_s +
           am2320_high_temp + 
           windspeed_middle + leaftemp_middle, data = comb)
summary(m1); summary(m2)


### RANDOM FOREST MODEL

# there can't be any missing values
# df <- comb[!is.na(comb$leaftemp_top), ]
df2 <- comb

# omit non-predictor vars
names(df2)
df2[,c('by15','block','treatment')] <- NULL
# replace NaN with NA (required for boosting)
df2$leaftemp_mean[is.nan(df2$leaftemp_mean)] <- NA
df2$airleaf_diff[is.nan(df2$airleaf_diff)] <- NA

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
               shrinkage = 0.1)

# summary(m.boost)
yhat.boost <- predict(m.boost, newdata = test_data, n.trees = 5000)
mean((yhat.boost - test_data$mean_psi_MPa)^2) # test MSE
mean(abs(yhat.boost - test_data$mean_psi_MPa)) # test MAD
preds <- data.frame(predicted_psi = yhat.boost, actual_psi=test_data$mean_psi_MPa)
plot(predicted_psi ~ actual_psi, data=preds)
abline(0,1, col='red')

# training model... will take some time to run...
# model <- train(mean_psi_MPa ~ par1_n + par2_s + pyr1_n + pyr2_s +
#                  am2320_high_temp + am2320_high_rh + sht1_high_temp + sht1_high_rh +
#                  sht2_low_temp + sht2_low_rh + line_PAR_east_umol_m2_s + line_PAR_west_umol_m2_s +
#                  windspeed_bottom + windspeed_middle + windspeed_top + leaftemp_mean,
#                data = train_data, 
#                method = 'rf', # Use the 'random forest' algorithm
#                trControl = trainControl(method = 'cv', # Use cross-validation
#                                         number = 5),
#                importance=TRUE) # Use 5 folds for cross-validation

# model <- train(mean_psi_MPa ~ par1_n + par2_s + pyr1_n + pyr2_s +
#                  am2320_high_temp + am2320_high_rh + sht1_high_temp + sht1_high_rh +
#                  sht2_low_temp + sht2_low_rh + bmp_box_temp + bmp_box_atm_p +
#                  line_PAR_east_umol_m2_s + line_PAR_west_umol_m2_s +
#                  leaftemp_mean + soil_temp_C,
#                data = train_data, 
#                method = 'rf', # Use the 'random forest' algorithm
#                trControl = trainControl(method = 'cv', # Use cross-validation
#                                         number = 10),
#                # na.action = na.roughfix,
#                importance=TRUE) # Use 5 folds for cross-validation
# 
# model
# varImp(model)


### SIMPLE LINEAR MODELS

m3 <- lm(mean_psi_MPa ~ 
           am2320_high_temp +  sht1_high_temp + 
           sht2_low_temp +  bmp_box_temp + sht2_low_rh +
           leaftemp_mean + soil_temp_C, data = df2)
summary(m3)

m3 <- lm(mean_psi_MPa ~ 
           am2320_high_temp + am2320_high_rh + sht1_high_temp + sht1_high_rh +
           sht2_low_temp + sht2_low_rh + bmp_box_temp +
           leaftemp_mean, data = df2)


model <- train(mean_psi_MPa ~ par1_n + par2_s + pyr1_n + pyr2_s +
                 am2320_high_temp + am2320_high_rh + sht1_high_temp + sht1_high_rh +
                 sht2_low_temp + sht2_low_rh + bmp_box_temp + bmp_box_atm_p +
                 line_PAR_east_umol_m2_s + line_PAR_west_umol_m2_s +
                 leaftemp_mean,
               data = comb)



### ---------- NEURAL NETS

data <- data.matrix(comb[,-1])

train_n <- 0.5 * nrow(data)
train_data <- data[1:train_n,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)

library(keras)

model <- keras_model_sequential() %>% 
  layer_flatten(input_shape = c(lookback / step, dim(data)[-1])) %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)