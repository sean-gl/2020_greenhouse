
rm(list = ls())

packages <- c('lubridate','plyr','ggplot2')
lapply(packages, require, character.only = TRUE)
Sys.setenv(tz='GMT')


# ------ Section 1: Data Preparation

# read data (all variables plus tranpsiration and predicted leaf water potential)
dat  <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_psi_leaf/combined_data_predict_psi_leaf_transpiration.rds')
names(dat)

# add hour column
dat$hour <- hour(dat$by15)

# change negative values  of T to zero
ind <- dat$mean_T_mg_s < 0 & !is.na(dat$mean_T_mg_s)
nrow(dat[ind, ]) / nrow(dat) # about 4% of data are < 0
dat$mean_T_mg_s[ind] <- 0

# read in leaf area (modeled) data
# note: the follwing dates and blocks were outside the range of any measurments, 
# so we may wish to exclude the leaf area estimates for them:
# 10/24 to 10/29 (all blocks), and 11-28 to 12-05 (W block only (virgins))
la <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/continuous_LA_pred_for_analysis.rds')




# Merge leaf area to other data
dat <- merge(dat, la, by = c('date','block'), all.x = T)
summary(dat$mean_plant_leaf_area_cm2) 
# note: 10-23 doesn't have leaf area data; but this is before treatments began so we will
# exclude this data anyway
dat <- subset(dat, date >= '2019-10-24')


# --- Transpiration needs to be scaled by leaf area. Since we are doing the analysis 
# at the block-level, we use the (mean) total plant leaf area in each block, and the 
# mean transpiration for each block.
# also we want to convert from cm2 to m2 so we divide by 10000.
dat$mean_plant_leaf_area_m2 <- (dat$mean_plant_leaf_area_cm2) / 1E4
dat$mean_plant_leaf_area_cm2 <- NULL
dat$mean_T_mg_m2_s <- dat$mean_T_mg_s / dat$mean_plant_leaf_area_m2
summary(dat$mean_plant_leaf_area_m2)
summary(dat$mean_T_mg_m2_s)
summary(dat$mean_T_mg_s)

# Examine the Transpiration data
# trt 1
subdat <- subset(dat, date >= '2019-10-24' & date <= '2019-11-04')
# trt 2
subdat <- subset(dat, date >= '2019-11-05' & date <= '2019-11-27')
# trt 3
subdat <- subset(dat, date >= '2019-11-28')

ggplot(subdat, aes(x=by15, y=mean_T_mg_s, color=block)) + 
  geom_line()
ggplot(subdat, aes(x=by15, y=mean_T_mg_m2_s, color=block)) + 
  geom_line()

# Now remove the unscaled transpiration variable
dat$mean_T_mg_s <- NULL


### Next, merge in the empirical calculation of soil water potentials ---
swp <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/modeled_psi_soil_block_means.rds')
head(swp)
dat <- merge(dat, swp, all.x = T)



# ------ Section 2: Modeling using Lasso for different time periods.


# First, split the data into daily time periods.

x = subset(dat, date >= '2019-10-29' & date <= '2019-10-30')

ggplot(x) +
  geom_line(aes(x=by15, y=line_PAR_east_umol_m2_s)) 

ggplot(x) +
  geom_line(aes(x=by15, y=mean_T_mg_m2_s, color=block)) 

x = subset(dat, date == '2019-11-05' & block == 'D') 
# 2:45-3:45, peak
plot(x$by15, x$mean_T_mg_m2_s); identify(x$by15, x$mean_T_mg_m2_s, labels = x$by15)


### What time does sun come up generally?
# PAR threshold of 10....
# Oops, there's a problem, on 11/3 the times jump back by 1 hour, which shouldn't happen!
# FUUUUUUUUCK...
x = dat[dat$block=='W' & dat$date == '2019-10-28',]
sundat <- ddply(dat[dat$block=='W',], .(date), function(x) {
  x <- x[order(x$by15), ]
  sunup <- which(x$line_PAR_west_umol_m2_s > 1)
  start <- sunup[1]
  end <- sunup[length(sunup)]
  return(data.frame(date=unique(x$date), 
                    sunrise=hour(x$by15[start])+minute(x$by15[start])/60,
                    sunset=hour(x$by15[end])+minute(x$by15[end])/60))
})
View(sundat) # looks like sun comes up at 6:45 at the earliest....

sundat <- sundat[-1,] # 1st day bad
plot(sundat$date, sundat$sunrise, type = 'b', main = "Sunrise (line_PAR_east_umol_m2_s > 10)", xlab='date', ylab='hour')
plot(sundat$date, sundat$sunset, type = 'b', main = "Sunset (line_PAR_east_umol_m2_s < 10)", xlab='date', ylab='hour')

## NOW, look at pyranometer data
x = dat[dat$block=='W' & dat$date == '2019-10-26',]

sundat <- ddply(dat[dat$block=='W',], .(date), function(x) {
  x <- x[order(x$by15), ]
  lag <- 3
  x$par1_n_diff <- c(diff(x$par1_n, lag = lag), rep(NA, lag))
  plot(x$par1_n_diff); abline(c(0,0))
  rle(x$par1_n_diff)
  # z <- sapply(1:nrow(x), function(i, w=6) {
  #   if((i+w)<nrow(x)) {
  #     d=x[i:(i+w),]
  #     # m=mean(d$par1_n_diff, na.rm = T)
  #     m=as.numeric(coef(lm(par1_n~by15, d))['by15'])
  #   } else m=NA
  #   return(m)
  # })
  # x$par1_n_slope <- z
  # x$par1_n_diff <- c(NA, diff(x$par1_n))
  # fil_len <- 10
  # fil <- rep(1/fil_len, fil_len)
  # x$par1_n_filter <- filter(x$par1_n, f5, sides = 1)
  plot(x$par1_n ~ x$by15)
  points(x$par1_n_slope*1e4~ x$by15, type='b', col='red')
  # lines(x$par1_n_filter ~ x$by15, type='b',col='red')
  sunup <- which(x$pyr1_n > 0)
  start <- sunup[1]
  end <- sunup[length(sunup)]
  return(data.frame(date=unique(x$date), 
                    sunrise=hour(x$by15[start])+minute(x$by15[start])/60,
                    sunset=hour(x$by15[end])+minute(x$by15[end])/60))
})
View(sundat) # looks like sun comes up at 6:45 at the earliest....

sundat <- sundat[-1,] # 1st day bad
plot(sundat$date, sundat$sunrise, type = 'b', main = "Sunrise (pyr1_n > 0)", xlab='date', ylab='hour')
plot(sundat$date, sundat$sunset, type = 'b', main = "Sunset (pyr1_n < 0)", xlab='date', ylab='hour')




sundown <- ddply(dat[dat$block=='W',], .(date), function(x) {
  x <- x[order(x$by15), ]
  end <- which(x$line_PAR_east_umol_m2_s > 10)
  end <- end[length(end)]
  return(x$by15[end])
})
View(sundown) # looks like sun does down by 16:45 on average

sunmax <- ddply(dat[dat$block=='W',], .(date), function(x) {
  x <- x[order(x$by15), ]
  max <- which(x$line_PAR_east_umol_m2_s == max(x$line_PAR_east_umol_m2_s, na.rm = T))[1]
  minutes <- hour(x$by15[max]) + minute(x$by15[max])/60
  maxpar <- x$line_PAR_east_umol_m2_s[max]
  return(data.frame(minutes=minutes, max_par=maxpar))
})
plot(sunmax$minutes, sunmax$max_par) # looks like max PAR is 12-12:30, esp. on sunny days
# on low-par days, it's generally later (between 13-14) or earlier (10-11)
boxplot(sunmax$minutes)
hist(sunmax$minutes)


# --- Define Time Periods
# 1. Night:  10:00 to 02:00 am (dark)
# 2. Early morning: (avoid LED 'spike' in T? If so then wait until 4 am). Lets say 4-6:30
# 3. Ramp-up to max PAR: 7:00 to 10:00
# 4. Midday (max PAR): 11:00 to 14:00
# 5. Late afternoon: 15:00 to 18:00

# Create backup of master dataframe
dat_backup <- dat

# Add "day" column (days since sart of experminet, 10-24)
dat$day <- dat$date - min(dat$date) + 1

# Use "minutes" column to split data into list according to time periods:
dat$period <- NA
dat$period[dat$minutes >= 21*60 | dat$minutes <= 2*60] <- 'night'
dat$period[dat$minutes >= 4*60 & dat$minutes <= 6*60+30] <- 'early_morning'
dat$period[dat$minutes >= 7*60 & dat$minutes <= 10*60] <- 'mid_morning'
dat$period[dat$minutes >= 11*60 & dat$minutes <= 14*60] <- 'midday'
dat$period[dat$minutes >= 15*60 & dat$minutes <= 18*60] <- 'late_afternoon'
dat$period <- as.factor(dat$period)
table(dat$period, useNA = 'always')


# Remove columns we don't care about for Lasso model.
names(dat)
dat <- subset(dat, select = -c(date, by15, block, treatment, daysPostTrt, hour, minutes,
                               irrig, mean_irrig_kg, stress_index,
                               mean_psi_MPa, # this was only measured 60 times
                               leaftemp_bottom, leaftemp_middle, leaftemp_top, # many NA
                               leaftemp_mean, leaftemp_highest_avail,
                               sd_T_mg_s, n_T_mg_s, mean_plant_leaf_area_m2,
                               VPD_leaf)) #NOTE: NEED TO redo VPD_leaf calculation!


### Examine the data, which columns contain the most NA?
### (We cannot have missing values for Lasso...unless we wish to impute them...?)
naRows <- apply(dat, 2, function(x)  length(which(is.na(x))))
naRows[order(naRows, decreasing = T)]

# Get complete cases for lasso
nrow(dat)
dat <- dat[complete.cases(dat), ]; nrow(dat)

# Split data into list according to time period
periodList <- split(dat, dat$period)
sapply(periodList, nrow)

df = periodList$early_morning

df = periodList$midday

require(glmnet)

# Function to fit a lasso model to a dataframe
fitLasso <- function(df) {
  
  # omit unwanted variables
  df <- subset(df, select = -period)
  
  # add a couple random variables just to check for spurious correlation
  df$random1 <- rnorm(nrow(df))
  df$random2 <- rnorm(nrow(df))
  
  # define model matrix and response
  x <- model.matrix(mean_T_mg_m2_s ~ ., df)
  y <- df$mean_T_mg_m2_s
  
  # split into train/test sets
  propTrain <- 0.75
  train <- sample(1:nrow(df), nrow(df)*propTrain)
  test <- -(train)
  
  # create grid of lambda values to use in lasso 
  lambdaGrid <- 10^seq(10, -2, length = 100)
  
  # fit a lasso model
  lassoModel <- glmnet(x[train, ], y[train], alpha = 1, lambda = lambdaGrid, standardize = T)
  
  # perform cross-validation to select best lambda value
  cvOut <- cv.glmnet(x=x[train, ], y=y[train], alpha = 1, standardize = T) 
  plot(cvOut)
  bestLambda <- cvOut$lambda.1se
  log(bestLambda)
  lassoPred <- predict(cvOut, s = bestLambda, newx = x[test,])
  resid <- lassoPred - y[test]
  sqrt(mean(resid^2)) # RMSE
  mean(abs(resid)) # MAE
  mean(resid) # MBE
  
  # fit lasso model to full data set
  lassoFull <- glmnet(x, y, alpha = 1, lambda = lambdaGrid, standardize = T)
  
  # get coefficients
  lc <- predict(lassoFull, type = 'coefficients', s = bestLambda)
  lc <- as.matrix(lc)
  lc <- data.frame(variable = dimnames(lc)[[1]], coef = round(as.numeric(lc), 2), stringsAsFactors = F)
  lc <- lc[lc$coef != 0, ]
  lc <- lc[order(abs(lc$coef), decreasing = T), ]
  last <- grep('random', lc$variable)
  if(length(last) > 0) {
    lc <- lc[1:(last[1]-1),]
  }
  last <- grep('random', lc$variable)[1] -1 
  # print(nrow(lc))
  # print(last)
  
  lc
  nrow(lc)
  return(lc)
}


require(tidyr); require(dplyr)
nreps <- 20

modelRuns_early_morning <- lapply(1:nreps, function(x) fitLasso(df = periodList$early_morning))
modelRuns_early_morning <- do.call(rbind, modelRuns_early_morning)

modelRuns_early_morning %>% group_by(variable) %>% 
  summarise(mean_val = mean(coef), sd=sd(coef), n=n()) %>%
  arrange(desc(abs(mean_val))) %>% filter(n >= nreps*0.75)

modelRuns_mid_morning <- lapply(1:nreps, function(x) fitLasso(df = periodList$mid_morning))
modelRuns_mid_morning <- do.call(rbind, modelRuns_mid_morning)

modelRuns_mid_morning %>% group_by(variable) %>% 
  summarise(mean_val = mean(coef), sd=sd(coef), n=n()) %>%
  arrange(desc(abs(mean_val))) %>% filter(n >= nreps*0.75)

modelRuns_midday <- lapply(1:nreps, function(x) fitLasso(df = periodList$midday))
modelRuns_midday <- do.call(rbind, modelRuns_midday)

modelRuns_midday %>% group_by(variable) %>% 
  summarise(mean_val = mean(coef), sd=sd(coef), n=n()) %>%
  arrange(desc(abs(mean_val))) %>% filter(n >= nreps*0.75)

modelRuns_late_afternoon <- lapply(1:nreps, function(x) fitLasso(df = periodList$late_afternoon))
modelRuns_late_afternoon <- do.call(rbind, modelRuns_late_afternoon)

modelRuns_late_afternoon %>% group_by(variable) %>% 
  summarise(mean_val = mean(coef), sd=sd(coef), n=n()) %>%
  arrange(desc(abs(mean_val))) %>% filter(n >= nreps*0.75)

modelRuns_night <- lapply(1:nreps, function(x) fitLasso(df = periodList$night))
modelRuns_night <- do.call(rbind, modelRuns_night)

modelRuns_night %>% group_by(variable) %>% 
  summarise(mean_val = mean(coef), sd=sd(coef), n=n()) %>%
  arrange(desc(abs(mean_val))) %>% filter(n >= nreps*0.75)
