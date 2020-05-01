
rm(list = ls())

packages <- c('lubridate','plyr','ggplot2','tidyr', 'dplyr','glmnet')
lapply(packages, require, character.only = TRUE)
Sys.setenv(tz='GMT')


# ------ Section 1: Data Preparation

# read data (all variables plus tranpsiration and predicted leaf water potential)
dat  <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/combined_data/combdat_plant_level.rds')
names(dat)

# remove columns we don't need
dat <- select(dat, -c(T_mg_s, scale_weight_kg, scale_flag))


# add date, minutes columns
dat$date <- date(dat$by15)
dat$minutes <- hour(dat$by15)*60 + minute(dat$by15)

# Transpiration >> change negative values of T to zero
ind <- dat$T_mg_m2_s < 0 & !is.na(dat$T_mg_m2_s)
nrow(dat[ind, ]) / nrow(dat) # about 5% of data are < 0
dat$T_mg_m2_s[ind] <- 0

# Calculate stomatal conductance
dat$gs_air <- dat$T_mg_m2_s / dat$VPD_air_low
dat$gs_leaf <- dat$T_mg_m2_s / dat$VPD_leaf


# Psi_leaf >> change negative values to zero
ind <- dat$mean_psi_leaf_MPa_modeled < 0 & !is.na(dat$mean_psi_leaf_MPa_modeled)
nrow(dat[ind, ]) / nrow(dat) # about 5% of data are < 0
dat$mean_psi_leaf_MPa_modeled[ind] <- 0

# Psi_leaf >> reverse the sign to make positive (so matches psi_leaf)
dat$soil_water_potential_kPa <- -(dat$soil_water_potential_kPa)
# no negative values, good!
ind <- dat$soil_water_potential_kPa < 0 & !is.na(dat$soil_water_potential_kPa)
any(ind)

# Calculate pressure gradient between leaf and soil
dat$pressure_gradient_soil_leaf_MPa <- dat$mean_psi_leaf_MPa_modeled - dat$soil_water_potential_kPa/1000
summary(dat$pressure_gradient_soil_leaf_MPa)


# Create backup of master dataframe
dat_backup <- dat

# Add "day" column (days since sart of experminet, 10-24)
dat$day <- dat$date - min(dat$date) + 1



### --- Remove data before box fans turned on
dat <- subset(dat, date >= '2019-10-29')

# Remove above vars, PLUS redundant ones.
dat <- subset(dat, select = -c(date,  
                               am2320_high_rh, am2320_high_temp, bmp_box_temp, # use SHT temp/rh
                               par1_n, pyr1_n, pyr2_s, # using the line quantum sensors instead
                               line_PAR_west_umol_m2_s, line_PAR_east_umol_m2_s, # using the mean
                               mean_psi_leaf_MPa, # this was only measured 60 times
                               leaftemp_bottom, leaftemp_middle, leaftemp_top, # many NA
                               leaftemp_mean, leaftemp_highest_avail, # used to model psi_leaf
                               sht1_high_rh, sht1_high_temp, sht2_low_temp, sht2_low_rh, # used for VPD calculation
                               bmp_box_atm_p, irrig, day
))

# If modeling T_mg_m2_s, remove these variables also
dat <- subset(dat, select = -c(gs_leaf, gs_air))


# ---- Calculate 1st-order diffs of all variables, by plant_id

# testing
# table(dat$plant_id, useNA = 'a')
# x = subset(dat, plant_id == 'D-10'); names(x)
# summary(x)
# ignoreVars = c('plant_id', 'minutes','by15')

dat2 <- ddply(dat, .(plant_id), function(x, ignoreVars= c('plant_id', 'minutes','by15')) {
  x <- x %>% arrange(by15) 
  xMat <- x %>% select(-ignoreVars) # omit variables we don't need diffs of
  xDiff <- apply(xMat, 2, function(y) c(diff(y), NA)) # calculate diff matrix
  xDiff <- cbind(x[,ignoreVars], xDiff) # add back the "key" variables
  xDiff$timediff <- c(diff(as.numeric(xDiff$by15)), NA) / 60 # convert to minutes
  # If there are any gaps in the data (not 15 diffs), set these to NA
  ind <- xDiff$timediff != 15 & !is.na(xDiff$timediff) 
  xDiff[ind, !names(xDiff) %in% c(ignoreVars, 'timediff')] <- NA
  return(xDiff)
})

# filter out data without 15-minute time steps
dat3 <- subset(dat2, timediff == 15)

dat_backup <- dat
dat <- dat3

# Use "minutes" column to split data into list according to time periods:
dat$period <- NA
dat$period[dat$minutes >= 21*60 | dat$minutes <= 2*60] <- 'night (21-2)'
dat$period[dat$minutes >= 3*60 & dat$minutes <= 6*60+30] <- 'early_morning (3-6:30)'
dat$period[dat$minutes >= 7*60 & dat$minutes <= 10*60] <- 'mid_morning (7-10)'
dat$period[dat$minutes >= 11*60 & dat$minutes <= 14*60] <- 'midday (11-14)'
dat$period[dat$minutes >= 15*60 & dat$minutes <= 18*60] <- 'late_afternoon (15-18)'
dat$period <- as.factor(dat$period)
table(dat$period, useNA = 'always')


# Examine data
plot(density(dat$T_mg_m2_s, na.rm = T))
plot(density(dat$T_mg_m2_s[dat$period=='midday (11-14)'], na.rm = T))
plot(density(dat$T_mg_m2_s[dat$period=='night (21-2)'], na.rm = T))


# remove some vars
dat <- select(dat, -c(plant_id, minutes, by15, timediff))

# Get complete cases for lasso
dat <- dat[complete.cases(dat), ]; nrow(dat)

# Split data into list according to time period
periodList <- split(dat, dat$period)
# sapply(periodList, nrow)


# testing
df = periodList$`midday (11-14)`


# Function to fit a lasso model to a dataframe
fitLasso <- function(df) {
  
  # omit unwanted variables
  df <- subset(df, select = -period)
  
  # add a couple random variables just to check for spurious correlation
  df$random1 <- rnorm(nrow(df))
  df$random2 <- rnorm(nrow(df))
  
  # define model matrix and response
  x <- model.matrix(T_mg_m2_s ~ ., df)
  y <- df$T_mg_m2_s
  
  # split into train/test sets
  propTrain <- 0.75
  train <- sample(1:nrow(df), nrow(df)*propTrain)
  test <- -(train)
  
  # create grid of lambda values to use in lasso 
  lambdaGrid <- 10^seq(10, -2, length = 100)
  
  # fit a lasso model
  lassoModel <- glmnet(x[train, ], y[train], alpha = 1, lambda = lambdaGrid,
                       standardize = T, family = 'gaussian')
  
  # perform cross-validation to select best lambda value
  cvOut <- cv.glmnet(x=x[train, ], y=y[train], alpha = 1, nfolds = 10, 
                     standardize = T, family = 'gaussian') 
  
  # # Try using full data set, is it different result?
  # cvOut <- cv.glmnet(x=x, y=y, alpha = 1, nfolds = 10, 
  #                    standardize = T, family = 'gaussian') 
  
  plot(cvOut)
  bestLambda <- cvOut$lambda.1se
  # log(bestLambda)
  
  # get test RMSE & R^2
  lassoPred <- predict(cvOut, s = bestLambda, newx = x[test,])
  resid <- lassoPred - y[test]
  rmse <- sqrt(mean(resid^2)) # RMSE
  # mean(abs(resid)) # MAE
  # mean(resid) # MBE
  
  # fit lasso model to full data set
  lassoFull <- glmnet(x, y, alpha = 1, lambda = lambdaGrid, standardize = T)
  
  # get coefficients
  lc <- predict(lassoFull, type = 'coefficients', s = bestLambda)
  lc <- as.matrix(lc)
  lc <- data.frame(variable = dimnames(lc)[[1]], coef = round(as.numeric(lc), 2), stringsAsFactors = F)
  lc <- lc[lc$coef != 0 & lc$variable != '(Intercept)', ]
  lc <- lc[order(abs(lc$coef), decreasing = T), ]
  
  # if any coefficients are smaller than a random variable's coefficient, drop them.
  last <- grep('random', lc$variable)
  if(length(last) > 0) {
    lc <- lc[1:(last[1]-1),]
  }
  
  # fit a linear model using the selected non-zero coefficients, to get the R2
  # also report the test RMSE
  ms <- summary(lm(y[test] ~ x[test, c(lc$variable)]))
  lc$r2 <- ms$adj.r.squared
  lc$rmse <- rmse
  return(lc)
}


require(tidyr); require(dplyr)
nreps <- 20

# Apply Lasso function to all time periods in list
modelRuns <- lapply(periodList, function(df) do.call(rbind, lapply(1:nreps, function(x) fitLasso(df))))
modelRuns_df <- do.call(rbind, modelRuns)
modelRuns_df$period <- substr(rownames(modelRuns_df), 1, regexpr('\\.', rownames(modelRuns_df))-1)
modelRuns_df$period <- factor(modelRuns_df$period, ordered = TRUE, 
                              levels = c('night (21-2)','early_morning (3-6:30)',
                                         'mid_morning (7-10)','midday (11-14)','late_afternoon (15-18)'))
table(modelRuns_df$period, useNA = 'a')  

# summarize coefficients by time period
mrSummary <- modelRuns_df %>% group_by(period, variable) %>% 
  summarise(coef_mean = mean(coef), coef_sd=sd(coef), mean_rmse=mean(rmse), mean_r2=mean(r2), n=n()) %>%
  arrange(desc(abs(coef_mean))) %>% filter(n >= nreps*0.75)

# remove coefficients with very small values
# mrSummary <- mrSummary %>% filter(abs(coef_mean) > 1)

# summarize N
summary(mrSummary$n)

# add category variable
# mrSummary$category <- 'other'
# table(mrSummary$variable)
# mrSummary$category[grepl('wind', mrSummary$variable)] <- 'wind'
# mrSummary$category[grepl('temp', mrSummary$variable)] <- 'temp'

# define error bars 
limits <- aes(ymax = mrSummary$coef_mean + mrSummary$coef_sd, ymin = mrSummary$coef_mean - mrSummary$coef_sd)

require(ggsci); require(scales)

ggplot(mrSummary, aes(x = period, y = coef_mean, color = variable)) +
  geom_line(aes(group = variable)) +
  geom_point(aes(group = variable)) + geom_errorbar(limits, width = 0.25) +
  # scale_color_lancet() +
  theme_bw(base_size = 15) + 
  facet_grid(~category)

