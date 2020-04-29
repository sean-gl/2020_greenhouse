
rm(list = ls())

packages <- c('lubridate','plyr','ggplot2')
lapply(packages, require, character.only = TRUE)
Sys.setenv(tz='GMT')


# ------ Section 1: Data Preparation

# read data (all variables plus tranpsiration and predicted leaf water potential)
dat  <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/combined_data/combdat_plant_level.rds')
names(dat)

# remove columns we don't need
dat <- select(dat, -c(T_mg_s, plant_id, scale_weight_kg, scale_flag))

# add hour and date columns
dat$hour <- hour(dat$by15)
dat$date <- date(dat$by15)

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



# ------ Combine/drop some variables that are highly collinear or redundant -----

# RH and VPD
cor(dat$sht2_low_rh, dat$VPD_air_low, use='complete.obs')
cor(dat$sht1_high_rh, dat$VPD_air_high, use='complete.obs')

# Air temp: Let's use just the SHT sensor temp (above canopy ca 1 m) and low (~70% canopy ht.)
# this is consistent with the RH and VPD (we used the SHT sensors)


# 
# # Examine the Transpiration data
# # trt 1
# subdat <- subset(dat, date >= '2019-10-24' & date <= '2019-11-04')
# # trt 2
# subdat <- subset(dat, date >= '2019-11-05' & date <= '2019-11-27')
# # trt 3
# subdat <- subset(dat, date >= '2019-11-28')
# 
# ggplot(subdat, aes(x=by15, y=T_mg_m2_s, color=block)) + 
#   geom_line()



# ------ Section 2: Modeling using Lasso for different time periods.


# First, split the data into daily time periods.


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
dat$period[dat$minutes >= 21*60 | dat$minutes <= 2*60] <- 'night (21-2)'
dat$period[dat$minutes >= 3*60 & dat$minutes <= 6*60+30] <- 'early_morning (3-6:30)'
dat$period[dat$minutes >= 7*60 & dat$minutes <= 10*60] <- 'mid_morning (7-10)'
dat$period[dat$minutes >= 11*60 & dat$minutes <= 14*60] <- 'midday (11-14)'
dat$period[dat$minutes >= 15*60 & dat$minutes <= 18*60] <- 'late_afternoon (15-18)'
dat$period <- as.factor(dat$period)
table(dat$period, useNA = 'always')


### --- Remove data before box fans turned on
dat <- subset(dat, date >= '2019-10-29')

# Remove columns we don't care about for Lasso model.

# names(dat)
# dat <- subset(dat, select = -c(date, by15, block, treatment, hour, minutes,
#                                leaftemp_bottom, leaftemp_middle, leaftemp_top, # many NA
#                                mean_psi_leaf_MPa, # this was only measured 60 times
#                                leaftemp_bottom, leaftemp_middle, leaftemp_top # many NA
# ))

# Remove above vars, PLUS redundant ones.
dat <- subset(dat, select = -c(date, by15, block, treatment, hour, minutes,
                               am2320_high_rh, am2320_high_temp, bmp_box_temp, # use SHT temp/rh
                               par1_n, pyr1_n, pyr2_s, # using the line quantum sensors instead
                               line_PAR_west_umol_m2_s, line_PAR_east_umol_m2_s, # using the mean
                               mean_psi_leaf_MPa, # this was only measured 60 times
                               leaftemp_bottom, leaftemp_middle, leaftemp_top, # many NA
                               leaftemp_mean, leaftemp_highest_avail, # used to model psi_leaf
                               sht1_high_rh, sht1_high_temp, sht2_low_temp, sht2_low_rh, # used for VPD calculation
                               bmp_box_atm_p
                               ))

# If modeling T_mg_m2_s, remove these variables also
dat <- subset(dat, select = -c(gs_leaf, gs_air))


# If modeling gs_air, remove these variables also
dat <- subset(dat, select = -c(gs_leaf, T_mg_m2_s, VPD_air_high, VPD_air_low, VPD_leaf))

### Examine the data, which columns contain the most NA?
### (We cannot have missing values for Lasso...unless we wish to impute them...?)
# naRows <- apply(dat, 2, function(x)  length(which(is.na(x))))
# naRows[order(naRows, decreasing = T)]

# Get complete cases for lasso
dat <- dat[complete.cases(dat), ]; nrow(dat)

# Split data into list according to time period
periodList <- split(dat, dat$period)
# sapply(periodList, nrow)


require(glmnet)

# testing
df = periodList$`midday (11-14)`
responseVar = 'gs_air'

  
# Function to fit a lasso model to a dataframe
fitLasso <- function(df) {
  
  # omit unwanted variables
  df <- subset(df, select = -period)
  
  # add a couple random variables just to check for spurious correlation
  df$random1 <- rnorm(nrow(df))
  df$random2 <- rnorm(nrow(df))
  
  # define model matrix and response
  x <- model.matrix(gs_air ~ ., df)
  y <- df$gs_air
  
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
  
  # plot(cvOut)
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
mrSummary <- mrSummary %>% filter(abs(coef_mean) > 1)

# summarize N
summary(mrSummary$n)

# add category variable
mrSummary$category <- 'other'
table(mrSummary$variable)
mrSummary$category[grepl('wind', mrSummary$variable)] <- 'wind'
mrSummary$category[grepl('temp', mrSummary$variable)] <- 'temp'

# define error bars 
limits <- aes(ymax = mrSummary$coef_mean + mrSummary$coef_sd, ymin = mrSummary$coef_mean - mrSummary$coef_sd)

require(ggsci); require(scales)

ggplot(mrSummary, aes(x = period, y = coef_mean, color = variable)) +
  geom_line(aes(group = variable)) +
  geom_point(aes(group = variable)) + geom_errorbar(limits, width = 0.25) +
  # scale_color_lancet() +
  theme_bw(base_size = 15) + 
  facet_grid(~category)







### Examine Wind data










### ----------- RANDOM FORESTS --------------------


fitRF <- function(df) {
  
  ### Split data into test/train sets
  propTrain <- 0.75
  
  # non-stratified sampling
  train <- sample(1:nrow(df), nrow(df)*propTrain, replace = F)
  
  # create split datasests
  df_train <- df[train,]; nrow(df_train)
  df_test <- df[-train,]; nrow(df_test)
  
  # fit a one-off random forest model (no tuning)
  rf_quick <- ranger(T_mg_m2_s ~ .,
                     data=df_train,
                     num.trees=500,
                     mtry=NULL,
                     importance = 'impurity')
  
  imp <- importance(rf_quick)[order(importance(rf_quick), decreasing = T)]
  imp <- imp / max(imp)
  out <- data.frame(variable = names(imp), imp = imp, r2 = rf_quick$r.squared)
  return(out)
  
}

nreps <- 20
# Apply RF function to all time periods in list
modelRuns <- lapply(periodList, function(df) do.call(rbind, lapply(1:nreps, function(x) fitRF(df))))
modelRuns_df <- do.call(rbind, modelRuns)
modelRuns_df$period <- substr(rownames(modelRuns_df), 1, regexpr('\\.', rownames(modelRuns_df))-1)
modelRuns_df$period <- factor(modelRuns_df$period, ordered = TRUE, 
                              levels = c('night (21-2)','early_morning (3-6:30)',
                                         'mid_morning (7-10)','midday (11-14)','late_afternoon (15-18)'))
table(modelRuns_df$period, useNA = 'a')  

# summarize coefficients by time period
mrSummary <- modelRuns_df %>% group_by(period, variable) %>% 
  summarise(imp_mean = mean(imp), imp_sd=sd(imp), mean_r2=mean(r2), n=n()) %>%
  arrange(desc(imp_mean)) 


require(ggsci); require(scales)


# define error bars 
limits <- aes(ymax = mrSummary$imp_mean + mrSummary$imp_sd, ymin = mrSummary$imp_mean - mrSummary$imp_sd)

ggplot(mrSummary, aes(x = period, y = imp_mean, color = variable)) +
  geom_line(aes(group = variable)) +
  geom_point(aes(group = variable)) + geom_errorbar(limits, width = 0.25) +
  # scale_color_lancet() +
  theme_bw(base_size = 15)  +
  ylim(c(0.5,1))









### Split data into test/train sets
propTrain <- 0.75

df <- periodList$`early_morning (3-6:30)`
df <- periodList$`midday (11-14)`
df
df$period <- NULL

# non-stratified sampling
train <- sample(1:nrow(df), nrow(df)*propTrain, replace = F)

# create split datasests
df_train <- df[train,]; nrow(df_train)
df_test <- df[-train,]; nrow(df_test)


# fit a one-off random forest model (no tuning)
require(ranger)
rf_quick <- ranger(T_mg_m2_s ~ .,
                   data=df_train,
                   num.trees=500,
                   mtry=NULL,
                   importance = 'impurity')

# treeInfo(Rf1)
print(rf_quick)

imp <- importance(rf_quick)[order(importance(rf_quick), decreasing = T)]
imp <- imp / max(imp)
par(mar=c(3,15,4,2))
barplot(imp, horiz = T, las=2)

### --- Tune the Random Forest Model --

# Set the tuning/training hyperparameters]
require(caret)
fitControl_randomForest <- trainControl(method='repeatedcv', 
                                        number=10, # either # folds (repeatedcv) or # of resampling iterations (not sure what method this is for?)
                                        repeats=1, # complete sets of folds to compute (repeatedcv method only)
                                        classProbs=F, # only for classification models
                                        verboseIter=T, # print training log
                                        search='grid', # grid or random
                                        savePredictions = 'final') # all, final or none

# (optional) create grid of tuning parameters for train()
sqrt(ncol(df_train)-1)
tuneGrid = expand.grid(mtry = 1:6, 
                       splitrule = c('variance', 'extratrees', 'maxstat'),
                       min.node.size = seq(1, 10, 2))


# train the model
randomForest_pl <- train(mean_T_mL_hr ~ .,
                         data=df_train,
                         metric='RMSE',
                         trControl=fitControl_randomForest,
                         # tuneLength=5, # use instead of tuneGrid (? I think)
                         method='ranger',
                         tuneGrid = tuneGrid)
print(randomForest_pl)
best <- randomForest_pl$bestTune
res <- randomForest_pl$results
res[res$RMSE == min(res$RMSE),]
res[res$Rsquared == max(res$Rsquared),]
















### ----- Old Code -------

modelRuns_early_morning <- lapply(1:nreps, function(x) fitLasso(df = periodList$early_morning))
modelRuns_early_morning <- do.call(rbind, modelRuns_early_morning)

modelRuns_early_morning %>% group_by(variable) %>% 
  summarise(mean_val = mean(coef), sd=sd(coef), n=n()) %>%
  arrange(desc(abs(mean_val))) #%>% filter(n >= nreps*0.75)

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
