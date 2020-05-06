
rm(list = ls())

packages <- c('lubridate','plyr','ggplot2','tidyr','dplyr','glmnet','ggsci')
lapply(packages, require, character.only = TRUE)
Sys.setenv(tz='GMT')


# ------ Section 1: Data Preparation

# read data (all variables plus tranpsiration and predicted leaf water potential)
dat  <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/data/combined_data/combdat_plant_level.rds')
names(dat)

# remove columns we don't need
dat <- select(dat, -c(T_mg_s, plant_id, scale_weight_kg, scale_flag))

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



# ------ Combine/drop some variables that are highly collinear or redundant -----


# mean windspeed
dat$windspeed_mean <- rowMeans(dat[,c('windspeed_bottom','windspeed_middle','windspeed_top')], na.rm = TRUE)


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
# dat <- subset(dat, select = -c(date, by15, block, treatment, minutes,
#                                leaftemp_bottom, leaftemp_middle, leaftemp_top, # many NA
#                                mean_psi_leaf_MPa, # this was only measured 60 times
#                                leaftemp_bottom, leaftemp_middle, leaftemp_top # many NA
# ))

# Remove above vars, PLUS redundant ones.
# dat <- subset(dat, select = -c(date, by15, block, minutes, treatment,
#                                am2320_high_rh, am2320_high_temp, bmp_box_temp, # use SHT temp/rh
#                                par1_n, pyr1_n, pyr2_s, # using the line quantum sensors instead
#                                line_PAR_west_umol_m2_s, line_PAR_east_umol_m2_s, # using the mean
#                                mean_psi_leaf_MPa, # this was only measured 60 times
#                                leaftemp_bottom, leaftemp_middle, leaftemp_top, # many NA
#                                leaftemp_highest_avail, # used to model psi_leaf
#                                sht1_high_rh, sht1_high_temp, sht2_low_temp, sht2_low_rh, # used for VPD calculation
#                                bmp_box_atm_p
#                                # VPD_leaf, irrig, day, mean_psi_leaf_MPa_modeled, soil_temp_C
#                                
#                                ))

dat <- subset(dat, select = c(treatment, T_mg_m2_s, period, pressure_gradient_soil_leaf_MPa, VPD_air_high, VPD_air_low,
                              leaftemp_mean, soil_water_potential_kPa, windspeed_mean, line_PAR_mean_umol_m2_s, day))

# If modeling T_mg_m2_s, remove these variables also
dat <- subset(dat, select = -c(gs_leaf, gs_air))


# If modeling gs_air, remove these variables also
# dat <- subset(dat, select = -c(gs_leaf, T_mg_m2_s, VPD_air_high, VPD_air_low, VPD_leaf))

### Examine the data, which columns contain the most NA?
### (We cannot have missing values for Lasso...unless we wish to impute them...?)
# naRows <- apply(dat, 2, function(x)  length(which(is.na(x))))
# naRows[order(naRows, decreasing = T)]


# Get complete cases for lasso
dat <- dat[complete.cases(dat), ]; nrow(dat)


# Split data into list according to time period
periodList <- split(select(dat, -treatment), dat$period)
# sapply(periodList, nrow)


# testing
df = periodList$`night (21-2)`
# df = df[df$treatment=='well_watered',]
# df$treatment <- NULL

# Function to fit a lasso model to a dataframe
fitLasso <- function(df) {
  
  # omit unwanted variables
  df <- subset(df, select = -period)
  
  # add a couple random variables just to check for spurious correlation
  # df$random1 <- rnorm(nrow(df))
  # df$random2 <- rnorm(nrow(df))
  
  # define model matrix and response
  x <- model.matrix(T_mg_m2_s ~ ., df)[,-1] # omit intercept from model matrix
  y <- df$T_mg_m2_s
  
  # scale x, y
  x <- scale(x)
  y <- scale(y)
  
  # split into train/test sets
  propTrain <- 0.75
  train <- sample(1:nrow(df), nrow(df)*propTrain)
  test <- -(train)
  
  # create grid of lambda values to use in lasso 
  lambdaGrid <- 10^seq(10, -2, length = 100)
  
  # fit a lasso model
  lassoModel <- glmnet(x[train, ], y[train], alpha = 1, lambda = lambdaGrid,
                       standardize = F, family = 'gaussian')
  
  # perform cross-validation to select best lambda value
  cvOut <- cv.glmnet(x=x[train, ], y=y[train], alpha = 1, nfolds = 10, 
                     standardize = F, family = 'gaussian') 
  
  # # Try using full data set, is it different result?
  # cvOut <- cv.glmnet(x=x, y=y, alpha = 1, nfolds = 10, 
  #                    standardize = F, family = 'gaussian') 
  
  # plot(cvOut)
  bestLambda <- cvOut$lambda.1se
  # log(bestLambda)
  
  # get test RMSE & R^2
  lassoPred <- predict(cvOut, s = bestLambda, newx = x[test,])
  lassoPredUnscaled <- lassoPred * sd(y) + mean(y)
  testValuesUnscaled <- y[test] * sd(y) + mean(y)
  resid <- lassoPredUnscaled - testValuesUnscaled
  rmse <- sqrt(mean(resid^2)) # RMSE
  # plot(testValuesUnscaled, lassoPredUnscaled); abline(c(0,1))
  
  # mean(abs(resid)) # MAE
  # mean(resid) # MBE
  
  # fit lasso model to full data set
  lassoFull <- glmnet(x, y, alpha = 1, lambda = lambdaGrid, standardize = F)
  
  # get coefficients
  lc <- predict(lassoFull, type = 'coefficients', s = bestLambda)
  lc <- as.matrix(lc)
  lc <- data.frame(variable = dimnames(lc)[[1]], lasso_coef = as.numeric(lc), stringsAsFactors = F)
  lc <- lc[lc$lasso_coef != 0 & lc$variable != '(Intercept)', ]
  lc <- lc[order(abs(lc$lasso_coef), decreasing = T), ]
  nrow(lc)
  
  # if any coefficients are smaller than a random variable's coefficient, drop them.
  # last <- grep('random', lc$variable)
  # if(length(last) > 0) {
  #   lc <- lc[1:(last[1]-1),]
  # }
  
  # fit a linear model using the selected non-zero coefficients, to get the R2
  # also report the test RMSE

  if(nrow(lc)==0) { # if all lasso coef are zero, create empty df for return
    lc <- data.frame(variable=NA, lasso_coef=NA, lm_coef=NA, lm_coef_se=NA, r2=NA, rmse=NA)
  } else {
    ms <- summary(lm(y[test] ~ x[test, c(lc$variable)]))
    lc$lm_coef <- as.numeric(ms$coefficients[-1,1]) # omit intercept
    lc$lm_coef_se <- as.numeric(ms$coefficients[-1,2]) # omit intercept
    lc$r2 <- ms$adj.r.squared
    lc$rmse <- rmse
  }
    
  return(lc)
}


nreps <- 20

# Apply Lasso function to all time periods in list
modelRuns <- lapply(names(periodList), function(period) {
  out <- lapply(1:nreps, function(x) {
    print(period)
    fitLasso(df = periodList[[period]])
  })
  df <- do.call(rbind, out)
  df$period <- period
  return(df)
})
modelRuns_df <- do.call(rbind, modelRuns)
modelRuns_df$period <- factor(modelRuns_df$period, ordered = TRUE, 
                              levels = c('night (21-2)','early_morning (3-6:30)',
                                         'mid_morning (7-10)','midday (11-14)','late_afternoon (15-18)'))
table(modelRuns_df$period, useNA = 'a')  

# summarize coefficients by time period
mrSummary <- modelRuns_df %>% group_by(period, variable) %>% 
  summarise(coef_mean = mean(lasso_coef), coef_sd=sd(lasso_coef),
            mean_rmse=mean(rmse), mean_r2=mean(r2), n=n()) %>%
  arrange(desc(abs(coef_mean))) %>% filter(n >= nreps*0.75)


# remove night, r2 is crap
mrSummary <- mrSummary[mrSummary$period != 'night (21-2)',]

# summarize N
summary(mrSummary$n)

# define error bars 
limits <- aes(ymax = mrSummary$coef_mean + mrSummary$coef_sd, ymin = mrSummary$coef_mean - mrSummary$coef_sd)

png('/home/sean/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/tranpsiration_lasso_model/lasso_alldata.png',
    width = 1500, height = 900)
ggplot(mrSummary, aes(x = period, y = coef_mean, color = variable)) +
  geom_line(aes(group = variable), size=1.5) +
  geom_point(aes(group = variable), size=2.5) + geom_errorbar(limits, width = 0.25) +
  scale_color_npg() +
  theme_bw(base_size = 20) +
  # facet_grid(~category) +
  ylim(c(-0.6, 0.6)) +
  theme(legend.title=element_blank(),
        legend.margin=margin(rep(0,4))) 
  # theme(axis.text.x = element_text(angle = 45))
dev.off()

unique(mrSummary[,c('period', 'mean_r2')])



### ---- Now, repeat above but for well_watered vs. full_drought treatments


### --- Well Watered ----

# Split data into list according to time period
dat_ww <- dat %>% filter(treatment=='well_watered') %>% select(-treatment)
periodList_ww <- split(dat_ww, dat_ww$period)

# Apply Lasso function to all time periods in list
modelRuns_ww <- lapply(names(periodList_WW), function(period) {
  out <- lapply(1:nreps, function(x) {
    print(period)
    fitLasso(df = periodList_ww[[period]])
  })
  df <- do.call(rbind, out)
  df$period <- period
  return(df)
})
modelRuns_ww_df <- do.call(rbind, modelRuns_ww)
modelRuns_ww_df$period <- factor(modelRuns_ww_df$period, ordered = TRUE, 
                              levels = c('night (21-2)','early_morning (3-6:30)',
                                         'mid_morning (7-10)','midday (11-14)','late_afternoon (15-18)'))
table(modelRuns_ww_df$period, useNA = 'a')  

# summarize coefficients by time period
mrSummary_ww <- modelRuns_ww_df %>% group_by(period, variable) %>% 
  summarise(coef_mean = mean(lasso_coef), coef_sd=sd(lasso_coef),
            mean_rmse=mean(rmse), mean_r2=mean(r2), n=n()) %>%
  arrange(desc(abs(coef_mean))) %>% filter(n >= nreps*0.75)


# remove night, r2 is crap
mrSummary_ww <- mrSummary_ww[mrSummary_ww$period != 'night (21-2)',]

# summarize N
summary(mrSummary_ww$n)

# define error bars 
limits <- aes(ymax = mrSummary_ww$coef_mean + mrSummary_ww$coef_sd, ymin = mrSummary_ww$coef_mean - mrSummary_ww$coef_sd)

png('/home/sean/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/tranpsiration_lasso_model/lasso_well_watered.png',
    width = 1500, height = 900)
ggplot(mrSummary_ww, aes(x = period, y = coef_mean, color = variable)) +
  geom_line(aes(group = variable), size=1.5) +
  geom_point(aes(group = variable), size=2.5) + geom_errorbar(limits, width = 0.25) +
  scale_color_npg() +
  theme_bw(base_size = 20) +
  # facet_grid(~category) +
  ylim(c(-0.6, 0.6)) +
  theme(legend.title=element_blank(),
        legend.margin=margin(rep(0,4))) 
# theme(axis.text.x = element_text(angle = 45))
dev.off()

unique(mrSummary_ww[,c('period', 'mean_r2')])


### --- Full Drought ----

# Split data into list according to time period
dat_fd <- dat %>% filter(treatment=='full_drought') %>% select(-treatment)
periodList_fd <- split(dat_fd, dat_fd$period)

# Apply Lasso function to all time periods in list
modelRuns_fd <- lapply(names(periodList_fd), function(period) {
  out <- lapply(1:nreps, function(x) {
    print(period)
    fitLasso(df = periodList_fd[[period]])
  })
  df <- do.call(rbind, out)
  df$period <- period
  return(df)
})
modelRuns_fd_df <- do.call(rbind, modelRuns_fd)
modelRuns_fd_df$period <- factor(modelRuns_fd_df$period, ordered = TRUE, 
                              levels = c('night (21-2)','early_morning (3-6:30)',
                                         'mid_morning (7-10)','midday (11-14)','late_afternoon (15-18)'))
table(modelRuns_fd_df$period, useNA = 'a')  

# summarize coefficients by time period
mrSummary_fd <- modelRuns_fd_df %>% group_by(period, variable) %>% 
  summarise(coef_mean = mean(lasso_coef), coef_sd=sd(lasso_coef),
            mean_rmse=mean(rmse), mean_r2=mean(r2), n=n()) %>%
  arrange(desc(abs(coef_mean))) %>% filter(n >= nreps*0.75)


# remove night, r2 is crap
mrSummary_fd <- mrSummary_fd[mrSummary_fd$period != 'night (21-2)',]

# summarize N
summary(mrSummary_fd$n)

# define error bars 
limits <- aes(ymax = mrSummary_fd$coef_mean + mrSummary_fd$coef_sd, ymin = mrSummary_fd$coef_mean - mrSummary_fd$coef_sd)

png('/home/sean/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/tranpsiration_lasso_model/lasso_full_drought.png',
    width = 1500, height = 900)
ggplot(mrSummary_fd, aes(x = period, y = coef_mean, color = variable)) +
  geom_line(aes(group = variable), size=1.5) +
  geom_point(aes(group = variable), size=2.5) + geom_errorbar(limits, width = 0.25) +
  scale_color_npg() +
  theme_bw(base_size = 20) +
  # facet_grid(~category) +
  ylim(c(-0.6, 0.6)) +
  theme(legend.title=element_blank(),
        legend.margin=margin(rep(0,4))) 
# theme(axis.text.x = element_text(angle = 45))
dev.off()

unique(mrSummary_fd[,c('period', 'mean_r2')])



## --- Combine summaries of ww, fd

mrSummary_ww$treatment <- 'well_watered'
mrSummary_fd$treatment <- 'full_drought'
mrSummary_wwfd <- rbind(mrSummary_ww, mrSummary_fd)

# define error bars 
limits <- aes(ymax = mrSummary_wwfd$coef_mean + mrSummary_wwfd$coef_sd, ymin = mrSummary_wwfd$coef_mean - mrSummary_wwfd$coef_sd)

png('/home/sean/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/tranpsiration_lasso_model/lasso_compare_treatments.png',
    width = 1800, height = 1100)
ggplot(mrSummary_wwfd, aes(x = period, y = coef_mean, color = variable)) +
  geom_line(aes(group = variable), size=1.5) +
  geom_point(aes(group = variable), size=2.5) + geom_errorbar(limits, width = 0.25) +
  scale_color_lancet() +
  theme_bw(base_size = 20) +
  facet_grid(~treatment) +
  ylim(c(-0.7, 0.7)) +
  theme(legend.title=element_blank(),
        legend.margin=margin(rep(0,4))) 
# theme(axis.text.x = element_text(angle = 45))
dev.off()



### ----------- RANDOM FORESTS --------------------

require(ranger)

df = periodList$`midday (11-14)`

fitRF <- function(df) {
  
  # omit unwanted variables
  df <- subset(df, select = -period)
  
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

nreps <- 5
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
