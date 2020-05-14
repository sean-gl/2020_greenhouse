require(ggplot2)
require(plyr)
require(lubridate)
require(readODS)
require(tidyr)
require(dplyr)
require(glmnet)
require(car)
require(leaps)

# rm(list = ls())

### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')
Sys.getenv('TZ') # make sure it got set

# read in all data
comb_all <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/data/combined_data/combdat_plant_level.rds')

# add date, minutes
comb_all$date <- date(comb_all$by15)
comb_all$minutes <- hour(comb_all$by15)*60 + minute(comb_all$by15)

# add irrigation amount (ml) given the previous night
comb_all$irrig <- NA
comb_all$irrig[comb_all$date <= "2019-10-30" & comb_all$treatment == 'well_watered'] <- 750
comb_all$irrig[comb_all$date >= "2019-10-31" & comb_all$treatment == 'well_watered'] <- 1000
comb_all$irrig[comb_all$treatment == 'moderate_drought'] <- 375
comb_all$irrig[comb_all$treatment %in% c('full_drought','virgin_drought')] <- 150
table(comb_all$irrig, useNA = 'a')

# add average of east/west PAR sensors
# comb_all$line_PAR_mean_umol_m2_s <- rowMeans(comb_all[,c('line_PAR_west_umol_m2_s','line_PAR_east_umol_m2_s')], na.rm = TRUE)


### First, subset data to rows with pressure bomb readings
comb <- subset(comb_all, !is.na(mean_psi_leaf_MPa))

#--- Explore linear correlations
nm <- names(comb)
nm <- nm[!nm%in%c('by15','date','block','treatment','plant_id','scale_flag','scale_weight_kg')]
corr <- sapply(nm, function(x) cor(comb[[x]], comb$mean_psi_leaf_MPa, use = 'complete.obs'))
ss <- sapply(nm, function(x) { # get sample sizes
  cc = complete.cases(comb[,c(x,'mean_psi_leaf_MPa')])
  length(cc[cc])
})
cormat <- data.frame(r=round(corr[order(corr, decreasing = T)],2),
                     n=ss[order(corr, decreasing = T)])
cormat


# Select variables to try in the Lasso model fitting below.
# NOTE: Limited selection to variables also available in Experiment 1.
df <- subset(comb, select = c(mean_psi_leaf_MPa, par1_n, pyr1_n, pyr2_s, am2320_high_temp, am2320_high_rh,
                              sht2_low_temp, sht2_low_rh, # omit sht1, since not in Experiment 1. (wasn't a good predictor anyhow)
                              VPD_air_high, VPD_air_low, 
                              windspeed_bottom, windspeed_middle, windspeed_top, soil_temp_C,
                              leaftemp_highest_avail, leaftemp_mean, VPD_leaf, 
                              irrig, irrig_cummean, irrig_cumsum, minutes
                              ))
df <- subset(df, complete.cases(df)); nrow(df)


# Function to fit a lasso model to a dataframe
fitLasso <- function(df) {
  
  # omit unwanted variables
  # df <- subset(df, select = -period)
  
  # add a couple random variables just to check for spurious correlation
  # df$random1 <- rnorm(nrow(df))
  # df$random2 <- rnorm(nrow(df))
  
  # define model matrix and response
  x <- model.matrix(mean_psi_leaf_MPa ~ ., df)[,-1] # omit intercept from model matrix
  y <- df$mean_psi_leaf_MPa
  
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


nreps <- 40
modelRuns <- do.call(rbind, lapply(1:nreps, function(x) fitLasso(df)))
mrSummary <- modelRuns %>% group_by(variable) %>% 
  summarise(coef_mean = mean(lasso_coef), coef_sd=sd(lasso_coef),
            mean_rmse=mean(rmse), mean_r2=mean(r2), n=n()) %>%
  arrange(desc(abs(coef_mean))) %>% filter(n >= nreps*0.75)

## Looks like these variables are consitently chosen by lasso (in order of importantc:
mrSummary

# try a linear model, adding variables in order of importance
summary(lm(mean_psi_leaf_MPa ~ irrig, df))
summary(lm(mean_psi_leaf_MPa ~ irrig + minutes, df))
summary(lm(mean_psi_leaf_MPa ~ irrig + minutes + sht2_low_temp, df))
summary(lm(mean_psi_leaf_MPa ~ irrig + minutes + sht2_low_temp + pyr2_s, df))

# Best Subsets Regression, using Lasso-selected variables
all.sub <- regsubsets(x = df[,mrSummary$variable], y = df$mean_psi_leaf_MPa, nvmax = length(mrSummary$variable))

rs <- summary(all.sub)
rs
rs$which
# This gives us the best subset of predictors for a given 
#  value of p (total number of parameters).
rs$rss

# BIC
rs$bic
which.min(rs$bic) # 6-predictor model has lowest BIC (but nearly as small w/only 3-4 parameters)


# Use this to compute -1*AIC's
p <- length(rs$rss) + 1
n <- length(df$mean_psi_leaf_MPa)
AIC <- n*log(rs$rss/n) + (2:p)*2
plot(AIC ~ I(2:p),ylab="AIC",xlab="Number of Parameters",
     pch=7)
# AIC minimized with all 7 predictors
round(AIC,1)
which.min(AIC)


# Consider the same exercise using adjusted R-squared
plot(2:p,rs$adjr2,xlab="Number of Parameters",
     ylab="Adjusted R-squared")
which.max(rs$adjr2)
round(rs$adjr2,2)
# Yields same model as min(AIC)

# Examine Cp: 
plot(2:p,rs$cp,xlab="Number of Parameters",ylab="Mallow's Cp")
# compare to line with slope of 1: 
abline(0,1)
# It looks like 7 parameters (4 predictors) is the smallest
# model with Cp close to or below p.  


# Fit model using all 7 parameters in Lasso best model
mrSummary$variable
lm_full <- lm(mean_psi_leaf_MPa ~ irrig + minutes + leaftemp_mean + sht2_low_temp + 
                sht2_low_rh + windspeed_bottom + windspeed_top,
              data = df)
summary(lm_full)

# Variance inflation factors: sht2_low_temp and leaftemp_mean are > 10,
# indicating these vars are highly colinear. I will omit sht2_low_temp since leaftemp_mean had a 
vif(lm_full)
lm_full <- lm(mean_psi_leaf_MPa ~ irrig + minutes + leaftemp_mean + 
                sht2_low_rh + windspeed_bottom + windspeed_top,
              data = df)
summary(lm_full)
vif(lm_full)
lm_full <- lm(mean_psi_leaf_MPa ~ irrig + minutes + sht2_low_temp + 
                sht2_low_rh + windspeed_bottom + windspeed_top,
              data = df)
summary(lm_full)
vif(lm_full)

rs$outmat

# 4-variable model (best subsets)
rs$which[4,][rs$which[4,]]
rs$which[5,][rs$which[5,]]

## Do best subsets again after removing sht2_low_temp
all.sub <- regsubsets(mean_psi_leaf_MPa ~ irrig + minutes + sht2_low_temp + 
                        sht2_low_rh + windspeed_bottom + windspeed_top, data = df, nvmax = 6)

rs <- summary(all.sub)
rs

# 4-variable model (best subsets)
rs$which[4,][rs$which[4,]]
lm4 <- lm(mean_psi_leaf_MPa ~ irrig + sht2_low_temp + minutes + sht2_low_rh, data=df)
summary(lm4); vif(lm4)

# predict and calculate RMSE
df$mean_psi_leaf_MPa_predicted <- predict(lm4)
resid <- df$mean_psi_leaf_MPa - df$mean_psi_leaf_MPa_predicted
sqrt(mean((resid)^2))

# predict on full data set
comb_all$mean_psi_leaf_MPa_modeled_new <- predict(lm4, newdata = comb_all)

# Plot predictions (lines) vs. pressure bomb readings (points)
sub = subset(comb_all, date >= '2019-11-10' & date <= '2019-11-26')
sub = subset(comb_all, date >= '2019-11-28' & date <= '2019-12-12')

ggplot(sub, aes(x=by15, color=treatment)) + geom_line(aes(y=mean_psi_leaf_MPa_modeled_new)) +
  geom_point(aes(y=mean_psi_leaf_MPa)) #+ facet_grid(~treatment)


# Save linear model to use for predictions
saveRDS(lm4, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_psi_leaf/psi_leaf_linear_model.rds')
