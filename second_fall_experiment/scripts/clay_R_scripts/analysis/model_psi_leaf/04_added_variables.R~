require(ggplot2)
require(plyr)
require(lubridate)
require(readODS)
require(tidyr)
require(dplyr)
require(ranger)
require(caret)

### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')
Sys.getenv('TZ') # make sure it got set

### load the combined data
comb_all <- readRDS('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/combined_data/pressure_bomb_combined_data.rds')




# random forest indicates these variables are importnat:
# irrig, leaftemp_highest, VPD_leaf, treatment, leaftemp_mean
# less important: minutes, VPD_air, bmp_box_temp, sht2_low_temp, sht2_low_rh


# calculate air-leaf temp diffs
comb_all$altd <- comb_all$temp_high_mean - comb_all$leaftemp_mean
plot(comb_all$altd, comb_all$mean_psi_MPa, col=comb_all$treatment)
cor(comb_all$altd, comb_all$mean_psi_MPa, use='complete.obs')

#for testing below
x = subset(comb_all, date=='2019-11-06' & block == 'D')

# mean altd daily
mean_altd <- ddply(comb_all, .(date, block), function(x) {
  # uncomment to calculate for previous day
  # x = subset(comb_all, date == (unique(x$date)-1) & block == unique(x$block))
  y = subset(x, hour(by15) >= 9 & hour(by15) <= 14)
  setNames(mean(y$altd, na.rm=T) , 'mean_altd')
})
ggplot(mean_altd, aes(x=date, y=mean_altd, color=block)) + geom_line() +
  scale_x_date(breaks='1 week', date_labels = '%m-%d')

# now add this data back to comb_all
for(i in 1:nrow(comb_all)) {
  d=comb_all$date[i]; b=comb_all$block[i]
  a=mean_altd$mean_altd[mean_altd$date==d & mean_altd$block==b]
  comb_all[i,'mean_altd'] <- a
}



# ---- mean VPD daily
mean_vpd_leaf <- ddply(comb_all, .(date, block), function(x) {
  # uncomment to calculate for previous day
  # x = subset(comb_all, date == (unique(x$date)-1) & block == unique(x$block))
  y = subset(x, hour(by15) >= 9 & hour(by15) <= 14)
  setNames(mean(y$VPD_air, na.rm=T) , 'mean_vpd_leaf')
})
ggplot(mean_vpd_leaf, aes(x=date, y=mean_vpd_leaf, color=block)) + geom_line() +
  scale_x_date(breaks='1 week', date_labels = '%m-%d')

# now add this data back to comb_all
for(i in 1:nrow(comb_all)) {
  d=comb_all$date[i]; b=comb_all$block[i]
  a=mean_vpd_leaf$mean_vpd_leaf[mean_vpd_leaf$date==d & mean_vpd_leaf$block==b]
  comb_all[i,'mean_vpd_leaf'] <- a
}


# --- mean PAR for CURRENT hour .... DOESNT SEEM USEFUL.
#for testing below
x = subset(comb_all, date=='2019-11-06' & hour == 11  & block == 'D')

# comb_all$hour <- hour(comb_all$by15)

# probably should do this in more sophisticted way (use hour immediately previous)
# mean_par <- ddply(comb_all, .(date, hour, block), function(x) {
#   setNames(mean(x$par1_n, na.rm=T) , 'mean_par')
# })
# ggplot(mean_par, aes(x=date, y=mean_par)) + geom_line() +
#   scale_x_date(breaks='1 week', date_labels = '%m-%d')
# # now add this data back to comb_all
# for(i in 1:nrow(comb_all)) {
#   d=comb_all$date[i]; h=comb_all$hour[i]; b=comb_all$block[i]
#   a=mean_par$mean_par[mean_par$date==d & mean_par$hour==h & mean_par$block==b]
#   comb_all[i,'mean_par'] <- a
# }


### First, subset data to rows with pressure bomb readings
comb <- subset(comb_all, !is.na(mean_psi_MPa))

### 4.1 Linear Regression (no cross-validation; trial and error)

### CURRENTLY THE BEST R2 (but likely overfit...too many porameters)
m <- lm(mean_psi_MPa ~ minutes + treatment + block + daysPostTrt +
          windspeed_middle + bmp_box_temp + soil_temp_C, data = comb); summary(m)

m <- lm(mean_psi_MPa ~ minutes + mean_vpd_leaf + leaftemp_mean + windspeed_top +
          mean_altd, data = comb); summary(m)
sqrt(mean(m$residuals^2))
mean(abs(m$residuals))

m <- lm(mean_psi_MPa ~ minutes + altd + daysPostTrt +
          + bmp_box_temp , data = df2); summary(m)
cor(comb$bmp_box_temp, comb$mean_psi_MPa)
cor(comb$sht2_low_temp, comb$mean_psi_MPa)
cor(comb$sht2_low_rh, comb$mean_psi_MPa)

cor(comb$sht1_high_rh, comb$mean_psi_MPa)

m <- lm(mean_psi_MPa ~ minutes + irrig + mean_altd + bmp_box_temp , data = comb); summary(m)
m <- lm(mean_psi_MPa ~ minutes + irrig + mean_altd + sht2_low_temp, data = comb); summary(m)
m <- lm(mean_psi_MPa ~ minutes + VPD_leaf + mean_altd + sht2_low_temp, data = comb); summary(m)

mean(m$residuals^2)
mean(abs(m$residuals))

m <- lm(mean_psi_MPa ~ minutes + daysPostTrt +
          + bmp_box_temp + mean_altd, data = comb); summary(m)
mean(m$residuals^2)
mean(abs(m$residuals))

plot(comb$altd, comb$mean_psi_MPa)


# approach 4: Use only non-experiment specific varaibles for physical model 
# also exclude leaf temp to avoid autoregression and box_temp since that's not replicable.
df2 <- subset(comb, select = -c(by15, date, leaftemp_bottom, leaftemp_middle,leaftemp_top,
                                leaftemp_highest_avail, leaftemp_mean, VPD_leaf,
                                bmp_box_temp, block, treatment, irrig,
                                mean_altd, altd, mean_vpd_leaf, daysPostTrt))
df2 <- subset(comb, select = -c(by15, date, leaftemp_bottom, leaftemp_middle,
                                bmp_box_temp, block, treatment, irrig))
df2 <- subset(df2, complete.cases(df2)); nrow(df2)

# fit a one-off random forest model (no tuning)
rf_quick <- ranger(mean_psi_MPa~ .,
                   data=df2,
                   num.trees=5000,
                   mtry=2,
                   importance = 'impurity')

# treeInfo(Rf1)
print(rf_quick)
importance(rf_quick)[order(importance(rf_quick), decreasing = T)]


## TUNE A FOREST =====

# Set the tuning/training hyperparameters
fitControl_randomForest <- trainControl(method='repeatedcv', 
                                        number=5, # either # folds (repeatedcv) or # of resampling iterations (not sure what method this is for?)
                                        repeats=3, # complete sets of folds to compute (repeatedcv method only)
                                        classProbs=F, # only for classification models
                                        verboseIter=F, # print training log
                                        search='grid', # grid or random
                                        savePredictions = 'final') # all, final or none

# (optional) create grid of tuning parameters for train()
sqrt(ncol(df2)-1)
tuneGrid = expand.grid(mtry = seq(1, 10, 2), 
                       splitrule = c('variance', 'extratrees', 'maxstat'),
                       min.node.size = seq(1, 10, 2))


# train the model
randomForest_pl <- train(mean_psi_MPa ~ .,
                         data=df2,
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


### ---------- LASSO -------------

# create model matrix for predictor variables
x <- model.matrix(mean_psi_MPa ~ ., df2)[,-1]
# create vector for response variable
y <- df2$mean_psi_MPa


### REPEAT THE CROSS-VALIDATION N TIMES, TO SEE WHICH VARIABLES ARE CONSISTENTLY IMPORTNAT
# list to store variables
nreps <- 50
nzcList <- list()
require(glmnet)
for(i in 1:nreps) {
  
  # CV using full dataset
  lasso.cv <- cv.glmnet(x, y, family='gaussian', alpha=1, nfolds=10, standardize=T)
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
x=sapply(b, function(a) length(z[z == a]))
x[x>(nreps/2)]

# METHOD 4 Physical variables that seem important:
# Always: daysPostTrt, minutes, leaftemp_mean, mean_vpd_leaf, mean_altd, windspeed_middle, windspeed_top
# Over half: sht2_low_rh, altd

# picking variables based on simulation
lm1 <- lm(mean_psi_MPa ~ minutes + leaftemp_mean + mean_vpd_leaf + mean_altd + 
            windspeed_middle + windspeed_top, data=df2); summary(lm1)

lm1 <- lm(mean_psi_MPa ~ minutes + leaftemp_mean + mean_vpd_leaf + mean_altd + windspeed_top, data=df2); summary(lm1)


lm1 <- lm(mean_psi_MPa ~ minutes + sht2_low_temp + VPD_air + windspeed_top, data=df2); summary(lm1)

