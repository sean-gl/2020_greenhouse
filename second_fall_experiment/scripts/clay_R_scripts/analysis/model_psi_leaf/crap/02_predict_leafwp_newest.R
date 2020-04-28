require(ggplot2)
require(plyr)
require(lubridate)
require(readODS)
require(tidyr)
require(dplyr)

rm(list = ls())

### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')
Sys.getenv('TZ') # make sure it got set

### load the combined data
comb_all <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/combined_data/pressure_bomb_combined_data.rds')



### SECTION 4. Model fitting ---------------------


### First, subset data to rows with pressure bomb readings
comb <- subset(comb_all, !is.na(mean_psi_MPa))

#--- Explore linear correlations
nm <- names(comb)
nm <- nm[!nm%in%c('by15','date','block','treatment','mean_psi_MPa')]
corr <- sapply(nm, function(x) cor(comb[[x]], comb$mean_psi_MPa, use = 'complete.obs'))
ss <- sapply(nm, function(x) { # get sample sizes
  cc = complete.cases(comb[,c(x,'mean_psi_MPa')])
  length(cc[cc])
})
cormat <- data.frame(r=round(corr[order(corr, decreasing = T)],2),
                     n=ss[order(corr, decreasing = T)])
cormat


### 4.1 Linear Regression (no cross-validation; trial and error)

### CURRENTLY THE BEST R2 (but likely overfit...too many porameters)
m <- lm(mean_psi_MPa ~ minutes + treatment + block + daysPostTrt +
          windspeed_middle + bmp_box_temp + soil_temp_C, data = comb); summary(m)
mean(m$residuals^2)
mean(abs(m$residuals))

# AS GOOD, could add windspeed_middle in, if possible.
# could use PAR length instead of minutes.
m2 <- lm(mean_psi_MPa ~ minutes + irrig + block + 
           bmp_box_temp + leaftemp_mean, data = comb); summary(m)
mean(m2$residuals^2)
mean(abs(m2$residuals))



### 4.2 Lasso Regression ===================

# Need to have complete cases for lasso...

## approach 1: use all variables, except leaftemp_bottom, leaftemp_middle, complete cases
df2 <- subset(comb, select = -c(by15, date, leaftemp_bottom, leaftemp_middle))
df2 <- subset(df2, complete.cases(df2)); nrow(df2)

# approach 2: Omit those variables so we have more complete cases
### THIS IS THE APPROACH I ENDED UP USING FOR FINAL MODEL.
df2 <- subset(comb, select = -c(by15, date, leaftemp_bottom, leaftemp_middle,leaftemp_top,
                                windspeed_bottom, windspeed_middle, windspeed_top,
                                soil_temp_C))
df2 <- subset(df2, complete.cases(df2)); nrow(df2)

# approach 3: Keep all cases (n=60) but drop probably important variables
df2 <- subset(comb, select = -c(by15, date, leaftemp_bottom, leaftemp_middle,leaftemp_top,
                                leaftemp_highest_avail, leaftemp_mean, VPD_leaf,
                                windspeed_bottom, windspeed_middle, windspeed_top,
                                soil_temp_C))

# approach 4: Use only non-experiment specific varaibles for physical model 
# also exclude leaf temp to avoid autoregression and box_temp since that's not replicable.
df2 <- subset(comb, select = -c(by15, date, leaftemp_bottom, leaftemp_middle,leaftemp_top,
                                leaftemp_highest_avail, leaftemp_mean, VPD_leaf,
                                bmp_box_temp, block, treatment, irrig))
df2 <- subset(comb, select = -c(by15, date, leaftemp_bottom, leaftemp_middle,
                                bmp_box_temp, block, treatment, irrig))
df2 <- subset(df2, complete.cases(df2)); nrow(df2)

# create model matrix for predictor variables
x <- model.matrix(mean_psi_MPa ~ ., df2)[,-1]
# create vector for response variable
y <- df2$mean_psi_MPa


### REPEAT THE CROSS-VALIDATION N TIMES, TO SEE WHICH VARIABLES ARE CONSISTENTLY IMPORTNAT
# list to store variables
nreps <- 20
nzcList <- list()
require(glmnet)
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
v=sapply(b, function(a) length(z[z == a]))
(nzCoef = names(v[(nreps-v)<3]))

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

# predictions on all data
lasso_pred_full <- predict(full_fit_lasso, s = lasso.cv$lambda.1se, newx = x)
sqrt(mean((lasso_pred_full - y)^2))
mean(abs(lasso_pred_full - y))
plot(lasso_pred_full, y); abline(0, 1, col='red')

# Use all variables (full model)
fullmod_lasso <- lm(y ~ x[,nzCoef])
fullmod_lasso <- lm(mean_psi_MPa ~ block + bmp_box_temp + irrig + leaftemp_mean +
                      minutes + sht2_low_rh + windspeed_top, data=df2)
summary(fullmod_lasso)
sqrt(mean(fullmod_lasso$residuals^2)); mean(abs(fullmod_lasso$residuals))

# ### Predict on full data
# pred_all_fullmod_lasso <- predict(fullmod_lasso, comb_all)

### Now compare to the model Clay determined earlier was best
## (Dropped variables 'treatmentwell_watered' and 'sht2_low_rh', 'blockM'; added 'block')
# This model has better R^2 and MSE/MAD
# NOTE: THIS IS THE FINAL MODEL I USED TO MAKE PREDICTIONS !!!
lasso_simplified_model <- lm(mean_psi_MPa ~ bmp_box_temp + minutes + irrig + block + leaftemp_mean, df2)

lasso_simplified_model <- lm(mean_psi_MPa ~ line_PAR_west_umol_m2_s + minutes + irrig + block + leaftemp_mean, df2)

m=lm(mean_psi_MPa~line_PAR_west_umol_m2_s, data=comb); summary(m)
m=lm(mean_psi_MPa~poly(line_PAR_west_umol_m2_s,2), data=comb); summary(m)
m=lm(mean_psi_MPa~poly(line_PAR_west_umol_m2_s,1) + mean_irrig_kg + block, data=comb); summary(m)
m=lm(mean_psi_MPa~poly(line_PAR_east_umol_m2_s,1) + mean_irrig_kg + block + leaftemp_mean, data=comb); summary(m)
m=lm(mean_psi_MPa~poly(line_PAR_east_umol_m2_s,2) + mean_irrig_kg + block + leaftemp_mean, data=comb); summary(m)

plot(mean_psi_MPa~minutes, comb)
# NOTE: these models without BLOCK are very simple and have almost as much predictive value.
# lasso_simplified_model <- lm(mean_psi_MPa ~ minutes + irrig + leaftemp_mean + block, df2)
# lasso_simplified_model <- lm(mean_psi_MPa ~ minutes + irrig + leaftemp_mean + block, df2)

summary(lasso_simplified_model)
mean(lasso_simplified_model$residuals^2); mean(abs(lasso_simplified_model$residuals))


### Predict continuous leaf water potential on full dataset, using above model
full_predictions <- predict(lasso_simplified_model, newdata = comb_all)
summary(full_predictions); plot(density(full_predictions[!is.na(full_predictions)]))

## Append predictions to full dataset of predictors and save output
comb_all_predict <- cbind(comb_all, predicted_psi_leaf = full_predictions)
saveRDS(comb_all_predict, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/analysis/clay/output_data/combined_data_predicted_psi_leaf.rds')



#### ---------------- OLDER CODE NOT USED FOR PREDICTIONS ----------------
#### --------- RANDOM FOREST DID NOT GIVE PREDICTIONS THAT LOOKED AS BELIEVABLE AS LASS0....

### Plot the predicted psi_leaf (lines) with pressure bomb readings (dots)

# plot 2nd treatments
sub <- subset(comb_all_predict, date(by15) >= '2019-11-11' & date(by15) <= '2019-11-12')
# plot 3rd treatments
sub <- subset(comb_all_predict, date(by15) >= '2019-12-01' & date(by15) <= '2019-12-08')

ggplot(comb_all_predict, aes(x=by15, y=predicted_psi_leaf, color=treatment)) + geom_line() +
  geom_point(aes(x=by15, y=mean_psi_MPa), size=3)



### RANDOM FORESTS

require(ranger) 
require(caret)


# examine PAR data for df2
summary(df2$par1_n)
summary(df2$line_PAR_east_umol_m2_s)
summary(df2$line_PAR_west_umol_m2_s)
plot(df2$line_PAR_west_umol_m2_s, df2$mean_psi_MPa, col = df2$treatment, pch = 19)
plot(df2$par1_n, df2$mean_psi_MPa, col = df2$treatment, pch = 19)
legend('bottomright', legend=c(levels(df2$treatment)), col=1:4, pch=19, cex=0.6)

ggplot(df2) +
  geom_point(aes(x=par1_n, y=mean_psi_MPa, color=treatment)) +
  geom_smooth(aes(x=par1_n, y=mean_psi_MPa, color=treatment), method = 'lm', formula = y ~ poly(x,2))
summary(lm(mean_psi_MPa ~ poly(par1_n, 2) + poly(leaftemp_mean, 2) + treatment, df2))

ggplot(df2) +
  geom_point(aes(x=line_PAR_west_umol_m2_s, y=mean_psi_MPa, color=treatment)) +
  geom_smooth(aes(x=line_PAR_west_umol_m2_s, y=mean_psi_MPa, color=treatment), method = 'lm', formula = y ~ poly(x,2), se=F)

ggplot(df2) +
  geom_point(aes(x=leaftemp_mean, y=mean_psi_MPa, color=treatment)) +
  geom_smooth(aes(x=leaftemp_mean, y=mean_psi_MPa, color=treatment), method = 'lm', formula = y ~ poly(x,2), se=F)
summary(lm(mean_psi_MPa ~ poly(leaftemp_mean, 2) + treatment, df2))

ggplot(df2) +
  geom_point(aes(x=bmp_box_temp, y=mean_psi_MPa, color=treatment)) +
  geom_smooth(aes(x=bmp_box_temp, y=mean_psi_MPa, color=treatment), method = 'lm', formula = y ~ poly(x,2), se=F)

ggplot(df2) +
  geom_point(aes(x=altd, y=mean_psi_MPa, color=treatment)) +
  geom_smooth(aes(x=altd, y=mean_psi_MPa, color=treatment), method = 'lm', formula = y ~ x, se=T)

summary(lm(mean_psi_MPa ~ poly(leaftemp_mean, 2) + treatment, df2))
summary(lm(mean_psi_MPa ~ poly(leaftemp_mean, 2) + mean_irrig_kg, df2))
summary(lm(mean_psi_MPa ~ poly(leaftemp_mean, 1)* par_threshold + treatment , df2))



ggplot(df2) +
  geom_point(aes(x=altd, y=mean_psi_MPa, color=treatment)) +
  geom_smooth(aes(x=altd, y=mean_psi_MPa, color=treatment), method = 'lm', formula = y ~ poly(x,2), se=F)
summary(lm(mean_psi_MPa ~ poly(altd, 1) + treatment, df2))
summary(lm(mean_psi_MPa ~ poly(altd, 1) + treatment, df2))

# fit a one-off random forest model (no tuning)
rf_quick_allvars <- ranger(mean_psi_MPa~ .,
                           data=df2,
                           num.trees=5000,
                           mtry=NULL,
                           importance = 'impurity')
print(rf_quick_allvars)
importance(rf_quick_allvars)[order(importance(rf_quick_allvars), decreasing = T)]

# try just some variables of interest

rf_quick <- ranger(mean_psi_MPa ~ leaftemp_mean + mean_irrig_kg + stress_index + par1_n + pyr1_n,
                   data=df2,
                   num.trees=5000,
                   mtry=NULL,
                   min.node.size = 1,
                   importance = 'impurity')
print(rf_quick)
importance(rf_quick)[order(importance(rf_quick), decreasing = T)]



### --- Tune the Random Forest Model --

# Set the tuning/training hyperparameters
fitControl_randomForest <- trainControl(method='repeatedcv', 
                                        number=5, # either # folds (repeatedcv) or # of resampling iterations (not sure what method this is for?)
                                        repeats=3, # complete sets of folds to compute (repeatedcv method only)
                                        classProbs=F, # only for classification models
                                        verboseIter=F, # print training log
                                        search='grid', # grid or random
                                        savePredictions = 'final') # all, final or none

# (optional) create grid of tuning parameters for train()
sqrt(ncol(df2_train-1))
tuneGrid = expand.grid(mtry = 1:3, 
                       splitrule = c('variance', 'extratrees', 'maxstat'),
                       min.node.size = 1:3)


# train the model
randomForest_pl <- train(mean_psi_MPa ~ treatment + irrig + leaftemp_highest_avail +
                           minutes + par1_n,
                         data=df2,
                         metric='RMSE',
                         trControl=fitControl_randomForest,
                         # tuneLength=5, # use instead of tuneGrid (? I think)
                         method='ranger',
                         tuneGrid = tuneGrid,
                         importance = 'impurity')

# using all variables.
randomForest_pl <- train(mean_psi_MPa ~ .,
                         data=df2,
                         metric='RMSE',
                         trControl=fitControl_randomForest,
                         # tuneLength=5, # use instead of tuneGrid (? I think)
                         method='ranger',
                         tuneGrid = tuneGrid, 
                         importance = 'impurity')
plot(varImp(randomForest_pl))
print(randomForest_pl)
best <- randomForest_pl$bestTune
res <- randomForest_pl$results
res[res$RMSE == min(res$RMSE),]
res[res$Rsquared == max(res$Rsquared),]
plot(randomForest_pl)

# Fit again using the "best" tuned model
rf_best <- ranger(mean_psi_MPa~ treatment + irrig + leaftemp_highest_avail +
                    minutes + par1_n,
                  data=df2,
                  num.trees=5000,
                  mtry=best$mtry,
                  importance = 'impurity', 
                  splitrule = best$splitrule,
                  min.node.size = best$min.node.size)
importance(rf_best)[order(importance(rf_best), decreasing = T)]



### make predictions using the tuned RF
pred <- predict(randomForest_pl, df2)

summary(df2$mean_psi_MPa); summary(pred)


## calculate RMSE 
sqrt(mean((pred - df2$mean_psi_MPa)^2)) # on all data


## calculate MAE (mean absolute error)
mean(abs(pred - df2$mean_psi_MPa))

## Plot y vs. yhat
plot(df2$mean_psi_MPa, pred, xlab='actual', ylab='predicted'); abline(0,1) # train points in black


### Predictions on full data set --------------------------

### Random Forest

## THIS DOESN'T SEEM TO BE WORKING RIGHT, THE RANGE IS SMALLER THAN FOR THE TEST OR TRAINIG SETS...

# select only columns used in training model
comb_all_predict <- comb_all[ , names(comb_all)[names(comb_all) %in% 
                                                  c('by15','date',names(df2))]]
copy <- comb_all_predict
comb_all_predict$mean_psi_MPa <- NULL
comb_all_predict <- comb_all_predict[complete.cases(comb_all_predict),]
comb_all_predict$yhat_rf <- predict(randomForest_pl, 
                                    newdata = comb_all_predict, 
                                    type = 'raw')
comb_all_predict$yhat_lasso <- predict(fullmod_lasso, newdata = comb_all_predict)

# merge back 
comb_all_predict <- merge(comb_all_predict, copy[,c('by15','block','mean_psi_MPa')])

# using ranger "quick" forest instead.
# pred_all_rf <- predict(rf_best, 
# select only columns used in training model
# data = comb_all[ , names(comb_all)[names(comb_all) %in% names(df2)]])

# pred_all_rf <- pred_all_rf * sd(df2$mean_psi_MPa) + mean(df2$mean_psi_MPa) # unscaled
summary(df2$mean_psi_MPa); summary(pred_all_rf)
plot(density(df2$mean_psi_MPa)); plot(density(pred_all_rf))


### Linear Models
mod2 <- lm(mean_psi_MPa ~ minutes + irrig + block + poly(leaftemp_mean,2), df2); summary(mod2)
# mod3 <- lm(mean_psi_MPa ~ irrig + block + leaftemp_mean * par1_n, df2); summary(mod3)
mod3 <- lm(mean_psi_MPa ~ par1_n + irrig + block +  poly(leaftemp_mean,2), df2); summary(mod3)

sqrt(mean(mod2$residuals^2)); mean(abs(mod2$residuals))
sqrt(mean(mod3$residuals^2)); mean(abs(mod3$residuals))

pred_all_lm <- predict(mod2, comb_all)
pred_all_lm_2 <- predict(mod3, comb_all)


### Plot the predicted psi_leaf =========================================

# add predcitions to comb_all
comb_all_predict <- comb_all
# comb_all_predict$yhat_lasso_phys <- pred_all_fullmod_lasso_phys
comb_all_predict$yhat_lm <- pred_all_lm
comb_all_predict$yhat_lm_2 <- pred_all_lm_2

comb_all_predict$yhat_rf <- pred_all_rf
# head(comb_all_predict)

# plot 2nd treatments
sub <- subset(comb_all_predict, date(by15) >= '2019-11-04' & date(by15) <= '2019-11-27', drop = F)
# plot 3rd treatments
sub <- subset(comb_all_predict, date(by15) >= '2019-12-01' & date(by15) <= '2019-12-08')
sub <- subset(comb_all_predict, date(by15) == '2019-11-20')

# ggplot(sub, aes(x=by15, y=yhat_rf, color=treatment)) + geom_line() +
#   # geom_line(aes(x=by15, y=line_PAR_east_umol_m2_s/500, color='black')) +
#   geom_point(aes(x=by15, y=mean_psi_MPa), size=3)


# plot 1st period

sub <- subset(comb_all_predict, date(by15) >= '2019-11-18' & date(by15) <= '2019-11-21', drop = F)

png(paste0('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/psi_leaf_model/',
'model_pred_no_PAR_period_1.png'), width=1400, height=700)
ggplot(sub) +
  geom_line(aes(x=by15, y=yhat_lm, color=treatment)) + ylim(c(-0.5,2.5)) +
  geom_line(aes(x=by15, y=par1_n/500), linetype = 'dashed') +
  geom_point(aes(x=by15, y=mean_psi_MPa, color=treatment), size=3) + ggtitle('Model without PAR')
dev.off()


# ggplot(sub) + 
#   geom_line(aes(x=by15, y=altd, color=treatment)) +
#   geom_line(aes(x=by15, y=leaftemp_mean, color=treatment), linetype = 'dashed')
#   geom_line(aes(x=by15, y=leaftemp_mean, color=treatment)) +
#   geom_line(aes(x=by15, y=temp_high_mean), linetype = 'dashed') +
#   geom_line(aes(x=by15, y=bmp_box_temp), linetype = 'solid')

png(paste0('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/psi_leaf_model/',
           'model_pred_with_PAR_period_1.png'), width=1400, height=700)
ggplot(sub) +
  geom_line(aes(x=by15, y=yhat_lasso, color=treatment)) + ylim(c(-0.5,2.5)) +
  geom_line(aes(x=by15, y=par1_n/500), linetype = 'dashed') +
  geom_point(aes(x=by15, y=mean_psi_MPa, color=treatment), size=3) + ggtitle('Model with PAR')
dev.off()

# plot 2nd period
sub <- subset(comb_all_predict, date(by15) >= '2019-12-09' & date(by15) <= '2019-12-12', drop = F)

png(paste0('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/psi_leaf_model/',
           'model_pred_no_PAR_period_2.png'), width=1400, height=700)
ggplot(sub) +
  geom_line(aes(x=by15, y=yhat_lm, color=treatment)) + ylim(c(-0.5,2.5)) +
  geom_line(aes(x=by15, y=par1_n/500), linetype = 'dashed') +
  geom_point(aes(x=by15, y=mean_psi_MPa, color=treatment), size=3) + ggtitle('Model without PAR')
dev.off()

png(paste0('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/psi_leaf_model/',
           'model_pred_with_PAR_period_2.png'), width=1400, height=700)
ggplot(sub) +
  geom_line(aes(x=by15, y=yhat_lm_2, color=treatment)) + ylim(c(-0.5,2.5)) +
  geom_line(aes(x=by15, y=par1_n/500), linetype = 'dashed') +
  geom_point(aes(x=by15, y=mean_psi_MPa, color=treatment), size=3) + ggtitle('Model with PAR')
dev.off()

### NOTES
# Using full data set (n=60), random forest does poor job (does not capture high/low values of psi_leaf)