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
sapply(b, function(a) length(z[z == a]))

# THESE 7 VARIABLES SEEM MOST IMPORTANT:
# blockM, bMP_box_temp, irrig, leaftemp_mean, minutes, sht2_low_rh, "treatmentwell_watered" (somewhat)
# leaftemp_top and windspeed_top important only if using these variables (but results in smaller n)
# Or using "derived variables", par_length also important
# to a much lesser degree, "daysPostTrt" is also important

# CV using full dataset
lasso.cv <- cv.glmnet(x, y, family='gaussian', alpha=1, nfolds=10, standardize=T); plot(lasso.cv)
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
mean((lasso_pred_full - y)^2)
mean(abs(lasso_pred_full - y))
plot(lasso_pred_full, y); abline(0, 1, col='red')

# Use all variables (full model)
fullmod_lasso_phys <- lm(y ~ x[,nzCoef])
summary(fullmod_lasso_phys)
mean(fullmod_lasso_phys$residuals^2); mean(abs(fullmod_lasso_phys$residuals))

# ### Predict on full data
# pred_all_fullmod_lasso_phys <- predict(fullmod_lasso_phys, comb_all)

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



### ----- Compare some models using cross-validation approach
### Split data into test/train sets

cvFun <- function(x, propTrain) {
  
  # non-stratified sampling
  train <- sample(1:nrow(df2), nrow(df2)*propTrain, replace = F)
  
  # create split datasests
  df2_train <- df2[train,]; nrow(df2_train)
  df2_test <- df2[-train,]; nrow(df2_test)
  nrow(df2_train); nrow(df2_test)
  
  m1 <- lm(mean_psi_MPa ~ bmp_box_temp + minutes + irrig + block + leaftemp_mean, df2_train)
  m2 <- lm(mean_psi_MPa ~ minutes + irrig + leaftemp_mean + block, df2_train)
  m3 <- lm(mean_psi_MPa ~ minutes + irrig + poly(VPD_leaf, 2), df2_train)
  
  mod.list <- list(m1, m2, m3)
  
  x=sapply(mod.list, function(m) {
    y.hat = predict(m, newdata = df2_test)
    resid = df2_test$mean_psi_MPa - y.hat
    mean(resid^2)
  })
  return(which(x==min(x))) # return the model number w/smallest test MSE
}

cvout = sapply(1:500, function(x) cvFun(x, propTrain = 0.8))
table(cvout)
cvout = sapply(1:500, function(x) cvFun(x, propTrain = 0.5))
table(cvout)


### RANDOM FORESTS

require(ranger) 
require(caret)


# scale entire data set before splitting
# cc <- sapply(df2, class) # column classes
# df2_scaled <- df2[ , names(cc[cc == 'numeric'])]
# df2_scaled <- cbind(df2[ , names(cc[cc != 'numeric'])],
#                           as.data.frame(scale(df2_scaled)))

### Split data into test/train sets
set.seed(1)
propTrain <- 0.8

# split the data
# stratified sampling based on treatment
stratvar <- 'treatment'

# stratified sampling based on response
stratvar <- 'mean_psi_MPa'

### OK to stratify using UNSCALED values for psi_leaf (?)
train <- as.numeric(createDataPartition(df2[[stratvar]], p = propTrain, list = F))

# non-stratified sampling
train <- sample(1:nrow(df2), nrow(df2)*propTrain, replace = F)

# create split datasests
df2_train <- df2[train,]; nrow(df2_train)
df2_test <- df2[-train,]; nrow(df2_test)
# df2_train_scaled <- df2_scaled[train,]; nrow(df2_train_scaled)
# df2_test_scaled <- df2_scaled[-train,]; nrow(df2_test_scaled)

# treatment
prop.table(table(df2$treatment)); prop.table(table(df2_test_scaled$treatment)); prop.table(table(df2_train_scaled$treatment))
# block
prop.table(table(df2$block)); prop.table(table(df2_test_scaled$block)); prop.table(table(df2_train_scaled$block))
#

# check stratification
prop.table(table(df2_train_scaled[[stratvar]])); prop.table(table(df2_test_scaled[[stratvar]]))
summary(df2_train_scaled[[stratvar]]); summary(df2_test_scaled[[stratvar]])



### **********
### Scale the test and training sets independently
### *********** THIS PROBABLY NOT IMPORTANT, LET'S TABLE FOR NOW....

# subset to only numeric columns
# cc <- sapply(df2, class)
# df2_train_scaled <- df2_train[ , names(cc[cc == 'numeric'])]
# df2_train_scaled <- cbind(df2_train[ , names(cc[cc != 'numeric'])],
#                           as.data.frame(scale(df2_train_scaled)))
# 
# df2_test_scaled <- df2_test[ , names(cc[cc == 'numeric'])]
# df2_test_scaled <- cbind(df2_test[ , names(cc[cc != 'numeric'])],
#                           as.data.frame(scale(df2_test_scaled)))


# fit a one-off random forest model (no tuning)
rf_quick <- ranger(mean_psi_MPa~ .,
                   data=df2_train,
                   num.trees=5000,
                   mtry=NULL,
                   importance = 'impurity')

# treeInfo(Rf1)
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
plot(randomForest_pl)

# Fit again using the "best" tuned model
rf_best <- ranger(mean_psi_MPa~ .,
                  data=df2_train,
                  num.trees=5000,
                  mtry=best$mtry,
                  importance = 'impurity', 
                  splitrule = best$splitrule,
                  min.node.size = best$min.node.size)
importance(rf_best)[order(importance(rf_best), decreasing = T)]



### make predictions using the tuned RF
pred <- predict(randomForest_pl, df2)
train_pred <- pred[train]
test_pred <- pred[-train]

# using refit forest "best"
# train_pred <- predict(rf_best, df2_train)$predictions 
# test_pred <- predict(rf_best, df2_test)$predictions

# unscale the predictions
# train_pred <- train_pred * sd(df2$mean_psi_MPa) + mean(df2$mean_psi_MPa)
# test_pred <- test_pred * sd(df2$mean_psi_MPa) + mean(df2$mean_psi_MPa)

summary(df2$mean_psi_MPa); summary(pred)
summary(df2_train$mean_psi_MPa); summary(train_pred)
summary(df2_test$mean_psi_MPa); summary(test_pred)


## calculate RMSE 
sqrt(mean((pred - df2$mean_psi_MPa)^2)) # on all data
sqrt(mean((test_pred - df2_test$mean_psi_MPa)^2)) # on test set
sqrt(mean((train_pred - df2_train$mean_psi_MPa)^2)) # on train set

## calculate MAE (mean absolute error)
mean(abs(test_pred - df2_test$mean_psi_MPa))
mean(abs(train_pred - df2_train$mean_psi_MPa))

## Plot y vs. yhat
plot(df2_train$mean_psi_MPa, train_pred, xlab='actual', ylab='predicted'); abline(0,1) # train points in black
points(df2_test$mean_psi_MPa, test_pred, col='red') # test points in red


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

# merge back 
comb_all_predict <- merge(comb_all_predict, copy[,c('by15','block','mean_psi_MPa')])

# using ranger "quick" forest instead.
# pred_all_rf <- predict(rf_best, 
                       # select only columns used in training model
                       # data = comb_all[ , names(comb_all)[names(comb_all) %in% names(df2)]])

# pred_all_rf <- pred_all_rf * sd(df2$mean_psi_MPa) + mean(df2$mean_psi_MPa) # unscaled
summary(df2$mean_psi_MPa); summary(pred_all_rf)
plot(density(df2$mean_psi_MPa)); plot(density(pred_all_rf))


### Linear Model
mod2 <- lm(mean_psi_MPa ~ bmp_box_temp + minutes + irrig + block + leaftemp_mean, df2); summary(mod2)
mean(mod2$residuals^2); mean(abs(mod2$residuals))
pred_all_lm <- predict(mod2, comb_all)



### Plot the predicted psi_leaf =========================================

# add predcitions to comb_all
comb_all_predict <- comb_all
comb_all_predict$yhat_lasso_phys <- pred_all_fullmod_lasso_phys
comb_all_predict$yhat_lm <- pred_all_lm
comb_all_predict$yhat_rf <- pred_all_rf
head(comb_all_predict)

# plot 2nd treatments
sub <- subset(comb_all_predict, date(by15) >= '2019-11-04' & date(by15) <= '2019-11-27', drop = F)
# plot 3rd treatments
sub <- subset(comb_all_predict, date(by15) >= '2019-12-01' & date(by15) <= '2019-12-08')
sub <- subset(comb_all_predict, date(by15) == '2019-11-20')

ggplot(sub, aes(x=by15, y=yhat_rf, color=treatment)) + geom_line() +
  # geom_line(aes(x=by15, y=line_PAR_east_umol_m2_s/500, color='black')) +
  geom_point(aes(x=by15, y=mean_psi_MPa), size=3)

ggplot(sub, aes(x=by15, y=yhat_lm, color=treatment)) + geom_line() +
  # geom_line(aes(x=by15, y=leaftemp_mean/10)) +
  geom_point(aes(x=by15, y=mean_psi_MPa), size=3)

### NOTES
# Using full data set (n=60), random forest does poor job (does not capture high/low values of psi_leaf)