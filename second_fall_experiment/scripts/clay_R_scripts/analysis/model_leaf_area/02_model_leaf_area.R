rm(list=ls())

# read in prepped data
widthData <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_prepped.rds')


### 1. First, Random Forest to predict leaf length from leaf width.

## Exclude leaves < 90% expanded from training data
widthData <- widthData[widthData$percent_expanded >= 90,]

require(randomForest); require(tdr); require(reshape2); require(tibble); require(caret)
require(ranger); require(plyr)

### Try to Model Leaf Length from other parameters....

# Set the tuning/training hyperparameters
fitControl_randomForest <- trainControl(method='repeatedcv', 
                                        number=10, # either # folds (repeatedcv) or # of resampling iterations (not sure what method this is for?)
                                        repeats=5, # complete sets of folds to compute (repeatedcv method only)
                                        classProbs=F, # only for classification models
                                        verboseIter=F, # print training log
                                        search='grid', # grid or random
                                        savePredictions = 'final') # all, final or none
# (optional) create grid of tuning parameters for train()

tuneGrid = expand.grid(mtry = 1:5,
                       splitrule = 'extratrees',
                       # splitrule = c('variance', 'extratrees', 'maxstat'),
                       min.node.size = 1:10)

# train the model
# predictors: leaf_width_cm, leaf_order, leaf_order_reverse, days_since_planting,
# mean_width_cm, 
rf_length <- train(leaf_length_cm ~ leaf_width_cm + leaf_order_reverse + leaf_order +
                     days_since_planting + mean_width_cm,
                   data=widthData,
                   metric='RMSE',
                   trControl=fitControl_randomForest,
                   # tuneLength=5, # use instead of tuneGrid (? I think)
                   method='ranger',
                   tuneGrid = tuneGrid,
                   importance = 'impurity')
varImp(rf_length)
# print(rf_length)
plot(rf_length, metric = c('RMSE'))
rf_length$bestTune
res <- rf_length$results
res[res$RMSE == min(res$RMSE),]
res[res$MAE == min(res$MAE),]
res[res$Rsquared == max(res$Rsquared),]


# predcitions
widthData$leaf_length_pred <- predict(rf_length, newdata = widthData)
# plot predicted vs. actual
plot(leaf_length_pred ~ leaf_length_cm, data = widthData); abline(c(0,1), col='red')
# RMSE
sqrt(mean((widthData$leaf_length_cm - widthData$leaf_length_pred)^2)) 

## Save the leaf length RF model
saveRDS(rf_length, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_length_RF_model.rds')


### ------- Now, use predicted leaf length in RF model to predict leaf area


# Set the tuning/training hyperparameters
fitControl_randomForest <- trainControl(method='repeatedcv', 
                                        number=10, # either # folds (repeatedcv) or # of resampling iterations (not sure what method this is for?)
                                        repeats=5, # complete sets of folds to compute (repeatedcv method only)
                                        classProbs=F, # only for classification models
                                        verboseIter=F, # print training log
                                        search='grid', # grid or random
                                        savePredictions = 'final') # all, final or none
# (optional) create grid of tuning parameters for train()

tuneGrid = expand.grid(mtry = 1:6,
                       splitrule = 'extratrees',
                       # splitrule = c('variance', 'extratrees', 'maxstat'),
                       min.node.size = 1:10)

# train the model
# predictors: leaf_width_cm, leaf_order, leaf_order_reverse, days_since_planting,
# mean_width_cm
rf_area <- train(leaf_area_cm2 ~ leaf_length_pred + leaf_width_cm + leaf_order_reverse + leaf_order +
                   days_since_planting + mean_width_cm,
                 data=widthData,
                 metric='RMSE',
                 trControl=fitControl_randomForest,
                 # tuneLength=5, # use instead of tuneGrid (? I think)
                 method='ranger',
                 tuneGrid = tuneGrid,
                 importance = 'impurity')
varImp(rf_area)
# print(rf_area)
plot(rf_area, metric = c('RMSE'))
rf_area$bestTune
res <- rf_area$results
res[res$RMSE == min(res$RMSE),]
res[res$MAE == min(res$MAE),]
res[res$Rsquared == max(res$Rsquared),]


# predcitions
widthData$leaf_area_pred <- predict(rf_area, newdata = widthData)
# plot predicted vs. actual
plot(leaf_area_pred ~ leaf_area_cm2, data = widthData); abline(c(0,1), col='red')
# RMSE
sqrt(mean((widthData$leaf_area_cm2 - widthData$leaf_area_pred)^2)) 


## Save the leaf area RF model
saveRDS(rf_area, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_RF_model.rds')


# Now predict on ALL data and scale up error to entire plant

out <- ddply(widthData, .(pot_id, date, treatment), function(x) {
  setNames(c(sum(x$leaf_area_cm2), sum(x$leaf_area_pred)), c('true','pred'))
})

out$error <- out$true - out$pred
out$perc_error <- out$error / out$true
hist(out$perc_error)
plot(out$true, out$pred); abline(c(0,1))
summary(lm(true ~ pred, out))


## Save data plus predicted leaf area
names(widthData)
saveRDS(widthData, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/harvest_LA_pred.rds')
