rm(list=ls())

# read in prepped data
widthData <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_prepped.rds')


### 1. First, Random Forests without using leaf lengths

## Exclude leaves < 90% expanded from training data
widthData <- widthData[widthData$percent_expanded >= 90,]

require(randomForest); require(tdr); require(reshape2); require(tibble); require(caret)
require(ranger)
# NOTE: I poached this from Dave's code and modified it.
# this function takes random subsets of the dataset and characterizes the distribution of error terms for a lot of models   
rfFunc <- function(x){
  # print(x)
  # sample 25% of plants for testing
  testPlantID <- sample(unique(widthData$pot_id), 7) 
  # sample 75% for training
  trainPlants <- subset(widthData, !pot_id %in% testPlantID) 
  testPlant <- subset(widthData, pot_id %in% testPlantID) 
  
  mod <- randomForest(leaf_area_cm2 ~ leaf_width_cm + mean_width_cm + leaf_order_reverse +
                        days_since_planting,
                      data=trainPlants, mtry=2, num.trees=500)
  testPlant$modelPredictions <- predict(mod, newdata=testPlant, type='response')
  
  tdStats(m=testPlant$modelPredictions, o=testPlant$leaf_area_cm2,
          functions=c('mbe', 'mae', 'r2')) %>%
    melt %>%
    rownames_to_column(., 'stat')
}

require(dplyr)
modelRuns <- bind_rows(lapply(1:1000, rfFunc)) 

modelRuns %>%
  group_by(stat) %>%
  summarise(med=median(value))

ggplot(modelRuns, aes(x=value)) + theme_bw(base_size=15) +
  geom_density(fill='dodgerblue3', alpha=0.75) +
  facet_wrap(~stat, scales='free')



# ---  Looks good, let's tune the model and then make predictions on a test set

# training set
propTrain <- 0.75
train <- sample(1:nrow(widthData), size = nrow(widthData) * propTrain, replace = F)

harvest_train <- widthData[train,]
harvest_test <- widthData[-train,]

### --- Tune the Random Forest Model --

# Set the tuning/training hyperparameters
fitControl_randomForest <- trainControl(method='repeatedcv', 
                                        number=10, # either # folds (repeatedcv) or # of resampling iterations (not sure what method this is for?)
                                        repeats=3, # complete sets of folds to compute (repeatedcv method only)
                                        classProbs=F, # only for classification models
                                        verboseIter=F, # print training log
                                        search='grid', # grid or random
                                        savePredictions = 'final') # all, final or none

# (optional) create grid of tuning parameters for train()
tuneGrid = expand.grid(mtry = 1:4, 
                       splitrule = c('variance', 'extratrees', 'maxstat'),
                       min.node.size = 1)

# train the model
randomForest_pl <- train(leaf_area_cm2 ~ leaf_width_cm + mean_width_cm +
                           leaf_order_reverse + days_since_planting,
                         data=harvest_train,
                         metric='RMSE',
                         trControl=fitControl_randomForest,
                         # tuneLength=5, # use instead of tuneGrid (? I think)
                         method='ranger',
                         tuneGrid = tuneGrid)
print(randomForest_pl)
plot(randomForest_pl, metric = c('RMSE'))
randomForest_pl$bestTune
res <- randomForest_pl$results
res[res$MAE == min(res$MAE),]
res[res$Rsquared == max(res$Rsquared),]

harvest_test$leaf_area_pred <- predict(randomForest_pl, newdata = harvest_test)

# plot predicted vs. actual
plot(leaf_area_pred ~ leaf_area_cm2, data = harvest_test); abline(c(0,1))


# test RMSE, MAD, MBE
sqrt(mean((harvest_test$leaf_area_cm2 - harvest_test$leaf_area_pred)^2)) # 14.5, 9.1, 8.1
mean(abs(harvest_test$leaf_area_cm2 - harvest_test$leaf_area_pred)) # 10.5, 7.7, 6.8
mean(harvest_test$leaf_area_cm2 - harvest_test$leaf_area_pred) # -2.4, 0.7, 1.1


# Now predict on ALL data and scale up error to entire plant
widthData$leaf_area_pred <- predict(randomForest_pl, newdata = widthData)

out <- ddply(widthData, .(pot_id, date, treatment), function(x) {
  setNames(c(sum(x$leaf_area_cm2), sum(x$leaf_area_pred)), c('true','pred'))
})

out$error <- out$true - out$pred
out$perc_error <- out$error / out$true
hist(out$perc_error)
hist(out$error)
plot(out$true, out$pred); abline(c(0,1))

summary(lm(true ~ pred, out))

### ---- Repeat on FULL DATA SET


# Set the tuning/training hyperparameters
fitControl_randomForest <- trainControl(method='repeatedcv', 
                                        number=10, # either # folds (repeatedcv) or # of resampling iterations (not sure what method this is for?)
                                        repeats=5, # complete sets of folds to compute (repeatedcv method only)
                                        classProbs=F, # only for classification models
                                        verboseIter=F, # print training log
                                        search='grid', # grid or random
                                        savePredictions = 'final') # all, final or none
# (optional) create grid of tuning parameters for train()
tuneGrid = expand.grid(mtry = 3,
                       splitrule = 'extratrees',
                       # splitrule = c('variance', 'extratrees', 'maxstat'),
                       min.node.size = 1:2)

tuneGrid = expand.grid(mtry = 1:4,
                       splitrule = 'extratrees',
                       # splitrule = c('variance', 'extratrees', 'maxstat'),
                       min.node.size = 1:2)

# train the model
# predictors: leaf_width_cm, leaf_order, leaf_order_reverse, days_since_planting,
# mean_width_cm, irrig_cummean
randomForest_pl_2 <- train(leaf_area_cm2 ~ leaf_width_cm + leaf_length_cm + leaf_order_reverse +
                             leaf_order + days_since_planting + mean_width_cm,
                         data=widthData,
                         metric='RMSE',
                         trControl=fitControl_randomForest,
                         # tuneLength=5, # use instead of tuneGrid (? I think)
                         method='ranger',
                         tuneGrid = tuneGrid,
                         importance = 'impurity')
varImp(randomForest_pl_2)
print(randomForest_pl_2)
plot(randomForest_pl_2, metric = c('RMSE'))
randomForest_pl_2$bestTune
res <- randomForest_pl_2$results
res[res$RMSE == min(res$RMSE),]
res[res$MAE == min(res$MAE),]
res[res$Rsquared == max(res$Rsquared),]



# predcitions
widthData$leaf_area_pred <- predict(randomForest_pl_2, newdata = widthData)

# plot predicted vs. actual
plot(leaf_area_pred ~ leaf_area_cm2, data = widthData); abline(c(0,1), col='red')


# RMSE, MAD, MBE
sqrt(mean((widthData$leaf_area_cm2 - widthData$leaf_area_pred)^2)) # 14.5, 9.1, 8.1
mean(abs(widthData$leaf_area_cm2 - widthData$leaf_area_pred)) # 10.5, 7.7, 6.8
mean(widthData$leaf_area_cm2 - widthData$leaf_area_pred) # -2.4, 0.7, 1.1


# Now predict on ALL data and scale up error to entire plant
# widthData$leaf_area_pred <- predict(randomForest_pl_2, newdata = widthData)

out <- ddply(widthData, .(pot_id, date, treatment), function(x) {
  setNames(c(sum(x$leaf_area_cm2), sum(x$leaf_area_pred)), c('true','pred'))
})

out$error <- out$true - out$pred
out$perc_error <- out$error / out$true
hist(out$perc_error)
hist(out$error)
plot(out$true, out$pred); abline(c(0,1))
summary(lm(true ~ pred, out))


## Save data plus predicted leaf area
names(widthData)
widthData <- subset(widthData, select = -c(leaf_order, irrig_cummean))
saveRDS(widthData, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/harvest_LA_pred.rds')


## Save the model
saveRDS(randomForest_pl_2, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_RF_model.rds')
