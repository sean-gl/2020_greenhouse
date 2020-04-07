
### EDA

# ratio of length/width
harvest$lw_ratio <- harvest$leaf_length_cm / harvest$leaf_width_cm
plot(density(harvest$lw_ratio))
summary(harvest$lw_ratio)
plot(density(harvest$lw_ratio[harvest$date_num==1]))
lines(density(harvest$lw_ratio[harvest$date_num==2]), col='red')
lines(density(harvest$lw_ratio[harvest$date_num==3]), col='blue')
plot(lw_ratio ~ leaf_order_reverse, harvest)

# effect of date
plot(leaf_area_cm2 ~ leaf_length_cm, harvest, col = date_num, pch = 19)
plot(leaf_area_cm2 ~ leaf_width_cm, harvest, col = date_num, pch = 19)
# for a given width, leaves are shorter on 12/12 than 10/31, 
# esp. for narrower (shorter) leaves.
plot(leaf_length_cm ~ leaf_width_cm, harvest, col = date_num, pch = 19)


# effect of treatment
u <- unique(harvest$treatment); u
harvest$trt_num <- match(harvest$treatment, u) 
plot(leaf_length_cm ~ leaf_width_cm, harvest, col = trt_num, pch = 19)



### Predict leaf length based on width

summary(lm(leaf_length_cm ~ leaf_width_cm + leaf_id + leaf_order_reverse, data=harvest))
# add date_num
summary(lm(leaf_length_cm ~ leaf_width_cm + leaf_id + leaf_order_reverse + date_num, data=harvest))
# add date_num (factor)
summary(lm(leaf_length_cm ~ leaf_width_cm + leaf_id + leaf_order_reverse + as.factor(date_num), data=harvest))
# add days since planting
summary(lm(leaf_length_cm ~ leaf_width_cm + leaf_id + leaf_order_reverse + days_since_planting, data=harvest))
# add quadratic term...CRAP
summary(lm(leaf_length_cm ~ poly(leaf_width_cm, 2) + leaf_id + leaf_order_reverse, data=harvest))


Rf1 <- ranger(leaf_length_cm ~ leaf_width_cm + leaf_id + leaf_order + leaf_order_reverse + days_since_planting,
              data=harvest, num.trees=5000, mtry=2)
Rf1

# predict LA 
Rf1 <- ranger(leaf_area_cm2 ~ leaf_width_cm + mean_width_cm +
                leaf_id + leaf_order + leaf_order_reverse + days_since_planting,
              data=harvest, num.trees=5000, mtry=NULL, importance = 'impurity')
Rf1
importance(Rf1)[order(importance(Rf1), decreasing = T)]
summary(Rf1)
# add leaf length
Rf1 <- ranger(leaf_area_cm2 ~ leaf_width_cm + leaf_length_cm + mean_width_cm + leaf_id + leaf_order + leaf_order_reverse + days_since_planting,
              data=harvest, num.trees=5000, mtry=NULL, importance = 'impurity')
Rf1
importance(Rf1)[order(importance(Rf1), decreasing = T)]

Rf1 <- ranger(leaf_area_cm2 ~ leaf_width_cm + leaf_order_reverse + days_since_planting,
              data=harvest, num.trees=5000, mtry=NULL, importance = 'impurity')
Rf1
importance(Rf1)[order(importance(Rf1), decreasing = T)]


# training set
propTrain <- 0.75
train <- sample(1:nrow(harvest), size = nrow(harvest) * propTrain, replace = F)

harvest_train <- harvest[train,]
harvest_test <- harvest[-train,]

# fit a one-off random forest model (no tuning)
rf_quick <- ranger(leaf_length_cm ~ leaf_width_cm + leaf_id + leaf_order + leaf_order_reverse + days_since_planting,
                   data=harvest_train,
                   num.trees=5000,
                   mtry=NULL,
                   importance = 'impurity')
print(rf_quick)
varImpPlot(rf_quick)

# leaf id doesn't matter...
rf_quick <- ranger(leaf_length_cm ~ leaf_width_cm + leaf_order + leaf_order_reverse + days_since_planting,
                   data=harvest_train,
                   num.trees=5000,
                   mtry=NULL,
                   importance = 'impurity')
print(rf_quick)
importance(rf_quick)[order(importance(rf_quick), decreasing = T)]


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
tuneGrid = expand.grid(mtry = 2:3, 
                       splitrule = c('variance', 'extratrees', 'maxstat'),
                       min.node.size = seq(2, 10, 2))

tuneGrid = expand.grid(mtry = 1:2, 
                       splitrule = 'extratrees',
                       min.node.size = seq(1, 10, 1))

# train the model
randomForest_pl <- train(leaf_length_cm ~ leaf_width_cm + predLength + mean_width_cm +
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

harvest_test$leaf_length_pred <- predict(randomForest_pl, newdata = harvest_test)

# plot predicted vs. actual
plot(leaf_length_pred ~ leaf_length_cm, data = harvest_test); abline(c(0,1))


# test RMSE, MAD, MBE
sqrt(mean((harvest_test$leaf_length_cm - harvest_test$leaf_length_pred)^2)) # 14.5, 9.1, 8.1
mean(abs(harvest_test$leaf_length_cm - harvest_test$leaf_length_pred)) # 10.5, 7.7, 6.8
mean(harvest_test$leaf_length_cm - harvest_test$leaf_length_pred) # -2.4, 0.7, 1.1


# add predictions to harvest data
harvest$predLength <- predict(randomForest_pl, newdata = harvest)
