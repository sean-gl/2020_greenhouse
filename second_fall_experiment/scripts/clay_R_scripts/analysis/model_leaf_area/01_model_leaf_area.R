

# rm(list = ls())

require(readODS); require(ggplot2); require(plyr); require(ranger)
require(randomForest); require(tdr); require(reshape2); require(tibble); require(caret)



# Part 1: Prepare Harvest plant data for modeling -------------------------



# read in destructive harvest data (has leaf length, width and areas)
# these are all border plants (except on final day, 12/12)
harvest <- read_ods('/home/sean/github/2020_greenhouse/second_fall_experiment/data/destructive_harvest_data/read_only/destructive_harvest.ods',
                    sheet = 'leaf_measurements', col_names = T)

# remove empty rows
i <- apply(harvest, 1, function(x) all(is.na(x)))
harvest <- harvest[!i,]

# remove rows without leaf area or leaf width
i <- is.na(harvest$leaf_area_cm2) | is.na(harvest$leaf_width_cm)
harvest <- harvest[!i,]

# convert date to date
harvest$date <- as.Date(harvest$date, format = '%m/%d/%y')

# convert measurement cols to numeric
for(col in c('leaf_length_cm','leaf_area_cm2','leaf_width_cm','leaf_id','percent_expanded','percent_dead')) {
  harvest[[col]] <- as.numeric(harvest[[col]])
}

# keep relevant columns only
harvest <- subset(harvest, select = c(date, treatment, pot_id, leaf_id,
                                      leaf_length_cm, leaf_width_cm, leaf_area_cm2,
                                      percent_expanded, percent_dead))

# percent_expanded: assume 100 when missing
ind <- which(is.na(harvest$percent_expanded)); ind

# percent_dead: assume zero when missing
ind <- which(is.na(harvest$percent_dead)); ind
harvest$percent_dead[ind] <- 0

# perfect, no missing data!
ind <- which(!complete.cases(harvest)); ind



### Added Variables

# add adjusted_leaf_area, which accounts for percent_dead 
# NOTE: I do NOT adjust for % expanded; I deal with this in the modelling code.
harvest$leaf_area_cm2_adjusted <- harvest$leaf_area_cm2 * (1 - harvest$percent_dead/100)


# leaf_id needs to be reclassified as "leaf_order", to be relative to smallest within-plant number
u <- unique(harvest[,c('date','pot_id')])
for(i in 1:nrow(u)) {
  ind <- harvest$date == u$date[i] & harvest$pot_id == u$pot_id[i]
  harvest$leaf_order[ind] <- harvest$leaf_id[ind] - min(harvest$leaf_id[ind])
  harvest$leaf_order_reverse[ind] <- max(harvest$leaf_id[ind]) - harvest$leaf_id[ind]
}
table(harvest$leaf_id)
table(harvest$leaf_order); table(harvest$leaf_order_reverse)


# add days since planting
harvest$days_since_planting <- harvest$date - as.Date('2019-09-09')

# add column for mean leaf width of entire plant
for(i in unique(harvest$pot_id)) {
  ind <- harvest$pot_id == i
  harvest$mean_width_cm[ind] <- mean(harvest$leaf_width_cm[ind])
}


# Save data
saveRDS(harvest, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_prepped.rds')



# Part 2: Random Forest Modeling ------------------------------------------


# rename data 
widthData <- harvest; rm(harvest)


### 1. First, Random Forest to predict leaf length from leaf width.

## Exclude leaves < 90% expanded from training data
widthData <- widthData[widthData$percent_expanded >= 90,]



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
saveRDS(rf_length, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_length_RF_model.rds')

# read back in
# rf_length <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_length_RF_model.rds')

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
saveRDS(rf_area, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_RF_model.rds')

# read back in
# rf_area <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_RF_model.rds')

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
saveRDS(widthData, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/harvest_LA_pred.rds')



# Part 3: Non-harvest plant data prep -------------------------------------



# read in destructive non_harvest data (has leaf length, width and areas)
# these are all border plants (except on final day, 12/12)
non_harvest <- read_ods('/home/sean/github/2020_greenhouse/second_fall_experiment/data/leaf_width_measurements_non_harvest/read_only/leaf_width_measurements.ods',
                        col_names = T)

# remove empty rows
i <- apply(non_harvest, 1, function(x) all(is.na(x)))
non_harvest <- non_harvest[!i,]

# change leaf_id to numeric
non_harvest$leaf_id <- as.numeric(non_harvest$leaf_id)

# convert date to date
non_harvest$date <- as.Date(non_harvest$date, format = '%m/%d/%y')

# convert measurement cols to numeric
non_harvest$leaf_width_cm <- as.numeric(non_harvest$leaf_width_cm)

# keep relevant columns only
non_harvest <- subset(non_harvest, select = c(date, treatment, pot_id, leaf_id, leaf_width_cm,
                                              percent_dead))

# percent_dead: assume zero when missing
ind <- which(is.na(non_harvest$percent_dead))
non_harvest$percent_dead[ind] <- 0

# examine missing data
ind <- which(!complete.cases(non_harvest)); ind

# complete cases required for random forest
non_harvest <- non_harvest[complete.cases(non_harvest),]


### Added Variables

# leaf_id needs to be reclassified as "leaf_order", to be relative to smallest within-plant number
u <- unique(non_harvest[,c('date','pot_id')])
for(i in 1:nrow(u)) {
  ind <- non_harvest$date == u$date[i] & non_harvest$pot_id == u$pot_id[i]
  non_harvest$leaf_order[ind] <- non_harvest$leaf_id[ind] - min(non_harvest$leaf_id[ind])
  non_harvest$leaf_order_reverse[ind] <- max(non_harvest$leaf_id[ind]) - non_harvest$leaf_id[ind]
}
table(non_harvest$leaf_order)
table(non_harvest$leaf_order_reverse)


# change date to factor for plotting
# u <- unique(non_harvest$date); u
# non_harvest$date_num <- match(non_harvest$date, u) 
non_harvest$days_since_planting <- non_harvest$date - as.Date('2019-09-09')

# add column for mean leaf width of entire plant
for(i in unique(non_harvest$pot_id)) {
  ind <- non_harvest$pot_id == i
  non_harvest$mean_width_cm[ind] <- mean(non_harvest$leaf_width_cm[ind])
}

# Save data
saveRDS(non_harvest, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_nonharvest_prepped.rds')


# ------------- TODO: Add rest of code from 03_prep_nonharvest. R ======


