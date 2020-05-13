

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
harvest$days_since_planting <- as.numeric(harvest$date - as.Date('2019-09-09'))

# add column for mean leaf width of entire plant
for(i in unique(harvest$pot_id)) {
  ind <- harvest$pot_id == i
  harvest$mean_width_cm[ind] <- mean(harvest$leaf_width_cm[ind])
}


# Save data
saveRDS(harvest, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_prepped.rds')

# read back in 
# harvest <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_prepped.rds')

# Part 2: Random Forest Modeling ------------------------------------------



### 1. First, Random Forest to predict leaf length from leaf width.


# rename data before modeling
widthData <- harvest

## For modeling, exclude leaves < 90% expanded from training data
# (this is done becuase all of the out-of-sample (non-harvest) data were sampled
# after all leaves were fully expanded.)
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
# note: 'extratrees' splitrule seems consistenly the best.
tuneGrid = expand.grid(mtry = 1:5,
                       splitrule = 'extratrees',
                       # splitrule = c('variance', 'extratrees', 'maxstat'),
                       min.node.size = 1:10)

# train the model
# predictors: leaf_width_cm, leaf_order, leaf_order_reverse, days_since_planting,
# mean_width_cm
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
# again, extratrees' splitrule seems consistenly the best.
tuneGrid = expand.grid(mtry = 1:6,
                       splitrule = 'extratrees',
                       # splitrule = c('variance', 'extratrees', 'maxstat'),
                       min.node.size = 1:10)

# train the model
# predictors: leaf_width_cm, leaf_order, leaf_order_reverse, days_since_planting,
# mean_width_cm
rf_area <- train(leaf_area_cm2 ~ leaf_length_pred + leaf_width_cm + leaf_order + leaf_order_reverse +
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
res[res$RMSE == min(res$RMSE),] # rmse = 48.1, r2=.95
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



# Part 3: Non-harvest plant data prep, leaf length/area predictions -------------------------------------



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
non_harvest$days_since_planting <- as.numeric(non_harvest$date - as.Date('2019-09-09'))

# add column for mean leaf width of entire plant
for(i in unique(non_harvest$pot_id)) {
  ind <- non_harvest$pot_id == i
  non_harvest$mean_width_cm[ind] <- mean(non_harvest$leaf_width_cm[ind])
}

# Save data
saveRDS(non_harvest, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_nonharvest_prepped.rds')

# read back in
# non_harvest <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_nonharvest_prepped.rds')


# Predict leaf length on non-harvest data
non_harvest$leaf_length_pred <- predict(rf_length, newdata = non_harvest)
summary(non_harvest$leaf_length_pred)


# Predict leaf area on non-harvest data
non_harvest$leaf_area_pred <- predict(rf_area, newdata = non_harvest)
summary(non_harvest$leaf_area_pred)




# Part 4: Linear-interpolation of leaf area between sampling dates --------



# Combine harvest and non-harvest data + predicitons
# use the measured leaf area for the harvest plants, and the predicted leaf area 
# for the non-harvest plants

# first, for non-harvest, adjust leaf by subtracting out dead leaf area
# (this was already done in harvest plants)
non_harvest$leaf_area_cm2_adjusted <- non_harvest$leaf_area_pred * (1 - non_harvest$percent_dead/100)


# read harvest data (including leaves < 90% expanded) back in 
harvest <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_prepped.rds')


# Prepare data sets to merge
harvest <- harvest[,names(harvest)[names(harvest) %in% names(non_harvest)]]
non_harvest <- non_harvest[,names(non_harvest)[names(non_harvest) %in% names(harvest)]]

# add id column
harvest$isHarvest <- 'y'
non_harvest$isHarvest <- 'n'

# combine harvest, nonharvest
dat <- rbind(harvest, non_harvest)

# add block (note, block "V" doesn't match other datasets but we'll fix this below...)
dat$block[grep('w-', dat$pot_id)] <- 'W'
dat$block[grep('m-', dat$pot_id)] <- 'M'
dat$block[grep('d-', dat$pot_id)] <- 'D'
dat$block[dat$pot_id %in% c('w-25','w-26','w-27','w-28')] <- 'V'
table(dat$block, useNA = 'always')
table(dat$block, dat$treatment)


# Calculate plant leaf area by plant and date
totalArea <- ddply(dat, .(date, pot_id, treatment, block), function(x) {
  setNames(sum(x$leaf_area_cm2_adjusted), 'total_leaf_area_cm2')
})

# Plot of plant leaf area by date
# ggplot(totalArea, aes(x=date, y=total_leaf_area_cm2, color=block)) + 
#   geom_point() 

# Save this data (I will use it to try to build a LA model using environmental variables only.)
saveRDS(totalArea, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/total_plant_leaf_area.rds')


# Calculate mean for each block/date, of plant leaf area 
meanArea <- ddply(totalArea, .(date, treatment, block), function(x) {
  setNames(mean(x$total_leaf_area_cm2), 'total_leaf_area_cm2_block_mean')
})

# Plot of mean plant leaf area by block/date
# ggplot(meanArea, aes(x=date, y=total_leaf_area_cm2_block_mean, color=block)) + 
#   geom_line() + geom_point() 



# Function to calculate piecewise-linear coefficiencts
# (fitting a straight line betwen each point in the plot above)
pw_linear_coef <- ddply(meanArea, .(date, block), function(x) {
  # print(x)
  dates <- sort(unique(meanArea$date[meanArea$block == x$block]))
  if(x$date == '2019-12-12') {
    return(rep(NA,2))
  } else {
    # calculate intercept & slope between each pair of points
    p1 <- c(as.numeric(x$date), x$total_leaf_area_cm2_block_mean)
    nextdate <- dates[which(dates == x$date) + 1]
    x2 <- subset(meanArea, block == x$block & date == nextdate)
    p2 <- c(as.numeric(x2$date), x2$total_leaf_area_cm2_block_mean)
    d <- data.frame(rbind(p1, p2))
    m <- lm(X2~X1, d) 
    return(as.numeric(m$coefficients))
  }
})

names(pw_linear_coef)[names(pw_linear_coef) %in% c('V1','V2')] <- c('b0','b1')

# Create grid of dates and blocks; this will be used to predict the daily leaf area
grid <- expand.grid(date = seq.Date(from = as.Date('2019-10-24'), to = as.Date('2019-12-12'), by = 1),
                    block = c('W','M','D','V'))

head(pw_linear_coef)
sdates <- sort(unique(meanArea$date))
pw_linear_coef$period <- match(pw_linear_coef$date, sdates)

# replace 11/15 with 12/12 (to extend lines out for non-virgin block)
pw_linear_coef$date[pw_linear_coef$date=='2019-11-15'] <- '2019-12-12'

# define periods between sampling dates
grid$period <- NA
grid$period[grid$date < sdates[1]] <- 0
grid$period[grid$date >= sdates[1] & grid$date < sdates[2]] <- 1
grid$period[grid$date >= sdates[2] & grid$date < sdates[3]] <- 2
grid$period[grid$date >= sdates[3] & grid$date <= sdates[5]] <- 3
grid$period[grid$block == 'V' & grid$date >= sdates[4] & grid$date <= sdates[5]] <- 4
table(grid$period, useNA = 'always')


# Fill in the grid with straight-line interpolations, via some ninja shit
z <- dlply(grid, .(block, period), function(x) {
  if(unique(x$period)==0) return(rep(NA, nrow(x)))
  ind <- pw_linear_coef$period == unique(x$period) & pw_linear_coef$block == unique(x$block)
  if(any(ind)) {
    coef <- pw_linear_coef[ind, c('b0','b1')]
    LA_pred <- coef$b0 + coef$b1 * as.numeric(x$date)
    return(LA_pred)
  } else {
    return(rep(NA, nrow(x)))
  }
})
grid$leaf_area_pred <- as.numeric(unlist(z))


# plot interpolations
ggplot(grid, aes(x=date, y=leaf_area_pred, color=block)) + geom_point()



# Save the predictions as-is (block doesn't match other data sets for 'virgin' period)
# remove period, not useful
# out <- grid
# out$period <- NULL
# saveRDS(out, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/continuous_LA_pred_raw.rds')



# ---Change blocks to match other Data set blocks...
# NOTE: block V (12/6-12/12) corresonds to block W in other data sets, so let's change that here.
# Also, the "W" block plants for these same dates don't correspond to any scale data.
# NOTE: To avoid overlap on day treatment changed (11/27), I'm going to use the leaf area from the previous W block plants
# on this day and not the virgin plants
out_final <- subset(grid, !(block=='V' & date < '2019-11-28'))
out_final <- subset(out_final, !(block=='W' & date > '2019-11-27'))
out_final[out_final$block=='V', 'block'] <- 'W'
colSums(table(out_final$date, out_final$block))
# See, now the W block plants are virgins after 11/27 and non-virgins before
ggplot(out_final, aes(x=date, y=leaf_area_pred, color=block)) + geom_point()


# extend (flat) lines out to periods before plants were measured...
ind <- out_final$block=='W' & out_final$date >='2019-11-28' & out_final$date <= '2019-12-05'
out_final$leaf_area_pred[ind] <- out_final$leaf_area_pred[out_final$block == 'W' & out_final$date == '2019-12-06']

for(b in unique(out_final$block)) {
  ind <- out_final$date <= '2019-10-29' & out_final$block == b
  out_final$leaf_area_pred[ind] <- out_final$leaf_area_pred[out_final$block == b & out_final$date == '2019-10-30']
}

# plot the complete dataset
ggplot(out_final, aes(x=date, y=leaf_area_pred, color=block)) + geom_point()

# rename column
names(out_final)[names(out_final) == 'leaf_area_pred'] <- 'mean_plant_leaf_area_cm2'

# save the data to be used in analysis
saveRDS(out_final, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/continuous_LA_pred_for_analysis.rds')
