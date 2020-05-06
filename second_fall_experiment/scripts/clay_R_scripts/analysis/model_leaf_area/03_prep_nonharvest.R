rm(list = ls())

require(readODS); require(ggplot2); require(plyr); require(ranger); require(caret)

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

# Load Random Forest (leaf length) model
rfmod_length <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_length_RF_model.rds')

# Predict on non-harvest data
non_harvest$leaf_length_pred <- predict(rfmod_length, newdata = non_harvest)
summary(non_harvest$leaf_length_pred)


# Load Random Forest (leaf area) model
rfmod_area <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_RF_model.rds')

# Predict on non-harvest data
non_harvest$leaf_area_pred <- predict(rfmod_area, newdata = non_harvest)
summary(non_harvest$leaf_area_pred)

# Read in harvest data (plus predicted LA)
harvest <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/harvest_LA_pred.rds')

# Combine harvest and non-harvest data + predicitons
# use the measured leaf area for the harvest plants, and the predicted leaf area 
# for the non-harvest plants

# first, for non-harvest, adjust leaf by subtracting out dead leaf area
# (this was already done in harvest plants)
non_harvest$leaf_area_cm2_adjusted <- non_harvest$leaf_area_pred * (1 - non_harvest$percent_dead/100)

harvest <- harvest[,names(harvest)[names(harvest) %in% names(non_harvest)]]
non_harvest <- non_harvest[,names(non_harvest)[names(non_harvest) %in% names(harvest)]]

# add id column
harvest$isHarvest <- 'y'
non_harvest$isHarvest <- 'n'

dat <- rbind(harvest, non_harvest)

# add block
dat$block[grep('w-', dat$pot_id)] <- 'W'
dat$block[grep('m-', dat$pot_id)] <- 'M'
dat$block[grep('d-', dat$pot_id)] <- 'D'
dat$block[dat$pot_id %in% c('w-25','w-26','w-27','w-28')] <- 'V'
table(dat$block, useNA = 'always')
table(dat$block, dat$treatment)




# # Calculate plant leaf area totals by plant and date
# totalArea <- ddply(dat, .(isHarvest, date, pot_id, treatment, block), function(x) {
#   setNames(sum(x$leaf_area_cm2_adjusted), 'total_leaf_area_cm2')
# })
# 
# ggplot(totalArea, aes(x=date, y=total_leaf_area_cm2, color=block)) + 
#   geom_point() + facet_wrap(~isHarvest)
# 
# # Calculate plant leaf area totals by block and date
# meanArea <- ddply(totalArea, .(isHarvest, date, treatment, block), function(x) {
#   setNames(mean(x$total_leaf_area_cm2), 'total_leaf_area_cm2_block_mean')
# })
# 
# ggplot(meanArea, aes(x=date, y=total_leaf_area_cm2_block_mean, color=block)) + 
#   geom_line() + geom_point() + facet_wrap(~isHarvest)


# ---- try again without isHarvest

 
# Calculate plant leaf area totals by plant and date
totalArea <- ddply(dat, .(date, pot_id, treatment, block), function(x) {
  setNames(sum(x$leaf_area_cm2_adjusted), 'total_leaf_area_cm2')
})

ggplot(totalArea, aes(x=date, y=total_leaf_area_cm2, color=block)) + 
  geom_point() 

# Save this data (I will use it to try to build a LA model using environmental variables only.)
saveRDS(totalArea, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/total_plant_leaf_area.rds')


# Calculate mean for each block/date, of total plant leaf area 
meanArea <- ddply(totalArea, .(date, treatment, block), function(x) {
  setNames(mean(x$total_leaf_area_cm2), 'total_leaf_area_cm2_block_mean')
})

ggplot(meanArea, aes(x=date, y=total_leaf_area_cm2_block_mean, color=block)) + 
  geom_line() + geom_point() 


# Function to calculate piecewise-linear coefficiencts
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
# pw_linear_coef <- pw_linear_coef[complete.cases(pw_linear_coef),]
names(pw_linear_coef)[names(pw_linear_coef) %in% c('V1','V2')] <- c('b0','b1')

# Create grid of dates and blocks
grid <- expand.grid(date = seq.Date(from = as.Date('2019-10-24'), to = as.Date('2019-12-12'), by = 1),
                    block = c('W','M','D','V'))

head(pw_linear_coef)
sdates <- sort(unique(meanArea$date))
pw_linear_coef$period <- match(pw_linear_coef$date, sdates)

# replace 11/15 with 12/12 (to extend lines out for non-virgin block)
pw_linear_coef$date[pw_linear_coef$date=='2019-11-15'] <- '2019-12-12'

grid$period <- NA
grid$period[grid$date < sdates[1]] <- 0
grid$period[grid$date >= sdates[1] & grid$date < sdates[2]] <- 1
grid$period[grid$date >= sdates[2] & grid$date < sdates[3]] <- 2
grid$period[grid$date >= sdates[3] & grid$date <= sdates[5]] <- 3
grid$period[grid$block == 'V' & grid$date >= sdates[4] & grid$date <= sdates[5]] <- 4
table(grid$period, useNA = 'always')


z = dlply(grid, .(block, period), function(x) {
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


# plot predictions
ggplot(grid, aes(x=date, y=leaf_area_pred, color=block)) + geom_point()


# merge datasets to check
# m <- merge(subset(grid, select = -c(period)),
#            subset(meanArea,  select = -c(period)),
#            by = c('date','block'), all.x = T)

# Save the predictions as-is (block doesn't match other data sets for 'virgin' period)
# remove period, not useful
out <- grid
out$period <- NULL
saveRDS(out, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/continuous_LA_pred_raw.rds')

# ---Change blocks to match other Data set blocks...
# NOTE: block V (12/6-12/12) corresonds to block W in other data sets, so let's change that here.
# Also, the "W" block plants for these same dates don't correspond to any scale data.
# NOTE: To avoid overlap on day treatment changed (11/27), I'm going to use the leaf area from the previous W block plants
# on this day and not the virgin plants
out_final <- subset(out, !(block=='V' & date < '2019-11-28'))
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
