rm(list = ls())

require(readODS); require(ggplot2); require(plyr); require(ranger); require(caret)

# read in destructive harvest data (has leaf length, width and areas)
# these are all border plants (except on final day, 12/12)
harvest <- read_ods('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/destructive_harvest_data/destructive_harvest.ods',
                    sheet = 'leaf_measurements', col_names = T)

# remove empty rows
i <- apply(harvest, 1, function(x) all(is.na(x)))
harvest <- harvest[!i,]

# change leaf_id to numeric
# NOTE: NAs are introduced here, some leaves numbers weren't recorded....
table(harvest$leaf_id)

# TODO: this plant didn't mark leaves but at least 2 can be determined...
# doesn't matter though, since no area or width recorded anyway.
ind <- which(harvest$pot_id=='w-10')
harvest$leaf_id[ind]
harvest$leaf_id <- as.numeric(harvest$leaf_id)

# convert date to date
harvest$date <- as.Date(harvest$date, format = '%m/%d/%y')

# convert measurement cols to numeric
harvest$leaf_length_cm <- as.numeric(harvest$leaf_length_cm)
harvest$leaf_area_cm2 <- as.numeric(harvest$leaf_area_cm2)
harvest$leaf_width_cm <- as.numeric(harvest$leaf_width_cm)

# keep relevant columns only
harvest <- subset(harvest, select = c(date, treatment, pot_id, leaf_id, leaf_length_cm, leaf_width_cm, leaf_area_cm2))

# examine missing data
ind <- which(!complete.cases(harvest)); ind
# View(harvest[ind,])
# w-2, leaves 6,7 should be excluded, they had pressure bomb portion of leaves not measured (see notes)
# ditto with m-10 leaf 8

# complete cases required for random forest
harvest <- harvest[complete.cases(harvest),]


### Added Variables

# leaf_id needs to be reclassified as "leaf_order", to be relative to smallest within-plant number
u <- unique(harvest[,c('date','pot_id')])
for(i in 1:nrow(u)) {
  ind <- harvest$date == u$date[i] & harvest$pot_id == u$pot_id[i]
  harvest$leaf_order[ind] <- harvest$leaf_id[ind] - min(harvest$leaf_id[ind])
  harvest$leaf_order_reverse[ind] <- max(harvest$leaf_id[ind]) - harvest$leaf_id[ind]
}
# table(harvest$leaf_id)
# table(harvest$leaf_order); table(harvest$leaf_order_reverse)


# change date to factor for plotting
# u <- unique(harvest$date); u
# harvest$date_num <- match(harvest$date, u) 
harvest$days_since_planting <- harvest$date - as.Date('2019-09-09')

# add column for mean leaf width of entire plant
for(i in unique(harvest$pot_id)) {
  ind <- harvest$pot_id == i
  harvest$mean_width_cm[ind] <- mean(harvest$leaf_width_cm[ind])
}


### Add cummulative mean irrigation (since treatment start on 10/24)
start <- as.Date('2019-10-24')
t2 <- as.Date('2019-11-04')

day1 <- as.Date('2019-10-30')
ind <- harvest$date == day1 & harvest$treatment == 'wet'
harvest$irrig_cummean[ind] <- 750
ind <- harvest$date == day1 & harvest$treatment == 'dry'
harvest$irrig_cummean[ind] <- 150
ind <- harvest$date == day1 & harvest$treatment == 'moderate'
harvest$irrig_cummean[ind] <- 375

day2 <- as.Date('2019-11-15')
ind <- harvest$date == day2 & harvest$treatment == 'wet' # was dry until 11-04
w1 = rep(150, as.integer(t2 - start))
w2 = rep(1000, as.integer(day2 - t2))
w = c(w1, w2); length(w)
harvest$irrig_cummean[ind] <- mean(c(w1, w2))
ind <- harvest$date == day2 & harvest$treatment == 'dry' # was wet until 11-04
w1 = rep(750, as.integer(day1 - start))
w2 = rep(1000, as.integer(t2 - day1))
w3 = rep(150, as.integer(day2 - t2))
w = c(w1, w2, w3); length(w)
harvest$irrig_cummean[ind] <- mean(w)
ind <- harvest$date == day2 & harvest$treatment == 'moderate'
harvest$irrig_cummean[ind] <- 375

day3 <- as.Date('2019-12-12')
ind <- harvest$date == day3 & harvest$treatment == 'wet' # was dry until 11-04
w1 = rep(150, as.integer(t2 - start))
w2 = rep(1000, as.integer(day3 - t2))
w = c(w1, w2); length(w)
harvest$irrig_cummean[ind] <- mean(c(w1, w2))
ind <- harvest$date == day3 & harvest$treatment == 'dry' # was wet until 11-04
w1 = rep(750, as.integer(day1 - start))
w2 = rep(1000, as.integer(t2 - day1))
w3 = rep(150, as.integer(day3 - t2))
w = c(w1, w2, w3); length(w)
harvest$irrig_cummean[ind] <- mean(w)
ind <- harvest$date == day3 & harvest$treatment == 'moderate'
harvest$irrig_cummean[ind] <- 375

# Save data
saveRDS(harvest, '/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_prepped.rds')
