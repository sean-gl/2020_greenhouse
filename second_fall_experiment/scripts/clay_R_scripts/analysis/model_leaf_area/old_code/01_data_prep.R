rm(list = ls())

require(readODS); require(ggplot2); require(plyr); require(ranger); require(caret)

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


