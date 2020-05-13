# Clay Bliss
# Script to develop model of leaf area based on non-plant measurement data

rm(list=ls())
require(ggplot2)
require(plyr)
require(lubridate)
require(tidyr)
require(dplyr)
require(readODS)

### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')

# read in Argus (greenhouse system) temperature/irradiance data
argus <- read.csv('/home/sean/github/2020_greenhouse/A3 temp light 2019.csv', stringsAsFactors = F)
colnames(argus) <- c('by15', 'Argus_temp_C', 'Argus_irradiance_W_m2')
argus$by15 <- gsub(',', '', argus$by15)
argus$by15 <- as.POSIXct(argus$by15, format='%Y/%m/%d %H:%M:%S', tz='GMT')
argus$date <- date(argus$by15)
# convert F to C
argus$Argus_temp_C <- (argus$Argus_temp_C - 32) * 5/9

# sort and check for duplicate rows
argus <- argus[order(argus$by15), ]
dups <- duplicated(argus)
argus <- argus[!dups, ]

# omit 1 row without data
ind <- which(!complete.cases(argus)); ind
argus <- argus[-ind, ]

# TODO: Note odd upward-shift in temps. 
plot(argus$by15, argus$Argus_temp_C, type = 'p')
sub = subset(argus, date > '2019-09-05' & date < '2019-09-11')
plot(sub$by15, sub$Argus_temp_C)
# These are on days in-between experiments, so we can ignore this luckily!


# rename data
tempDat <- argus

# calculate daily max/min temperatures at 75% canopy height ("low") and ~1m above canopy("high")
tempSummary <- tempDat %>% 
  group_by(date) %>% 
  summarise(max_temp = max(Argus_temp_C), min_temp = min(Argus_temp_C))

# now calculate simple mean temp using daily max/mean
tempSummary$mean_temp <- rowMeans(tempSummary[,c('max_temp','min_temp')])

# calculate GDD (note: T_base = 50 F = 10 C)
tempSummary$gdd <- ifelse(tempSummary$max_temp > 10, tempSummary$mean_temp - 10, 0)

# calculate modified GDD
tempSummary$max_temp_mod <- ifelse(tempSummary$max_temp > 30, 30, tempSummary$max_temp)
tempSummary$min_temp_mod <- ifelse(tempSummary$min_temp < 10, 10, tempSummary$min_temp)
tempSummary$mean_temp_mod <- rowMeans(tempSummary[,c('max_temp_mod','min_temp_mod')])
tempSummary$mgdd <- ifelse(tempSummary$max_temp_mod > 10, tempSummary$mean_temp_mod - 10, 0)
summary(tempSummary)
plot(tempSummary$date, tempSummary$gdd, type='l')
lines(tempSummary$date, tempSummary$mgdd, type='l', col='red')



# Leaf Area Modeling ------------------------------------------------------


# Subset to dates after planting in 2nd experiment, to use in fitting leaf area model.
tempSummaryExp2 <- subset(tempSummary, date >= '2019-09-09', select = c(date, gdd, mgdd))

# add cumsum columns
tempSummaryExp2$gdd_cumsum <- cumsum(tempSummaryExp2$gdd)
tempSummaryExp2$mgdd_cumsum <- cumsum(tempSummaryExp2$mgdd)


# Read in leaf area data from 2nd experiment (prepped). 
# These include both data on dates which leaf area was directly measrured,
# or when leaf width/length were measured (and leaf area predicted from them.)
la <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/total_plant_leaf_area.rds')

# drop virgin plants, they have no analoge in 1st experiment
la <- la[la$block!='V',]

# Subset leaf area to roughly the same # of days since planting as in 1st experiment
as.Date('2019-09-03') - as.Date('2019-07-11') # 55 days in 1st exp.
la$day <- as.numeric(la$date - as.Date('2019-09-09')) + 1

# I'll use the first 2 leaf area dates (the first 59 days) to model leaf area growth in each treatment
la <- la[la$day <= 59, ]


# For 1st 2 measurements only! Ignore 2nd treatment since it had no time to take effect.
# change treatment to contrast full_drought vs. moderate/no drought.
# (if we keep all 3 treatments, there are no significant diffs between them in regression.)
la$treatment[la$block=='D'] <- 'dry'
la$treatment[la$block!='D'] <- 'wet'

# change to m2
la$total_leaf_area_m2 <- la$total_leaf_area_cm2 / 1e4
la$total_leaf_area_cm2 <- NULL


ggplot(la, aes(x=day, y=total_leaf_area_m2, color = block)) + geom_point() +
  geom_smooth(method = 'lm')


# merge ;leaf area data to temp summary
la <- merge(la, tempSummaryExp2, by='date', all.x = TRUE)


# Finally, fit a linear model to experiment 2 data
summary(lm(total_leaf_area_m2 ~ day, la))
summary(lm(total_leaf_area_m2 ~ gdd_cumsum, la))
summary(lm(total_leaf_area_m2 ~ gdd_cumsum + treatment, la))

# Some notes: It doesn't matter if we use day or gdd_cumsum (model fit is same). 
# However, let's use gdd_cumsum, since we want to use this model on experiment 1 and this seems more justifiable,
# esp. since the GDD were greater earlier in the summer (in experiment 1).
# Also, adding a treatment variable increases the R2 by 0.14 so it seems worth including. 
# However, this means we cannot we are lumping well-watered and moderate-drought together, so keep that in mind below...

# Save the best model to use below.
# can use either GDD or day, doesn't seem to matter in terms of fitting with measured leaves below.
exp2LeafAreaModel <- lm(total_leaf_area_m2 ~ gdd_cumsum + treatment, la)
summary(exp2LeafAreaModel)


# Predict Leaf Area for 1st Experiment, using Linear Model from 2nd Experiment ------------------------------------
# NOTE: This is a temporal model, modeling change in leaf area over time in each block.


# Subset to dates after planting in 2nd experiment, to use in fitting leaf area model.
laExp1 <- subset(tempSummary, date >= '2019-07-11' & date <= '2019-09-03', select = c(date, gdd, mgdd))

# add cumsum columns
laExp1$gdd_cumsum <- cumsum(laExp1$gdd)
laExp1$mgdd_cumsum <- cumsum(laExp1$mgdd)

# add treatment column
laExp1_wet <- laExp1_dry <- laExp1
laExp1_wet$treatment <- 'wet'
laExp1_dry$treatment <- 'dry'
laExp1 <- rbind(laExp1_wet, laExp1_dry)
laExp1 <- laExp1[order(laExp1$date),]
laExp1$treatment <- as.factor(laExp1$treatment)

# Add predicted leaf area to dataframe
laExp1$total_leaf_area_m2 <- predict(exp2LeafAreaModel, newdata = laExp1)
plot(laExp1$date, laExp1$total_leaf_area_m2, col=laExp1$treatment)



# Use Random Forest Leaf Length/Width Model (non-temporal model) from 2nd Experiment to 
# Predict Leaf Area from 1st Experiment Leaf Measurements -----------


# get leaf length/width data from 1st experiment (4 plants only)
leafMeasExp1 <- readODS::read_ods('/home/sean/github/2020_greenhouse/first_summer_experiment/data/experiment1_nondestructive_leaf_measurements.ods')

# rename treatments to match above model
leafMeasExp1$treatment[leafMeasExp1$treatment=='well_watered'] <- 'wet' 
leafMeasExp1$treatment[leafMeasExp1$treatment=='full_drought'] <- 'dry' 
leafMeasExp1$treatment <- as.factor(leafMeasExp1$treatment)

# convert numeric variables
leafMeasExp1$leaf_id <- as.numeric(leafMeasExp1$leaf_id)
# NOTE: NAs introduced here are due to missing measurments.
leafMeasExp1$leaf_length_cm <- as.numeric(leafMeasExp1$leaf_length_cm)
leafMeasExp1$leaf_width_cm <- as.numeric(leafMeasExp1$leaf_width_cm)
leafMeasExp1$percent_dead <- as.numeric(leafMeasExp1$percent_dead)

# Get random forest models from Experiment 2:
# model 1: use leaf width (and other vars.) to predict leaf length
rfMod_length <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_length_RF_model.rds')
# model 2: use leaf length (and other vars.) to predict leaf area
rfMod_area <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/leaf_area_RF_model.rds')


# Now, we measured leaf lenth in 1st experiment (so we can use these measurements to model area below)
# But, out of curiosity, let's see how well the 2nd Experiment model predicts leaf length from width etc.

# These are the variables we need, let's derive them below
rfMod_length$finalModel$variable.importance

# add days since planting
leafMeasExp1$days_since_planting <- as.numeric(as.Date('2019-08-22') - as.Date('2019-07-11'))

# leaf_id needs to be reclassified as "leaf_order", to be relative to smallest within-plant number
u <- unique(leafMeasExp1$plant_id)
for(i in unique(leafMeasExp1$plant_id)) {
  ind <- leafMeasExp1$plant_id == i
  leafMeasExp1$leaf_order[ind] <- leafMeasExp1$leaf_id[ind] - min(leafMeasExp1$leaf_id[ind])
  leafMeasExp1$leaf_order_reverse[ind] <- max(leafMeasExp1$leaf_id[ind]) - leafMeasExp1$leaf_id[ind]
}
table(leafMeasExp1$leaf_id)
table(leafMeasExp1$leaf_order); table(leafMeasExp1$leaf_order_reverse)

# add column for mean leaf width of entire plant
for(i in unique(leafMeasExp1$plant_id)) {
  ind <- leafMeasExp1$plant_id == i
  leafMeasExp1$mean_width_cm[ind] <- mean(leafMeasExp1$leaf_width_cm[ind], na.rm = T)
}

### Now, predict leaf length using width etc.
ind <- !is.na(leafMeasExp1$leaf_length_cm)
leafMeasExp1$leaf_length_cm_predicted[ind] <- predict(rfMod_length, newdata = leafMeasExp1[ind,])
summary(lm(leaf_length_cm_predicted ~ leaf_length_cm, leafMeasExp1))
sqrt(mean((leafMeasExp1$leaf_length_cm_predicted - leafMeasExp1$leaf_length_cm)^2, na.rm = T))
plot(leaf_length_cm_predicted ~ leaf_length_cm, leafMeasExp1); abline(c(0,1))
# pretty decent; R2=0.77 between predicted and measured length


# --- OK< moving on to important stuff....Let's now predict leaf area using measured length/width, etc.

# note: need to rename leaf_length column to match RF model
names(leafMeasExp1)[names(leafMeasExp1)=='leaf_length_cm'] <- 'leaf_length_pred'

# PREDICT THAT SHIZ!
leafMeasExp1$leaf_area_cm2_predicted[ind] <- predict(rfMod_area, newdata = leafMeasExp1[ind,])
names(leafMeasExp1)[names(leafMeasExp1)=='leaf_length_pred'] <- 'leaf_length_cm' # revert column name
summary(leafMeasExp1$leaf_area_cm2_predicted)


# add "adjusted leaf area" to account for % dead
leafMeasExp1$leaf_area_cm2_predicted_adjusted <- leafMeasExp1$leaf_area_cm2_predicted * (1 - leafMeasExp1$percent_dead/100)


# scale up to entire-plant leaf area and convert to m2
leafMeasExp1 %>% group_by(plant_id, treatment) %>% 
  summarise(total_leaf_area_m2_predicted_adjusted = sum(leaf_area_cm2_predicted_adjusted/1e4))


# What is leaf area using GDD-based model above, on same date as plants measured (8/22)?
laExp1[laExp1$date=='2019-08-22',]


# --- Model predictions seem to match ok enough, let's save the total leaf area predictions.

# subset to data of interest and during treatment period
laExp1_out <- subset(laExp1, date >= '2019-08-24', select=c('date','treatment','total_leaf_area_m2'))

# NOTE: We need to fix the treatments to match those in other datasets. 
# Other datasets have 3 treatments, this one has only 2.
laExp1_out_wet <- subset(laExp1_out, treatment=='wet')
laExp1_out_wet$treatment <- 'well_watered'
laExp1_out_moderate <- laExp1_out_wet
laExp1_out_moderate$treatment <- 'moderate_drought'
laExp1_out_dry<- subset(laExp1_out, treatment=='dry')
laExp1_out_dry$treatment <- 'full_drought'
laExp1_out <- rbind(laExp1_out_dry, laExp1_out_wet, laExp1_out_moderate)

# Save the output
saveRDS(laExp1_out, '/home/sean/github/2020_greenhouse/first_summer_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/plant_leaf_area_predictions.rds')
