# Clay Bliss
# Script to develop model of leaf area based on non-plant measurement data

rm(list=ls())
require(ggplot2)
require(plyr)
require(lubridate)
require(tidyr)
require(dplyr)

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
exp2LeafAreaModel <- lm(total_leaf_area_m2 ~ mgdd_cumsum + treatment, la)



# Predict Leaf Area for 1st Experiment, using Linear Model from 2nd Experiment ------------------------------------

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

## Using MGDD instead of GDD seems to give more reasonable predictions.
laExp1$total_leaf_area_m2 <- predict(exp2LeafAreaModel, newdata = laExp1)

plot(laExp1$date, laExp1$total_leaf_area_m2, col=laExp1$treatment)
