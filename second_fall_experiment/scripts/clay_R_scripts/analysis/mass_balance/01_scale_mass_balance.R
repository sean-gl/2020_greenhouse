rm(list=ls())
packages <- c('lubridate','plyr','ggplot2','car','readODS')
lapply(packages, require, character.only = TRUE)
Sys.setenv(tz='GMT')


# read in 15-minute aggregated (and flagged) balance data 
baldat <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_long_aggflag.rds')


### Code to estimate soil weight as continuous variable, to get at soil matric potential.
# 
# d <- subset(baldat, date=='2019-10-23' & flag == 0 & treatment == 'well_watered')
# d <- subset(baldat, date=='2019-10-23' & !(flag %in% c('irrig','man')) & treatment == 'well_watered')
# d <- subset(baldat, date >= '2019-10-24' & date <= '2019-11-26' & !grepl('border', plant_id))
# 
# 
# d <- subset(baldat,  date >= '2019-09-18' & date <= '2019-09-25' & block == 'D' & !grepl('border', plant_id))
# ggplot(d, aes(x=roundTime, y=mean_weight_kg, color=plant_id)) +  geom_line() + facet_grid(~block)

# daily mass at same time, 3 AM (3 hours after watering)
dm <- ddply(baldat, .(date, plant_id), function(x) {
  ind <- hour(x$roundTime)==3 # & minute(x$roundTime)==0
  if(length(ind[ind]) != 4) {
    out <- NA
  } else {
    out <- mean(x$mean_weight_kg[ind], na.rm = T)
  }
  return(setNames(out,'saturated_mass_kg'))
})
dm$saturated_mass_kg <- as.numeric(dm$saturated_mass_kg)

### Test method on a single plant...
sub = subset(dm, plant_id == 'W-6')
ggplot(sub, aes(x=date, y=saturated_mass_kg)) + geom_point() + geom_line() +
  scale_x_date(breaks = "1 week", date_labels = '%m-%d') + ggtitle(unique(sub$plant_id))

# add the end weight
# end wt = (measured plant wet weight) + (sat. pot/soil wt at experiment end) - (sat pot/soil wt at experiment start)

# W-6
mww = 0.5936
sww_end = 14.51 
sww_start = 13.9409
(end_wt = mww + sww_end - sww_start)
 
# Read in end-of-experiment saturated pot weights data
pot_wt <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/saturated_pot_weights.rds')

# Read in end-of-experiment plant/root wet weights
plant_wt <- read_ods('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/end_of_experiment_data.ods',
                     col_names = T)
plant_wt$plant_id <- toupper(plant_wt$plant_id)

# For each pot, exclude non-equilibrium dates,
# add final saturated weight, and fit trend line...


# create template dataframe (one per plant) to save predictions for each date
# these will be combined into a list at end, and then back into a single dataframe
pdat <- data.frame(date = seq.Date(min(baldat$date, na.rm = T),
                                   max(baldat$date, na.rm = T), by = 1))
pdat$date_num <- as.numeric(pdat$date - pdat$date[1]) + 1
pdat[,c('plant_id','pred_plant_weight_kg')] <- NA

# master list to save all plants predictions
plist <- list()

# --- W block plants


p = 'W-6'
y <- subset(baldat, plant_id == p & date == '2019-09-17')
ggplot(y, aes(x=roundTime, y=mean_weight_kg)) + geom_point()


x <- subset(dm, plant_id == p)
plot(x$saturated_mass_kg)
(start_wt <- x$saturated_mass_kg[!is.na(x$saturated_mass_kg)][1]) # data are sorted above
# x$saturated_mass_kg <- x$saturated_mass_kg - start_wt
(pot.wt <- pot_wt$pot_saturated_weight_kg[pot_wt$plant_id == p])
(plant.wt <- plant_wt$wet_weight_g[plant_wt$plant_id == p] / 1000) # wt in grams, convert to kg
# (root.wt <- plant_wt$wet_weight_roots_g[plant_wt$plant_id == p] / 1000) # wt in grams, convert to kg


# recalculate weights 
# try substracting the saturated pot weight at end (plus root wet weight) from time seriess...
x$new_wt <- x$saturated_mass_kg - pot.wt + root.wt 
plot(x$new_wt ~ x$date)

# end pot wt, minus roots
pot.wt - root.wt
start_wt - (pot.wt - plant.wt - root.wt)

# note: pot W-10 does not have a recorded ending pot weight, and W-11 has no recorded ending plant wet weight.
# So for the original wet treatment, we only have n=2 (W-6 and W-7)


###  CLAY UPDATED ON 4/10....THIS SHOULD NOW WORK WITH ANY PLANT, NOT JUST W BLOCK...
for(p in c('W-6','W-7')) {

  # subset data based on specific (saturated) dates, determined for each block...
  x <- switch(substr(p,1,1), # block letter
              'W' = subset(dm, plant_id == p & date <= '2019-11-04'),
              'M' = subset(dm, plant_id==p & (date <= '2019-10-24' | date >= '2019-12-01')),
              'D' = subset(dm, plant_id == p & (date <= '2019-10-24' | date >= '2019-11-11' & date <= '2019-11-27')))
  
  # check the plot
  ggplot(x, aes(x=date, y=saturated_mass_kg)) + geom_point() + ggtitle(p)
  
  # plants were a week old on 9-16 so assume the weight is zero on day 1
  # start_wt <- x$saturated_mass_kg[!is.na(x$saturated_mass_kg)][1] # data are sorted above
  # x$saturated_mass_kg <- x$saturated_mass_kg - start_wt
  pot.wt <- pot_wt$pot_saturated_weight_kg[pot_wt$plant_id == p]
  plant.wt <- plant_wt$wet_weight_g[plant_wt$plant_id == p] / 1000 # wt in grams, convert to kg
  end.plant.wt <- plant.wt + pot.wt #- start_wt
  x <- rbind(x, data.frame(date=as.Date('2019-12-13', format='%Y-%m-%d'),
                           plant_id=p, 
                           saturated_mass_kg=end.plant.wt))
  x$saturated_mass_kg <- as.numeric(x$saturated_mass_kg)
  ggplot(x, aes(x=date, y=saturated_mass_kg)) + geom_point() + geom_point() + ggtitle(p)
  # x$saturated_mass_kg[x$saturated_mass_kg < 0] <- NA
  
  # convert date to numeric for model fitting
  x$date_num <- as.numeric(x$date - pdat$date[1]) + 1
  
  ## logistic model fitting ----
  
  # fit a self-starting logistic curve
  # ssl <- nls(saturated_mass_kg ~ SSlogis(date_num, a, b, c), data = x) # 3-point doesn't work often.
  ssl <- nls(saturated_mass_kg ~ SSfpl(date_num, a, b, c, d), data = x) # 4-point seems to work better.
  
  summary(ssl)
  xv <- seq(0, x$date_num[length(x$date_num)], 1)
  yv <- predict(ssl, newdata = list(date_num = xv))
  yv <- as.numeric(yv)
  plot(x$date_num, x$saturated_mass_kg)
  lines(xv, yv)
  
  # save predicted weights to master dataframe
  preds <- pdat
  preds$plant_id <- p
  preds$pred_plant_weight_kg <- as.numeric(predict(ssl, newdata = list(date_num = preds$date_num)))
  # plot(x$date_num, x$saturated_mass_kg)
  # lines(preds$date_num, preds$pred_plant_weight_kg)
  plist[[p]] <- preds
}


# --- M block plants

p = 'M-6'
for(p in c('M-6','M-7','M-10','M-11')) {
  x <- subset(dm, plant_id == p)
  
  # Subsetting dates for D block plants
  x <- subset(dm, plant_id == p & (date <= '2019-10-24' | date >= '2019-11-11' & date <= '2019-11-27'))
  ggplot(x, aes(x=date, y=saturated_mass_kg)) + geom_point() + geom_point() + ggtitle(p)
  
  # plants were a week old on 9-16 so assume the weight is zero on day 1
  start_wt <- x$saturated_mass_kg[!is.na(x$saturated_mass_kg)][1] # data are sorted above
  x$saturated_mass_kg <- x$saturated_mass_kg - start_wt
  pot.wt <- pot_wt$pot_saturated_weight_kg[pot_wt$plant_id == p]
  plant.wt <- plant_wt$wet_weight_g[plant_wt$plant_id == p] / 1000 # wt in grams, convert to kg
  end.plant.wt <- plant.wt + pot.wt - start_wt
  x <- rbind(x, data.frame(date=as.Date('2019-12-13', format='%Y-%m-%d'),
                           plant_id=p, 
                           saturated_mass_kg=end.plant.wt))
  x$saturated_mass_kg <- as.numeric(x$saturated_mass_kg)
  ggplot(x, aes(x=date, y=saturated_mass_kg)) + geom_point() + geom_line() + ggtitle(p)
  x$saturated_mass_kg[x$saturated_mass_kg < 0] <- NA
  
  # convert date to numeric for model fitting
  x$date_num <- as.numeric(x$date - pdat$date[1]) + 1
  
  ## logistic model fitting ----
  
  # fit a self-starting logistic curve
  ssl <- nls(saturated_mass_kg ~ SSlogis(date_num, a, b, c), data = x) # 3-point
  # ssl <- nls(saturated_mass_kg ~ SSfpl(date_num, a, b, c, d), data = x) # 4-point
  
  # summary(ssl)
  # xv <- seq(0, x$date_num[length(x$date_num)], 1)
  # yv <- predict(ssl, newdata = list(date_num = xv))
  # yv <- as.numeric(yv)
  # plot(x$date_num, x$saturated_mass_kg)
  # lines(xv, yv)
  
  # save predicted weights to master dataframe
  preds <- pdat
  preds$plant_id <- p
  preds$pred_plant_weight_kg <- as.numeric(predict(ssl, newdata = list(date_num = preds$date_num)))
  plot(x$date_num, x$saturated_mass_kg)
  lines(preds$date_num, preds$pred_plant_weight_kg)
  plist[[p]] <- preds
}


# combine all predictions of list into master df
pdat_all <- do.call(rbind, plist)




### ----- OLD CODE, WAS TRYING TO GET DAILY DIFFERENCES (3 AM DAY n+1 MINUS 3 AM DAY n)

diffs = ddply(dm, .(plant_id), function(x) {
  setNames(c(NA, diff(x[['3am_mass']])), '3am_diff')
})
head(diffs)

dm_wide = pivot_wider(dm, names_from = 'plant_id', values_from = '3am_mass')

diffs = as.data.frame(apply(dm_wide[,-1], 2, function(x) c(NA, diff(x))))
diffs$date <- sort(unique(d$date))
diffs_long = pivot_longer(diffs, cols = -date, names_to = 'plant_id', values_to = '3am_diff')

ggplot(diffs_long, aes(x=date, y=`3am_diff`, color=plant_id)) + geom_line()
