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
# sub = subset(dm, plant_id == 'W-6')
# ggplot(sub, aes(x=date, y=saturated_mass_kg)) + geom_point() + geom_line() +
#   scale_x_date(breaks = "1 week", date_labels = '%m-%d') + ggtitle(unique(sub$plant_id))

# add the end weight
# end wt = (measured plant wet weight) + (sat. pot/soil wt at experiment end) - (sat pot/soil wt at experiment start)

# W-6
# mww = 0.5936
# sww_end = 14.51 
# sww_start = 13.9409
# (end_wt = mww + sww_end - sww_start)
 
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


###  CLAY UPDATED ON 4/10....THIS SHOULD NOW WORK WITH ANY PLANT, NOT JUST W BLOCK...
allplants <- unique(plant_wt$plant_id); allplants
allplants <- allplants[!allplants %in% c('W-2','W-25','W-27','W-26','W-28') ]
for(p in allplants) {

  # subset data based on specific (saturated) dates, determined for each block...
  x <- switch(substr(p,1,1), # block letter
              'W' = subset(dm, plant_id == p & date <= '2019-11-04'),
              'M' = subset(dm, plant_id==p & (date <= '2019-10-24' | date >= '2019-12-01')),
              'D' = subset(dm, plant_id == p & (date <= '2019-10-24' | date >= '2019-11-11' & date <= '2019-11-27')))
  
  # x = subset(dm, plant_id==p)
  # check the plot
  # ggplot(x, aes(x=date, y=saturated_mass_kg)) + geom_point() + ggtitle(p)
  
  # plants were a week old on 9-16 so assume the weight is zero on day 1
  # start_wt <- min(x$saturated_mass_kg, na.rm = T)
  start_wt <- x$saturated_mass_kg[!is.na(x$saturated_mass_kg)][1] # data are sorted above
  x$saturated_mass_kg <- x$saturated_mass_kg - start_wt
  pot.wt <- pot_wt$pot_saturated_weight_kg[pot_wt$plant_id == p]
  plant.wt <- plant_wt$wet_weight_g[plant_wt$plant_id == p] / 1000 # wt in grams, convert to kg
  
  end.plant.wt <- plant.wt 
  
  # if(substr(p,1,1)=='D') {
  #   end.plant.wt <- x$saturated_mass_kg[nrow(x)]
  # } else {
  #   end.plant.wt <- plant.wt #+ pot.wt #- start_wt
  # }
  x <- rbind(x, data.frame(date=as.Date('2019-12-13', format='%Y-%m-%d'),
                           plant_id=p, 
                           saturated_mass_kg=end.plant.wt))
  x$saturated_mass_kg <- as.numeric(x$saturated_mass_kg)
  # ggplot(x, aes(x=date, y=saturated_mass_kg)) + geom_point() + ggtitle(p)

  # convert negative values to zero
  x$saturated_mass_kg[x$saturated_mass_kg < 0] <- NA
  
  # convert date to numeric for model fitting
  x$date_num <- as.numeric(x$date - pdat$date[1]) + 1
  
  ## logistic model fitting ----
  
  # fit a self-starting logistic curve
  ssl <- NULL
  ssl <- tryCatch(nls(saturated_mass_kg ~ SSlogis(date_num, a, b, c), data = x),
                  error = function(e) return('error'))
  if(ssl == 'error') ssl <- tryCatch(nls(saturated_mass_kg ~ SSfpl(date_num, a, b, c, d), data = x),
                                     error = function(e) return('error2'))
  if(ssl %in% c('error', 'error2')) ssl <- NULL
  
  # ssl <- nls(saturated_mass_kg ~ SSlogis(date_num, a, b, c), data = x) # 3-point doesn't work often.
  # ssl <- nls(saturated_mass_kg ~ SSfpl(date_num, a, b, c, d), data = x) # 4-point seems to work better.
  
  # summary(ssl)
  # xv <- seq(0, x$date_num[length(x$date_num)], 1)
  
  # save predicted weights to master dataframe
  preds <- pdat
  preds$plant_id <- p
  
  if(is.null(ssl)) {
    preds$pred_plant_weight_kg <- NA
  } else {
    preds$pred_plant_weight_kg <- as.numeric(predict(ssl, newdata = list(date_num = preds$date_num)))
    png(paste('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/mass_balance/',
              p, 'logistic.png', sep = '_'))
    myplot <- ggplot(x) + geom_point(aes(x=date, y=saturated_mass_kg)) +
      geom_line(data = preds, aes(x=date, y=pred_plant_weight_kg)) + ggtitle(p)
    print(myplot)
    dev.off()
  }
 
  plist[[p]] <- preds
}

#combine lis
preds.all <- do.call(rbind, plist)




### ----- Simpler method: Get mean dry weights by treatement for harvested plants, and 
## thne model the wet weigth

biomass1 <- read_ods('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/destructive_harvest_data/read_only/destructive_harvest.ods',
                    sheet = 'biomass')
biomass1$date <- as.Date('2019-10-30')
biomass2 <- read_ods('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/destructive_harvest_data/read_only/destructive_harvest.ods',
                     sheet = 'biomass_2')
biomass2 <- biomass2[1:(which(is.na(biomass2$pot_id))[1]-1),]
biomass2$date <- as.Date('2019-11-15')
biomass3 <- read_ods('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/destructive_harvest_data/read_only/destructive_harvest.ods',
                     sheet = 'biomass_3')
biomass3$date <- as.Date('2019-12-12')
biomass3 <- biomass3[1:(which(is.na(biomass3$pot_id))[1]-1),]

# Note: for biomass1, I didn't separate stem and leaves. 
names(biomass1)[names(biomass1) == 'mass_below_g'] <- 'root_mass_g'
biomass2$mass_above_g <- rowSums(biomass2[ , c('leaf_live_mass','leaf_dead_mass','stem_mass','ear_mass','tassel_mass')])
names(biomass2)[names(biomass2) == 'root_mass'] <- 'root_mass_g'
biomass3$mass_above_g <- rowSums(biomass3[ , c('leaf_live_mass','leaf_dead_mass','stem_mass','ear_mass','tassel_mass')])
names(biomass3)[names(biomass3) == 'root_mass'] <- 'root_mass_g'

# combine data
biomass <- rbind(biomass1[,c('date','pot_id','mass_above_g','root_mass_g')], biomass2[,c('date','pot_id','mass_above_g','root_mass_g')],
                 biomass3[,c('date','pot_id','mass_above_g','root_mass_g')])
biomass$pot_id <- toupper(biomass$pot_id)
biomass$block <- substr(biomass$pot_id,1,1)
biomass$block[biomass$pot_id %in% c('W-25','W-26','W-27','W-28')] <- 'V'
biomass$block <- as.factor(biomass$block)

# get mean dry mass by block and date
# meanDryMass <- ddply(biomass, .(date, block), function(x){
#   setNames(mean(x$mass_above_g), 'mean_mass_above_g')
# })

# model wet mass based on dry mass, using final harvest data
bm3 <- biomass3[,c('date','pot_id','mass_above_g','root_mass_g')]
bm3$pot_id <- toupper(bm3$pot_id)
bm3$dry_weight_g <- rowSums(bm3[,c('mass_above_g','root_mass_g')]) # add root + plant weights
bm3 <- subset(bm3, select = -c(mass_above_g, root_mass_g))
names(bm3)[names(bm3) == 'pot_id'] <- 'plant_id'
bm3 <- merge(bm3, plant_wt[,c('plant_id','wet_weight_g','wet_weight_roots_g')])
bm3$wet_weight_g <- rowSums(bm3[,c('wet_weight_g','wet_weight_roots_g')]) # add root + plant weights
bm3$wet_weight_roots_g <- NULL
bm3$block <- substr(bm3$plant_id, 1,1)
bm3$block[bm3$plant_id %in% c('W-25','W-26','W-27','W-28')] <- 'V'
bm3$block <- as.factor(bm3$block)
# add points for intercept at 0,0 (for each block)
# bm3 <- rbind(bm3, data.frame(plant_id=NA, date=NA, dry_weight_g=rep(0,4), wet_weight_g=rep(0,4), block=c('W','M','D','V')))
plot(wet_weight_g ~ dry_weight_g, bm3, col=bm3$block)
m <- lm(wet_weight_g ~ dry_weight_g + block, bm3); summary(m)
sqrt(mean(m$residuals^2))
# res_zero <- m$residuals[(nrow(bm3)-4):nrow(bm3)]; res_zero # zero-pt predictions
# sqrt(mean(res_zero^2))

# now, predict wet weights for each block/date combination, using dry weights
biomass$dry_weight_g <- rowSums(biomass[,c('dry_weight_g', 'root_mass_g')])
biomass$root_mass_g <- NULL
biomass$predicted_wet_weight_g <- predict(m, newdata = biomass)

# plot predictions
ggplot(biomass, aes(x=date, y=predicted_wet_weight_g, color=block)) + geom_line()

# compare predictions to actual wet weights (on final harvest)
m <- merge(plant_wt[,c('plant_id','wet_weight_g','wet_weight_roots_g')],
           subset(biomass, date=='2019-12-12', select = c(pot_id, predicted_wet_weight_g)),
           by.x = 'plant_id', by.y ='pot_id')
m$wet_weight_g <- rowSums(m[,c('wet_weight_g','wet_weight_roots_g')])
plot(predicted_wet_weight_g ~ wet_weight_g, m); abline(c(0,1))
m2 <- lm(predicted_wet_weight_g ~ wet_weight_g, m); summary(m2)
m2$residuals
sqrt(mean((m2$residuals)^2)) # RMSE
mean(abs(m2$residuals)) # MAD


# add zero weight on plant date
x <- expand.grid(date=as.Date('2019-09-09'), pot_id=unique(biomass$pot_id))
x$block <- substr(x$pot_id,1,1)
x$block[x$pot_id %in% c('W-25','W-26','W-27','W-28')] <- 'V'
x$block <- as.factor(x$block)
x[,c('dry_weight_g', 'predicted_wet_weight_g')] <- 0
biomass.expand <- rbind(x, biomass)

# replace predicted wet weight at harvest with measured weight 
ind <- which(biomass.expand$date=='2019-12-12')
i=ind[1]
for(i in ind){
  m <- which(plant_wt$plant_id == biomass.expand$pot_id[i])
  if(length(m)==1) biomass.expand$predicted_wet_weight_g[i] <- plant_wt$wet_weight_g[m]
}
ggplot(biomass.expand, aes(x=date,y=predicted_wet_weight_g,color=block)) + geom_point()

biomass.expand$date_num <- as.numeric(biomass.expand$date - min(biomass.expand$date)) + 1
biomass$date_num <- as.numeric(biomass$date - min(biomass$date)) + 1

# --- Fit logistic & linear curves for each block's means (excluding block V)
x <- subset(biomass.expand, block=='M')
out <- ddply(biomass.expand, .(block), function(x) {
  b <- unique(x$block)
  d <- data.frame(date_num=seq(1, max(x$date_num)), date=seq.Date(min(x$date), max(x$date), 1))
  if(b != 'V') {
    m.logis <- nls(predicted_wet_weight_g ~ SSlogis(date_num, Asym=600, b, c), data = x)
    d$predict_logis <- predict(m.logis, d)
  } 
  m.linear <- lm(predicted_wet_weight_g ~ date_num, data=x)
  d$predict_lm <- predict(m.linear, d)
  png(paste('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/mass_balance/block',
            b, 'logistic_v_linear.png', sep = '_'), width=1500, height=900)
  plot(predicted_wet_weight_g ~ date, data=x, col='red', main=paste('block', b, sep = ' '))
  if(block != 'V') lines(predict_logis ~ date, data=d)
  lines(predict_lm ~ date, data=d, lty='dashed')
  dev.off()
  return(d)
})
summary(out)
ggplot(out, aes(x=date, y=predict_logis, color=block)) + geom_line()
ggplot(out, aes(x=date, y=predict_lm, color=block)) + geom_line()


# --- Fit logistic & linear curves for each plant with an ending wet weight (excluding block V)


# get means(wet, dry)
# meanMass <- ddply(biomass, .(date, block), function(x){
#   setNames(c(mean(x$dry_weight_g), mean(x$predicted_wet_weight_g)), c('mean_dry_weight_g', 'mean_wet_weight_g'))
# })
# 
# # Wet weights
# ggplot(meanMass, aes(x=date, y=mean_wet_weight_g, color=block)) + geom_point() 
# p <- ggplot(meanMass, aes(x=date, y=mean_wet_weight_g)) + geom_point() +
#   geom_smooth(method = 'exponential', se = F)
# p
# layer_data(p, 2)


### ----- Calculate pot soil weight (dry) using bulk density from green's grade manufacturer
