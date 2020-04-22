rm(list=ls())
packages <- c('lubridate','plyr','ggplot2','car','readODS')
lapply(packages, require, character.only = TRUE)
Sys.setenv(tz='GMT')



### -------- SECTION 1: Calculate Daily Above-Ground Plant Mass -------------


# Read in end-of-experiment plant/root wet weights
plant_wt <- read_ods('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/end_of_experiment_data.ods',
                     col_names = T)
plant_wt$plant_id <- toupper(plant_wt$plant_id)
# add up weight of sensors (will be used below)
plant_wt$all_sensors_wt_kg <- 1e-3*rowSums(plant_wt[,c('bs_sensor1_wt_g','bs_sensor2_wt_g','teros_wt_g','watermark_wt_g')], na.rm = T)


# Get mean dry weights by treatement for harvested plants, and then model the wet weight
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
biomass2$mass_above_g <- rowSums(biomass2[ , c('leaf_live_mass','leaf_dead_mass','stem_mass','ear_mass','tassel_mass')])
summary(biomass2$mass_above_g)
biomass3$mass_above_g <- rowSums(biomass3[ , c('leaf_live_mass','leaf_dead_mass','stem_mass','ear_mass','tassel_mass')])
summary(biomass3$mass_above_g)

# combine data
biomass <- rbind(biomass1[,c('date','pot_id','mass_above_g')], biomass2[,c('date','pot_id','mass_above_g')],
                 biomass3[,c('date','pot_id','mass_above_g')])
biomass$block <- toupper(substr(biomass$pot_id,1,1))
biomass$pot_id <- toupper(biomass$pot_id)
biomass$block[biomass$pot_id %in% c('W-25','W-26','W-27','W-28')] <- 'V'
biomass$block <- as.factor(biomass$block)


# model wet mass based on dry mass, using final harvest data
bm3 <- biomass3[,c('date','pot_id','mass_above_g')]
bm3$pot_id <- toupper(bm3$pot_id)
names(bm3) <- c('date','plant_id','dry_weight_g')
bm3 <- merge(bm3, plant_wt[,c('plant_id','wet_weight_g')])
bm3$block <- substr(bm3$plant_id, 1,1)
bm3$block[bm3$plant_id %in% c('W-25','W-26','W-27','W-28')] <- 'V'
bm3$block <- as.factor(bm3$block)
plot(wet_weight_g ~ dry_weight_g, bm3, col=bm3$block)
m <- lm(wet_weight_g ~ dry_weight_g + block, bm3); summary(m)

# now, predict wet weights for each block/date combination, using dry weights
names(biomass)[names(biomass) %in% 'mass_above_g'] <- 'dry_weight_g'
biomass$predicted_wet_weight_g <- predict(m, newdata = biomass)
# plot predictions
ggplot(biomass, aes(x=date, y=predicted_wet_weight_g, color=block)) + geom_point()

# compare predictions to actual wet weights (on final harvest)
m <- merge(plant_wt[,c('plant_id','wet_weight_g')],
           subset(biomass, date=='2019-12-12', select = c(pot_id, predicted_wet_weight_g)),
           by.x = 'plant_id', by.y ='pot_id')
plot(predicted_wet_weight_g ~ wet_weight_g, m); abline(c(0,1))
m2 <- lm(predicted_wet_weight_g ~ wet_weight_g, m); summary(m2)
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
for(i in ind){
  m <- which(plant_wt$plant_id == biomass.expand$pot_id[i])
  if(length(m)==1) biomass.expand$predicted_wet_weight_g[i] <- plant_wt$wet_weight_g[m]
}
ggplot(biomass.expand, aes(x=date,y=predicted_wet_weight_g,color=block)) + geom_point()


# convert date to numeric, so we can fit a logisitic curve
biomass.expand$date_num <- as.numeric(biomass.expand$date - min(biomass.expand$date)) + 1

# --- Fit logistic & linear curves for each block's means (excluding block V)
x <- subset(biomass.expand, block=='V')
modeled_plant_wt <- ddply(biomass.expand, .(block), function(x) {
  blk <- unique(x$block)
  d <- data.frame(date_num=seq(1, max(x$date_num)), date=seq.Date(min(x$date), max(x$date), 1))
  if(blk != 'V') {
    m.logis <- nls(predicted_wet_weight_g ~ SSlogis(date_num, a, b, c), data = x)
    d$modeled_weight_logistic_g <- predict(m.logis, d)
  } 
  m.linear <- lm(predicted_wet_weight_g ~ date_num, data=x)
  d$modeled_weight_linear_g <- predict(m.linear, d)
  png(paste('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/mass_balance/block',
            blk, 'logistic_v_linear.png', sep = '_'), width=1500, height=900)
  plot(predicted_wet_weight_g ~ date, data=x, col='red', main=paste('block', blk, sep = ' '))
  if(blk != 'V') lines(modeled_weight_logistic_g ~ date, data=d)
  lines(modeled_weight_linear_g ~ date, data=d, lty='dashed')
  dev.off()
  return(d)
})
summary(modeled_plant_wt)
ggplot(modeled_plant_wt, aes(x=date, y=modeled_weight_logistic_g, color=block)) + geom_line()
ggplot(modeled_plant_wt, aes(x=date, y=modeled_weight_linear_g, color=block)) + geom_line()

# Save the predictions
saveRDS(modeled_plant_wt, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/modeled_aboveground_plant_weights.rds')

# read back in 
# modeled_plant_wt <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/modeled_aboveground_plant_weights.rds')



### -------- SECTION 2: Calculate Soil Water Content and Matric Potential -------------


### 2.1 --- Recreate vanBavel desorption curve 
###         (for converting soil water content to water potential)


# read in desorption-curve data (reverse-engineered from vanBavel etal, 1978, fig. 1)
vb <- read.csv("/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/vanBavel/van_bavel_data_extract.csv")
names(vb) <- c('volumetric_water_content','pressure_potential_kPa')

# plot vanBavel curve (points extracted from curve in fig. 1)
plot(pressure_potential_kPa ~ volumetric_water_content, data = vb, log='y', xlim=c(0,0.8), ylim=c(1e-1, 1e4), xaxt='n')
axis(side = 1, at=seq(0,0.8,0.1)); axis(side = 2, at=10^(-1:4))

# fit a loess spline to curve
# NOTE: We aren't concerned with overfitting here; the points in the plot above are
# not actual data but are digitally extracted from the curve in Fig. 1, vanBavel
vb_loess_fit <- loess(pressure_potential_kPa~volumetric_water_content, data = vb, span=0.1); summary(vb_loess_fit)

# prediction grid
vb_pred <- data.frame(volumetric_water_content=seq(0, 0.8, 0.001))
vb_pred$pressure_potential_kPa <- predict(vb_loess_fit, newdata = vb_pred)

# overlay loess fit to original data/curve: looks perfect!
plot(pressure_potential_kPa ~ volumetric_water_content, data = vb, log='y', xlim=c(0,0.8), ylim=c(1e-1, 1e4), xaxt='n')
axis(side = 1, at=seq(0,0.8,0.1)); axis(side = 2, at=10^(-1:4))
lines(pressure_potential_kPa ~ volumetric_water_content,data = vb_pred, col='red')

plot(pressure_potential_kPa ~ volumetric_water_content, data = vb_pred, type = 'b')

# Save loess fit vb_pred
vb_pred <- vb_pred[complete.cases(vb_pred),] # remove NA data outside of range
vb_pred$pressure_potential_kPa <- -(vb_pred$pressure_potential_kPa) # reverse the sign (tension not pressure)
write.csv(vb_pred, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/vanBavel/vanBavel_curve_data_final.csv',
          row.names = F)


### 2.2 ---  Calculate Soil Water Content for each Treatment Period


# read in 15-minute aggregated (and flagged) balance data 
baldat <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_long_aggflag.rds')
# omit border plants
baldat <- subset(baldat, !grepl('border', baldat$plant_id)) 
# rename scale weight column for clarity
colnames(baldat)[colnames(baldat)=='mean_weight_kg'] <- 'scale_weight_kg'
colnames(baldat)[colnames(baldat)=='roundTime'] <- 'by15'




## ----- Treatmeant Periods 1-2 ----

start <- '2019-10-22'; end <- '2019-12-12'

sub <- subset(baldat, date >= start & date <= end)
ggplot(sub, aes(x=by15, y=scale_weight_kg, color=plant_id)) + geom_point() +geom_line() + 
  geom_vline(xintercept = as.POSIXct('2019-10-24 00:00', tz = 'GMT')) +
  geom_vline(xintercept = as.POSIXct('2019-10-23 02:00', tz = 'GMT')) +
  geom_vline(xintercept = as.POSIXct('2019-10-22 02:00', tz = 'GMT'))



# subset scale data to range for Treatment 1
wc <- subset(baldat, date >= start & date <= end &
               !plant_id %in% c('W-25','W-26','W-27','W-28'),
                 select = -c(max_diff_weight, hour))

# get mean bs sensor wt to subtract from pot W-11 below (it was not recorded)
mean_bs_wt <- mean(unlist(plant_wt[,c('bs_sensor1_wt_g','bs_sensor2_wt_g')]), na.rm = T)
# omit virgin plants, they aren't in this trt
plant_wt_sub <- subset(plant_wt, !plant_id %in% c('W-25','W-26','W-27','W-28','W-2'),
                       select = c(plant_id, all_sensors_wt_kg))

# merge to harvest plant/sensor weight data
wc <- merge(wc, plant_wt_sub, all = T)

# calculate mass of wet soil only: subtract out pot weight and sensor weights
# pot weight guestimated to be 0.15 kg
wc$wet_soil_mass_kg <- wc$scale_weight_kg - wc$all_sensors_wt_kg - 0.15
wc$wet_soil_mass_kg[wc$plant_id=='W-11'] <- wc$scale_weight_kg[wc$plant_id=='W-11'] - mean_bs_wt/1000
# drop columns 
wc <- subset(wc, select=-c(all_sensors_wt_kg))


# now merge modeled plant weights 
wc2 <- merge(wc,
             subset(modeled_plant_wt, date >= start & date <= end & block != 'V'),
             all.x = T)

# convert modeled plant weights from g to kg
wc2$modeled_weight_linear_kg <- wc2$modeled_weight_linear_g/1000
wc2$modeled_weight_logistic_kg <- wc2$modeled_weight_logistic_g/1000
wc2 <- subset(wc2, select = -c(modeled_weight_linear_g, modeled_weight_logistic_g, date_num))

# subtract out modeled above-ground plant biomass.
# Use logistic model, since no Virign plants involved.
wc2$wet_soil_mass_kg <- wc2$wet_soil_mass_kg - wc2$modeled_weight_logistic_kg


# Now, subset out the starting pot weights to use as a reference in calculating dry mass of soil
wcStart <- subset(wc2, by15 == as.POSIXct('2019-10-22 02:00', tz = "GMT"))

# back-calculate mass of dry soil, assuming water content of 0.45 (vanBavel) and bulk density of 0.65 (from Garrett; air dry)
saturated_prop_water <- 0.45
bulk_density <- 0.65
wcStart$dry_soil_mass_kg <- wcStart$wet_soil_mass_kg / (1 + saturated_prop_water / bulk_density)
wcStart$dry_soil_vol_L <- wcStart$dry_soil_mass_kg / bulk_density


# now calculate water content for each plant at 15-min steps
wcSplit <- split(wc2, wc2$plant_id)
wc2 <- do.call(rbind, lapply(wcSplit, function(x) {
  pl <- unique(x$plant_id)
  drywt <- wcStart$dry_soil_mass_kg[wcStart$plant_id == pl]
  dryvol <- wcStart$dry_soil_vol_L[wcStart$plant_id == pl]
  x$volumetric_water_content <- (x$wet_soil_mass_kg - drywt) / dryvol
  return(x)
}))

# use the loess model (see above; vanBavel) to convert soil water content to water potential
# note: we have to change the sign to get tension instead of postive pressure.
wc2$pressure_potential_kPa <- -(predict(vb_loess_fit, newdata = wc2))
summary(wc2)

# save the predcited soil water potentials: we will add to these below...
wcAll <- wc2

# plots of soil water potential
png(paste0('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/mass_balance/modeled_soil_water_potential/',
           'Treatment1_WestBlock'), width=1500, height=900)
ggplot(subset(wc2, block == 'D')) + 
  geom_point(aes(x=by15, y=pressure_potential_kPa, color = plant_id)) +
  scale_x_datetime(date_breaks = '2 days', date_labels = '%m-%d') +
  ggtitle("Modeled soil water potential, D block (west)")
dev.off()

png(paste0('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/mass_balance/modeled_soil_water_potential/',
           'Treatment1_MiddleBlock'), width=1500, height=900)
ggplot(subset(wc2, block == 'M')) +
  geom_point(aes(x=by15, y=pressure_potential_kPa, color = plant_id)) +
  scale_x_datetime(date_breaks = '2 days', date_labels = '%m-%d') +
  ggtitle("Modeled soil water potential, M block (middle)")
dev.off()

# what's going on with M-7 ?

## CLAY: I think the reason M-7 ceclines so much more than others in that block 
# is that the plant was smaller than the others, so the logisitc model overestimates 
# it's weight, and thus underestimates the weight of the soil...and thus 
# overestimates the soil water content....at least that's my best guess.

x <- subset(baldat, block == 'M' & date >= '2019-10-21' & date <= '2019-11-04')
ggplot(x) +
  geom_point(aes(x=by15, y=scale_weight_kg, color = plant_id)) +
  scale_x_datetime(date_breaks = '2 days', date_labels = '%m-%d')

png(paste0('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/mass_balance/modeled_soil_water_potential/',
           'Treatment1_EastBlock'), width=1500, height=900)
ggplot(subset(wc2, block == 'W')) +
  geom_point(aes(x=by15, y=pressure_potential_kPa, color = plant_id)) +
  scale_x_datetime(date_breaks = '2 days', date_labels = '%m-%d') +
  ggtitle("Modeled soil water potential, W block (east)")
dev.off()



## ----- Treatmeant Period 3 (Virgin plants only) ----


start <- '2019-11-27'; end <- '2019-12-12'

# sub <- subset(baldat, date >= start & date <= end)
# ggplot(sub, aes(x=by15, y=scale_weight_kg, color=plant_id)) + geom_point() +geom_line() + 
#   geom_vline(xintercept = as.POSIXct('2019-10-24 00:00', tz = 'GMT')) +
#   geom_vline(xintercept = as.POSIXct('2019-10-23 02:00', tz = 'GMT')) +
#   geom_vline(xintercept = as.POSIXct('2019-10-22 02:00', tz = 'GMT'))


# subset scale data to range for Treatment 3 & virgin plants only
wc <- subset(baldat, date >= start & date <= end &
               plant_id %in% c('W-25','W-26','W-27','W-28'),
             select = -c(max_diff_weight, hour))

ggplot(wc, aes(x=by15, y=scale_weight_kg, color=scale)) + geom_point()

# merge to harvest plant/sensor weight data
plant_wt_sub <- subset(plant_wt, plant_id %in% c('W-25','W-26','W-27','W-28'),
                       select = c(plant_id, all_sensors_wt_kg))
wc <- merge(wc, plant_wt_sub, all.x = T)

# calculate mass of wet soil only: subtract out pot weight and sensor weights
# pot weight guestimated to be 0.15 kg
wc$wet_soil_mass_kg <- wc$scale_weight_kg - wc$all_sensors_wt_kg - 0.15
# drop columns 
wc <- subset(wc, select=-c(all_sensors_wt_kg))


# now merge modeled plant weights 
wc2 <- merge(wc,
             subset(modeled_plant_wt, date >= start & date <= end & block != 'V'),
             all.x = T)

# convert modeled plant weights from g to kg
wc2$modeled_weight_linear_kg <- wc2$modeled_weight_linear_g/1000
wc2$modeled_weight_logistic_kg <- wc2$modeled_weight_logistic_g/1000
wc2 <- subset(wc2, select = -c(modeled_weight_linear_g, modeled_weight_logistic_g, date_num))

# subtract out modeled above-ground plant biomass.
# Use linear model, since all plants are Virign 
wc2$wet_soil_mass_kg <- wc2$wet_soil_mass_kg - wc2$modeled_weight_linear_kg


### Clay: Need to decide to use Method 1 or Method 2. With Method 1, there is no
# reponse for plant W-28 (interestingly this seems to refelct the scale data).
# With Method 2, there  is a dramatic response. Method 1 is more consistent with 
# the way I did it for treatments 1 & 2 above, so I'll go with that for now.


# Method 1: Now, subset out the starting pot weights to use as a reference in calculating dry mass of soil
wcStart <- subset(wc2, by15 == as.POSIXct('2019-11-28 02:00', tz = "GMT"))
wcStart <- merge(wcStart, plant_wt, all.x = T)
wcStart$wet_soil_mass_kg <- wcStart$scale_weight_kg - wcStart$all_sensors_wt_kg - 0.15


# Method 2: Now, use the ENDING pot weights to use as a reference in calculating dry mass of soil
# wcStart <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/saturated_pot_weights.rds')
# wcStart <- subset(wcStart, plant_id %in% c('W-25','W-26','W-27','W-28'))
# wcStart <- merge(wcStart, plant_wt)
# # calculate mass of wet soil only: subtract out pot weight and sensor weights
# # pot weight guestimated to be 0.15 kg
# wcStart$wet_soil_mass_kg <- wcStart$pot_saturated_weight_kg - wcStart$all_sensors_wt_kg - 0.15


# back-calculate mass of dry soil, assuming water content of 0.45 (vanBavel) and bulk density of 0.65 (from Garrett; air dry)
saturated_prop_water <- 0.45
bulk_density <- 0.65
wcStart$dry_soil_mass_kg <- wcStart$wet_soil_mass_kg / (1 + saturated_prop_water / bulk_density)
wcStart$dry_soil_vol_L <- wcStart$dry_soil_mass_kg / bulk_density


# now calculate water content for each plant at 15-min steps
wcSplit <- split(wc2, wc2$plant_id)
wc2 <- do.call(rbind, lapply(wcSplit, function(x) {
  pl <- unique(x$plant_id)
  drywt <- wcStart$dry_soil_mass_kg[wcStart$plant_id == pl]
  dryvol <- wcStart$dry_soil_vol_L[wcStart$plant_id == pl]
  x$volumetric_water_content <- (x$wet_soil_mass_kg - drywt) / dryvol
  return(x)
}))

# use the loess model (see above; vanBavel) to convert soil water content to water potential
# note: we have to change the sign to get tension instead of postive pressure.
wc2$pressure_potential_kPa <- -(predict(vb_loess_fit, newdata = wc2))
summary(wc2)
           
# PLOT the Virgin treatment   
png(paste0('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/mass_balance/modeled_soil_water_potential/',
           'Treatment3_EastBlock_Virgins'), width=1500, height=900)
ggplot(wc2) +
  geom_point(aes(x=by15, y=pressure_potential_kPa, color = plant_id)) +
  scale_x_datetime(date_breaks = '2 days', date_labels = '%m-%d') +
  ggtitle("Modeled soil water potential, W block (east)")
dev.off()     

# Save the predicted soil water potential data
wcAll <- rbind(wcAll, wc2)
saveRDS(wcAll, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/modeled_psi_soil.rds')

# Read back in and calculate block means
# wcAll <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/modeled_psi_soil.rds')

wcAll_means <- ddply(wcAll, .(by15, date, block), function(x){
  setNames(mean(x$pressure_potential_kPa, na.rm = T), 'mean_soil_water_potential_kPa')
})
with(wcAll_means, table(date, block))

ggplot(wcAll_means, aes(x=by15, y=mean_soil_water_potential_kPa, color=block)) + geom_line()

# Save the predicted soil water potential data (block means)
saveRDS(wcAll_means, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/modeled_psi_soil_block_means.rds')
