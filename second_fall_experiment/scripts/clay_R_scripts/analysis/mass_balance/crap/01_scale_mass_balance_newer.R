rm(list=ls())
packages <- c('lubridate','plyr','ggplot2','car','readODS')
lapply(packages, require, character.only = TRUE)
Sys.setenv(tz='GMT')


### -------- SECTION 1: Calculate Daily Above-Ground Plant Mass -------------


# Read in end-of-experiment plant/root wet weights
plant_wt <- read_ods('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/end_of_experiment_data.ods',
                     col_names = T)
plant_wt$plant_id <- toupper(plant_wt$plant_id)

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

# read in 15-minute aggregated (and flagged) balance data 
baldat <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_long_aggflag.rds')
# omit border plants
baldat <- subset(baldat, !grepl('border', baldat$plant_id)) 
# rename scale weight column for clarity
colnames(baldat)[colnames(baldat)=='mean_weight_kg'] <- 'scale_weight_kg'
colnames(baldat)[colnames(baldat)=='roundTime'] <- 'by15'


# daily mass after irrigation & fully drained, 2 AM
dm <- ddply(baldat, .(date, plant_id, block), function(x) {
  ind <- hour(x$by15)==2 # & minute(x$by15)==0
  n <- length(ind[ind])
  # if(n != 4) {
  #   out <- NA
  # } else {
    out <- c(mean(x$mean_weight_kg[ind], na.rm = T), n)
  # }
  return(setNames(out,c('night_scale_mass_kg','n')))
})
dm$night_scale_mass_kg <- as.numeric(dm$night_scale_mass_kg)


### Test method on a single plant...
sub = subset(dm, plant_id == 'W-6')
ggplot(sub, aes(x=date, y=night_scale_mass_kg)) + geom_point() + geom_line() +
  scale_x_date(breaks = "1 week", date_labels = '%m-%d') + ggtitle(unique(sub$plant_id))

x=subset(dm, plant_id=='D-10')

# get starting weights
start_wt_emp <- ddply(dm, .(plant_id, block), function(x){
  first <- which(!is.na(x$night_scale_mass_kg))[1]
  return(data.frame(date=x$date[first], night_scale_mass_kg=x$night_scale_mass_kg[first]))
})


# subtract out pot weight and sensor weights
# add up weight of sensors
plant_wt$all_sensors_wt_kg <- 1e-3*rowSums(plant_wt[,c('bs_sensor1_wt_g','bs_sensor2_wt_g','teros_wt_g','watermark_wt_g')], na.rm = T)
start_wt_emp <- merge(start_wt_emp, plant_wt[c('plant_id','all_sensors_wt_kg')])
# calculate mass of wet soil only
start_wt_emp$wet_soil_mass_kg <- start_wt_emp$night_scale_mass_kg - start_wt_emp$all_sensors_wt_kg - 0.15



# Now, merge modeled plant weights so we can subtract those out for each day.

# first change block to V to match modeled plant wt df
start_wt_emp$block <- as.character(start_wt_emp$block)
start_wt_emp$block[start_wt_emp$plant_id %in% c('W-25','W-26','W-27','W-28')] <- 'V'
wtRef <- merge(start_wt_emp[ ,c('plant_id','block','wet_soil_mass_kg')],
               modeled_plant_wt, all.y = T)

# convert modeled plant weights from g to kg
wtRef$modeled_weight_linear_kg <- wtRef$modeled_weight_linear_g/1000
wtRef$modeled_weight_logistic_kg <- wtRef$modeled_weight_logistic_g/1000

wtRef$wet_soil_mass_kg_2 <- ifelse(wtRef$block == 'V',
                                   wtRef$wet_soil_mass_kg - wtRef$modeled_weight_linear_kg,
                                   wtRef$wet_soil_mass_kg - wtRef$modeled_weight_logistic_kg)

# back-calculate mass of dry soil, assuming water content of 0.45 and bulk density of 0.65
start_wt_emp$dry_soil_mass_kg <- start_wt_emp$wet_soil_mass_kg / (1 + 0.45/0.65)



# ----- Let's try this for First Treatement, nighttime only

start <- '2019-10-22'; end <- '2019-11-04'
sub <- subset(baldat, plant_id == pid & date >= start & date <= end)
ggplot(sub, aes(x=by15, y=mean_weight_kg)) + geom_point() + 
  geom_vline(xintercept = as.POSIXct('2019-10-24 00:00', tz = 'GMT')) +
  geom_vline(xintercept = as.POSIXct('2019-10-23 02:00', tz = 'GMT')) +
  geom_vline(xintercept = as.POSIXct('2019-10-22 02:00', tz = 'GMT'))


# get starting weights: use 10-22 since data missing on 10-23 for W, M blocks
start_wt_emp <- dm[dm$date >= start & dm$date <= end, ]


# subtract out pot weight and sensor weights
# add up weight of sensors
plant_wt$all_sensors_wt_kg <- 1e-3*rowSums(plant_wt[,c('bs_sensor1_wt_g','bs_sensor2_wt_g','teros_wt_g','watermark_wt_g')], na.rm = T)
mean_bs_wt <- mean(unlist(plant_wt[,c('bs_sensor1_wt_g','bs_sensor2_wt_g')]), na.rm = T)
plant_wt_sub <- subset(plant_wt, !plant_id %in% c('W-25','W-26','W-27','W-28'),
                       select = c(plant_id, all_sensors_wt_kg))

start_wt_emp <- merge(start_wt_emp, plant_wt_sub, all = T)

# calculate mass of wet soil only
start_wt_emp$wet_soil_mass_kg <- start_wt_emp$night_scale_mass_kg - start_wt_emp$all_sensors_wt_kg - 0.15
start_wt_emp$wet_soil_mass_kg[start_wt_emp$plant_id=='W-11'] <- start_wt_emp$night_scale_mass_kg[start_wt_emp$plant_id=='W-11'] - mean_bs_wt/1000
# drop columns 
start_wt_emp <- subset(start_wt_emp, select=-c(n, all_sensors_wt_kg))

# split off the reference weights (day1)
wtRef <- subset(start_wt_emp, date == '2019-10-22')

# back-calculate mass of dry soil, assuming water content of 0.45 (vanBavel) and bulk density of 0.65 (from Garrett; air dry)
saturated_prop_water <- 0.45
bulk_density <- 0.65
wtRef$dry_soil_mass_kg <- wtRef$wet_soil_mass_kg / (1 + saturated_prop_water / bulk_density)
wtRef$dry_soil_vol_L <- wtRef$dry_soil_mass_kg / bulk_density

# now merge modeled plant weights 
wtRef2 <- merge(start_wt_emp,
               subset(modeled_plant_wt, date >= start & date <= end & block != 'V'),
               all = T)

# convert modeled plant weights from g to kg
wtRef2$modeled_weight_linear_kg <- wtRef2$modeled_weight_linear_g/1000
wtRef2$modeled_weight_logistic_kg <- wtRef2$modeled_weight_logistic_g/1000
wtRef2 <- subset(wtRef2, select = -c(modeled_weight_linear_g, modeled_weight_logistic_g, date_num))

# use linear predcitions only for Virgin block; otherwise use logistic
wtRef2$wet_soil_mass_kg <- ifelse(wtRef2$block == 'V',
                                 wtRef2$wet_soil_mass_kg - wtRef2$modeled_weight_linear_kg,
                                 wtRef2$wet_soil_mass_kg - wtRef2$modeled_weight_logistic_kg)

# now calculate water content
wtRef2$volumetric_water_content <- NA
for(i in 1:nrow(wtRef2)) {
  # print(i)
  pl <- wtRef2$plant_id[i]
  # WHY ARE SOME PLANT_ID's NA???
  if(!is.na(pl)) {
    drywt <- wtRef$dry_soil_mass_kg[wtRef$plant_id == pl]
    dryvol <- wtRef$dry_soil_vol_L[wtRef$plant_id == pl]
    wtRef2$volumetric_water_content[i] <- (wtRef2$wet_soil_mass_kg[i] - drywt) / dryvol
  } else wtRef2$volumetric_water_content[i] <- NA
}

# convert water content to soil water potential, using vanBavel curve fit data
vb <- read.csv('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/vanBavel/vanBavel_desorptionCurve_dataExtract.csv')

# round values to 3 digits, to match VB data
wtRef2$volumetric_water_content <- round(wtRef2$volumetric_water_content, 3)
# merge
wtRef3 <- merge(wtRef2, vb, by = 'volumetric_water_content', all.x = T)

# plots of soil water potential
ggplot(subset(wtRef3, block == 'D')) +
  geom_point(aes(x=date, y=pressure_potential_kPa, color = plant_id))
ggplot(subset(wtRef3, block == 'M')) +
  geom_point(aes(x=date, y=pressure_potential_kPa, color = plant_id))
ggplot(subset(wtRef3, block == 'W')) +
  geom_point(aes(x=date, y=pressure_potential_kPa, color = plant_id))

# plots of soil water content
ggplot(subset(wtRef3, block == 'D')) +
  geom_point(aes(x=date, y=volumetric_water_content, color = plant_id))
ggplot(subset(wtRef3, block == 'M')) +
  geom_point(aes(x=date, y=volumetric_water_content, color = plant_id))
ggplot(subset(wtRef3, block == 'W')) +
  geom_point(aes(x=date, y=volumetric_water_content, color = plant_id))




### ---- Repeat above, Treatement 1, but using 15-minute scale data ----

pid <- 'D-10'
start <- '2019-10-22'; end <- '2019-11-04'
sub <- subset(baldat, plant_id == pid & date >= start & date <= end)
ggplot(sub, aes(x=by15, y=scale_weight_kg)) + geom_point() + 
  geom_vline(xintercept = as.POSIXct('2019-10-24 00:00', tz = 'GMT')) +
  geom_vline(xintercept = as.POSIXct('2019-10-23 02:00', tz = 'GMT')) +
  geom_vline(xintercept = as.POSIXct('2019-10-22 02:00', tz = 'GMT'))



# subset scale data to range for Treatment 1
wc <- subset(baldat, date >= start & date <= end, select = -c(max_diff_weight, hour))

# add up weight of sensors
plant_wt$all_sensors_wt_kg <- 1e-3*rowSums(plant_wt[,c('bs_sensor1_wt_g','bs_sensor2_wt_g','teros_wt_g','watermark_wt_g')], na.rm = T)
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
wcStart <- subset(wc2, by15 == as.POSIXct('2019-10-22 01:00', tz = "GMT"))

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



# convert water content to soil water potential, using vanBavel curve fit data
vb <- read.csv('/home/wmsru/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance/vanBavel/vanBavel_desorptionCurve_dataExtract.csv')

# round values to 3 digits, to match VB data
wc2$volumetric_water_content <- round(wc2$volumetric_water_content, 3)
# merge
wc3 <- merge(wc2, vb, by = 'volumetric_water_content', all.x = T)

# plots of soil water potential
png(paste0('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/mass_balance/modeled_soil_water_potential/',
           'Treatment1_FullDrought'), width=1500, height=900)
ggplot(subset(wc3, block == 'D')) + ylim(c(-600, 0)) +
  geom_point(aes(x=by15, y=pressure_potential_kPa, color = plant_id)) +
  scale_x_datetime(date_breaks = '2 days', date_labels = '%m-%d') +
  ggtitle("Modeled soil water potential, full drought treatment")
dev.off()

png(paste0('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/mass_balance/modeled_soil_water_potential/',
           'Treatment1_ModerateDrought'), width=1500, height=900)
ggplot(subset(wc3, block == 'M')) + ylim(c(-600, 0)) +
  geom_point(aes(x=by15, y=pressure_potential_kPa, color = plant_id)) +
  scale_x_datetime(date_breaks = '2 days', date_labels = '%m-%d') +
  ggtitle("Modeled soil water potential, moderate drought treatment")
dev.off()

ggplot(subset(wc3, block == 'W')) + ylim(c(-600, 0)) +
  geom_point(aes(x=by15, y=pressure_potential_kPa, color = plant_id))

# plots of soil water content
ggplot(subset(wc3, block == 'D')) +
  geom_point(aes(x=by15, y=volumetric_water_content, color = plant_id))
ggplot(subset(wc3, block == 'M')) +
  geom_point(aes(x=by15, y=volumetric_water_content, color = plant_id))
ggplot(subset(wc3, block == 'W')) +
  geom_point(aes(x=by15, y=volumetric_water_content, color = plant_id))


# plots comparing to Garret's sensors 
# D block = plant D-11 

# read garrett's soil sensor data in
soilDat <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/soil_water_potential/soil_water_potential_compiled_condensed.rds')

# chose block
blk <- 'D'

# subset to D block treatment 1
sdSub <- subset(soilDat, date >= start & date <= end & block == blk)

# first get means of water content data
# wcMeans <- ddply(wc3, .(block, by15, date), function(x){
#   setNames(mean(x$pressure_potential_kPa, na.rm = T), 'pressure_potential_kPa')
# })

# compare same pot, D-11
wc4 <- subset(wc3, plant_id == 'D-11')
wc4 <- merge(wc4, sdSub, all = T); nrow(wcMeans2); nrow(wtRef4)

colors <- c("modeled" = "blue", "watermark" = "red", "teros" = "orange")

png(paste0('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/mass_balance/modeled_soil_water_potential/',
           'Treatment1_FullDrought_Sensors_v_Modeled'), width=1500, height=900)
ggplot(wc4, aes(x=by15)) +
  geom_line(aes(y=pressure_potential_kPa, color='modeled')) +
  geom_line(aes(y=teros_MP_kPa, color='teros')) + 
  geom_line(aes(y=watermark_MP_kPa, color='watermark')) +
  scale_x_datetime(date_breaks = '2 days', date_labels = '%m-%d') +
  ggtitle("Soil water potential, full drought treatment") +
  labs(color = "Legend")
dev.off()


# --- repeat above but for M block (moderate trt)---


# chose block
blk <- 'M'

# subset to M block treatment 1
sdSub <- subset(soilDat, date >= start & date <= end & block == blk)

# compare same pot, m_7
wc4 <- subset(wc3, plant_id == 'M-7')
wc4 <- merge(wc4, sdSub, all = T); nrow(wcMeans2); nrow(wtRef4)

colors <- c("modeled" = "blue", "watermark" = "red", "teros" = "orange")

png(paste0('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/mass_balance/modeled_soil_water_potential/',
           'Treatment1_ModerateDrought_Sensors_v_Modeled'), width=1500, height=900)
ggplot(wc4, aes(x=by15)) +
  geom_line(aes(y=pressure_potential_kPa, color='modeled')) +
  geom_line(aes(y=teros_MP_kPa, color='teros')) + 
  geom_line(aes(y=watermark_MP_kPa, color='watermark')) +
  scale_x_datetime(date_breaks = '2 days', date_labels = '%m-%d') +
  ggtitle("Soil water potential, moderate drought treatment") +
  labs(color = "Legend")
dev.off()








# -- ----- OLDER CODE.....


start_wt = sub$night_scale_mass_kg[2]
ind <- as.character(modeled_plant_wt$block)==unique(sub$block) & 
  modeled_plant_wt$date == '2019-09-17'
start_plant_wt = modeled_plant_wt$modeled_weight_logistic_g[ind] / 1000

### ----- Calculate bulk dry weight of soil in each pot, based on volume.
head(plant_wt)

# omit plant W-2, not on a scale
plant_wt <- subset(plant_wt, plant_id != 'W-2')

# for M-7, use average depth to soil of other pots 
mn_depth <- mean(unlist(plant_wt[,c('soil_depth_1','soil_depth_2','soil_depth_3','soil_depth_4')]),  na.rm = T)
# depth of soil (pots are 29.8 cm tall, then subtract depth to soil surface) 
plant_wt$mean_soil_depth_cm <- 29.8 - (rowMeans(plant_wt[,c('soil_depth_1','soil_depth_2','soil_depth_3','soil_depth_4')], na.rm = T) / 10)
plant_wt$mean_soil_depth_cm[plant_wt$plant_id=='M-7'] <- 29.8 - (mn_depth/10)


# use formula for conic fustrum volume. r1 = 11.0 cm, r2 = 12.9 cm
plant_wt$soil_volume_L <- 1000*(pi/3)*(plant_wt$mean_soil_depth_cm/100)*(0.11^2 + 0.11*0.129 + 0.129^2)
# convert to soil weight, using bulk density given by manufacturer = 36 (+/-2) lb/ft^3
# (bd_kg_L <- 36 * 0.0160185) # bulk density, in kg/L (manufacturer)
bd_kg_L <- 0.65 # using Garret's calculated value.
plant_wt$soil_dry_weight_kg <- plant_wt$soil_volume_L * bd_kg_L
# assuming water is 45% of soil volume, after drainage, calculate soil saturated wt
plant_wt$soil_saturated_weight_kg <- plant_wt$soil_dry_weight_kg + 0.5*plant_wt$soil_volume_L
summary(plant_wt$soil_saturated_weight_kg)
# add up weight of sensors
plant_wt$all_sensors_wt_kg <- 1e-3*rowSums(plant_wt[,c('bs_sensor1_wt_g','bs_sensor2_wt_g','teros_wt_g','watermark_wt_g')], na.rm = T)
# total pot weight, saturated soil + sensors
plant_wt$total_pot_sat_wt_kg <- rowSums(plant_wt[,c('soil_saturated_weight_kg','all_sensors_wt_kg')])
# View(plant_wt[,c('plant_id','total_pot_sat_wt_kg')])


# merge empirical and theoretical starting weights
m <- merge(plant_wt, 
           start_wt_emp[,c('plant_id','night_scale_mass_kg')], all = T)

# back-calculate water content
m$water_content <- (m$night_scale_mass_kg - m$soil_dry_weight_kg) / (0.65 *m$soil_volume_L)

# exlude the outliers
m <- subset(m, !plant_id %in% c('M-11','W-25','W-26','W-27','W-28','M-6'))
plot(total_pot_sat_wt_kg ~ night_scale_mass_kg, m, ylim=c(10,15)); abline(c(0,1))
identify(m$night_scale_mass_kg, m$total_pot_sat_wt_kg, labels = m$plant_id)
model <- lm(total_pot_sat_wt_kg ~ night_scale_mass_kg, data=m); summary(model)
abline(model$coefficients[1], model$coefficients[2])
m$diff <- m$night_scale_mass_kg - m$total_pot_sat_wt_kg
