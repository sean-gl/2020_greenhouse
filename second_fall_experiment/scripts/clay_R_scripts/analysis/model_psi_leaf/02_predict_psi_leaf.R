require(ggplot2)
require(plyr)
require(lubridate)
require(readODS)
require(tidyr)
require(dplyr)
require(glmnet)

rm(list = ls())

### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')
Sys.getenv('TZ') # make sure it got set

# read in all data
comb_all <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/data/combined_data/combdat_plant_level.rds')

# add date
comb_all$date <- date(comb_all$by15)

# add irrigation amount (ml) given the previous night
comb_all$irrig <- NA
comb_all$irrig[comb_all$date <= "2019-10-30" & comb_all$treatment == 'well_watered'] <- 750
comb_all$irrig[comb_all$date >= "2019-10-31" & comb_all$treatment == 'well_watered'] <- 1000
comb_all$irrig[comb_all$treatment == 'moderate_drought'] <- 375
comb_all$irrig[comb_all$treatment %in% c('full_drought','virgin_drought')] <- 150
table(comb_all$irrig, useNA = 'a')

# add average of east/west PAR sensors
# comb_all$line_PAR_mean_umol_m2_s <- rowMeans(comb_all[,c('line_PAR_west_umol_m2_s','line_PAR_east_umol_m2_s')], na.rm = TRUE)


### First, subset data to rows with pressure bomb readings
comb <- subset(comb_all, !is.na(mean_psi_leaf_MPa))

#--- Explore linear correlations
nm <- names(comb)
nm <- nm[!nm%in%c('by15','date','block','treatment','plant_id','scale_flag','scale_weight_kg')]
corr <- sapply(nm, function(x) cor(comb[[x]], comb$mean_psi_leaf_MPa, use = 'complete.obs'))
ss <- sapply(nm, function(x) { # get sample sizes
  cc = complete.cases(comb[,c(x,'mean_psi_leaf_MPa')])
  length(cc[cc])
})
cormat <- data.frame(r=round(corr[order(corr, decreasing = T)],2),
                     n=ss[order(corr, decreasing = T)])
cormat


# approach 2: Omit those variables so we have more complete cases
### THIS IS THE APPROACH I ENDED UP USING FOR FINAL MODEL.
df2 <- subset(comb, select = -c(by15, date, leaftemp_bottom, leaftemp_middle,leaftemp_top,
                                windspeed_bottom, windspeed_middle, windspeed_top,
                                soil_temp_C, T_mg_m2_s, T_mg_s, soil_water_potential_kPa, plant_id))
df2 <- subset(df2, complete.cases(df2)); nrow(df2)



# THIS IS THE FINAL MODEL... Selected earlier, see "02 code"
psi_leaf_model <- lm(mean_psi_leaf_MPa ~ line_PAR_mean_umol_m2_s + minutes + irrig + block + leaftemp_mean, df2)

# compare to this model
# psi_leaf_model <- lm(mean_psi_leaf_MPa ~ bmp_box_temp + minutes + irrig + block + leaftemp_mean, df2)

summary(psi_leaf_model)

## Save the model to use elsewhere for prediciton
saveRDS(psi_leaf_model, '/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_psi_leaf/psi_leaf_final_model.rds')
