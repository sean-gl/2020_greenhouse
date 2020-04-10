
rm(list = ls())
Sys.setenv(tz='GMT')

# read in soil water potential data (watermark & teros/decagon)
swp <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/soil_water_potential/soil_water_potential_compiled_condensed.rds')

# read in raw scale data
bal <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_raw_compiled_long.rds')

# read 15-min means scale data
bal.15 <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_long_aggflag.rds')

ggplot(swp, aes(x=by15, y=teros_MP_kPa, color=block)) + geom_line()

# look at pot D-11 (first treatement)
start <- '2019-10-20'; end <- '2019-11-05'
d11.bal <- subset(bal.15, plant_id == 'D-11' & date > start & date < end)
d11.swp <- subset(swp, block == 'D' & date > start & date < end)

# merge data
d11 <- merge(d11.bal[,c('roundTime','mean_weight_kg')], d11.swp, by.x = 'roundTime', by.y='by15', all = T)
d11$teros_MPa <- d11$teros_MP_kPa / 1000

pdf('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/soil_water_potential/Rplot_d11_teros_v_scale_t1.pdf',
    width=10, height=7)
plot(mean_weight_kg ~ roundTime, d11, col='red',  main='Pot D-11 (full drought)')
par(new = TRUE)
plot(teros_MPa ~ roundTime, d11, type='l', xaxt = "n", yaxt = "n",  ylab = "", xlab = "")
axis(side = 4)
mtext("Teros (MPa)", side = 4, line = 3)
dev.off()

# look at pot M-7
start <- '2019-10-20'; end <- '2019-11-05'
m7.bal <- subset(bal.15, plant_id == 'M-7' & date > start & date < end)
m7.swp <- subset(swp, block == 'M' & date > start & date < end)

# merge data
m7 <- merge(m7.bal[,c('roundTime','mean_weight_kg')], m7.swp, by.x = 'roundTime', by.y='by15', all = T)
m7$teros_MPa <- m7$teros_MP_kPa / 1000
pdf('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/soil_water_potential/Rplot_m7_teros_v_scale_t1.pdf',
    width=10, height=7)
plot(mean_weight_kg ~ roundTime, m7, col='red', main='Pot M-7 (moderate drought)')
par(new = TRUE)
plot(teros_MPa ~ roundTime, m7, type='l', xaxt = "n", yaxt = "n",  ylab = "", xlab = "")
axis(side = 4)
mtext("Teros (MPa)", side = 4, line = 3)
dev.off()

# look at pot D-11 (2nd treatement)
start <- '2019-11-25'; end <- '2019-12-12'
d11.bal <- subset(bal.15, plant_id == 'D-11' & date > start & date < end)
d11.swp <- subset(swp, block == 'D' & date > start & date < end)

# merge data
d11 <- merge(d11.bal[,c('roundTime','mean_weight_kg')], d11.swp, by.x = 'roundTime', by.y='by15', all = T)
d11$teros_MPa <- d11$teros_MP_kPa / 1000

pdf('/home/wmsru/github/2020_greenhouse/second_fall_experiment/figures/clay_figures/soil_water_potential/Rplot_d11_teros_v_scale_t2.pdf',
    width=10, height=7)
plot(mean_weight_kg ~ roundTime, d11, col='red',  main='Pot D-11 (full drought)')
par(new = TRUE)
plot(teros_MPa ~ roundTime, d11, type='l', xaxt = "n", yaxt = "n",  ylab = "", xlab = "")
axis(side = 4)
mtext("Teros (MPa)", side = 4, line = 3)
dev.off()
