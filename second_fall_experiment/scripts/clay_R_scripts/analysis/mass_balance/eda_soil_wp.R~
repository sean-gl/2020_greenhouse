
rm(list = ls())
Sys.setenv(tz='GMT')

# read in soil water potential data (watermark & teros/decagon)
swp <- readRDS('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/soil_water_potential/soil_water_potential_compiled_condensed.rds')

# read in raw scale data
bal <- readRDS('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/scale_output/scale_data_raw_compiled_long.rds')

ggplot(swp, aes(x=by15, y=teros_MP_kPa, color=block)) + geom_line()

# look at pot D-11
start <- '2019-10-23'; end <- '2019-11-05'
d11.bal <- subset(bal, plant_id == 'D-11' & date > start & date < end)
d11.swp <- subset(swp, block == 'D' & date > start & date < end)
d11.swp <- subset(swp, block == 'M' & date > start)
d11.swp <- subset(swp, block == 'M' )

plot(teros_MP_kPa/1000 ~ by15, d11.swp, type='l')

plot(teros_MP_kPa/1000 ~ by15, d11.swp, type='l')

plot(teros_MP_kPa/1000 ~ by15, d11.swp, type='l', ylim=c(-3,4))
points((weight-12.5)*3~ timestamp, d11.bal, col='red', pch=20, cex=0.5)



ggplot(d11.swp) +
  geom_line(aes(x=by15, y=teros_MP_kPa)) +
  geom_line(aes(x=by15, ))
