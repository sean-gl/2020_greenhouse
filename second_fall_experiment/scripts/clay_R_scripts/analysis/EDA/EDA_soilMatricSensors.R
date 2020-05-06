# read garrett's soil sensor data in
soilDat <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/data/soil_water_potential/soil_water_potential_compiled_condensed.rds')

# plot during treatment 1
sdSub <- subset(soilDat, date >= '2019-10-22' & date <= '2019-11-04')

# looks like soil temp and matric potential are inversely related, at least for block D
ggplot(sdSub) +
  geom_line(aes(x=by15, y=teros_MP_kPa, color=block)) +
  geom_point(aes(x=by15, y=teros_soil_temp_1_C*-50, color=block))

# plot during treatment 2
sdSub <- subset(soilDat, date >= '2019-11-03' & date <= '2019-11-27')
ggplot(sdSub) +
  geom_line(aes(x=by15, y=teros_MP_kPa, color=block)) +
  geom_point(aes(x=by15, y=teros_soil_temp_1_C*50, color=block))

# check for correlation in 1st order diffs of soil temp and matric potential
sdSub <- subset(soilDat, date >= '2019-11-18' & date <= '2019-11-19' & block == 'W')
sdSub$teros_MP_kPa <- -sdSub$teros_MP_kPa
sdSub$teros_MP_kPa_diff <- c(NA, diff(sdSub$teros_MP_kPa))
sdSub$teros_soil_temp_1_C_diff <- c(NA, diff(sdSub$teros_soil_temp_1_C))

cor(sdSub$teros_MP_kPa_diff, sdSub$teros_soil_temp_1_C_diff, use = 'complete.obs')
plot(teros_MP_kPa_diff~teros_soil_temp_1_C_diff, sdSub)
m=lm(teros_MP_kPa_diff~teros_soil_temp_1_C_diff, sdSub); summary(m)



# ---- Now look at WATERMARK sensors ----
# plot during treatment 1
sdSub <- subset(soilDat, date >= '2019-10-28' & date <= '2019-10-31' &  block=='M')

# looks like soil temp and matric potential are inversely related, at least for block D (drought trt)
ggplot(sdSub) +
  geom_line(aes(x=by15, y=watermark_MP_kPa, color=block)) +
  geom_point(aes(x=by15, y=teros_soil_temp_1_C*-5, color=block))

sdSub$watermark_MP_kPa <- -sdSub$watermark_MP_kPa
sdSub$watermark_MP_kPa_diff <- c(NA, diff(sdSub$watermark_MP_kPa))
sdSub$teros_soil_temp_1_C_diff <- c(NA, diff(sdSub$teros_soil_temp_1_C))

cor(sdSub$watermark_MP_kPa_diff, sdSub$teros_soil_temp_1_C_diff, use = 'complete.obs')
plot(watermark_MP_kPa_diff~teros_soil_temp_1_C_diff, sdSub)
m=lm(watermark_MP_kPa_diff~teros_soil_temp_1_C_diff, sdSub); summary(m)

# plot during treatment 2....
# watermark in this treatment does't seem to be correlated with soil temperature.
sdSub <- subset(soilDat, date >= '2019-11-04' & date <= '2019-11-27' & block == 'W')
ggplot(sdSub) +
  geom_line(aes(x=by15, y=watermark_MP_kPa, color=block)) +
  geom_point(aes(x=by15, y=teros_soil_temp_1_C*-10, color=block))

sdSub <- subset(soilDat, date >= '2019-11-18' & date <= '2019-11-22' & block == 'W')
# sdSub$watermark_MP_kPa <- -sdSub$watermark_MP_kPa
sdSub$watermark_MP_kPa_diff <- c(NA, diff(sdSub$watermark_MP_kPa,3))
sdSub$teros_soil_temp_1_C_diff <- c(NA, diff(sdSub$teros_soil_temp_1_C,3))

cor(sdSub$watermark_MP_kPa_diff, sdSub$teros_soil_temp_1_C_diff, use = 'complete.obs')
plot(watermark_MP_kPa_diff~teros_soil_temp_1_C_diff, sdSub)
m=lm(watermark_MP_kPa_diff~teros_soil_temp_1_C_diff, sdSub); summary(m)
