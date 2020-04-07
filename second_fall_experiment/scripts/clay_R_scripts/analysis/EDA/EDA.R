### EDA 

### Line quantum sensors
d = lq[1000:3000,]
d=lq
plot(d$by15, d$line_PAR_west_umol_m2_s, type='l')
lines(d$by15, d$line_PAR_east_umol_m2_s, col='red')
plot(d$line_PAR_east_umol_m2_s, d$line_PAR_west_umol_m2_s)
abline(0,1, col='red')


### PAR sensors

### PAR_2 south data are only good for a subset of dates ()
### NOTE: interesting reduction in noise (at least at night) for par1_n starting in mid-November
### which coincides with the huge increase in noise for par2_s
d2 = rh
plot(d2$by15, d2$par1_n, type='l')
lines(d2$by15, d2$par2_s, col='red')
d3 = subset(d2, date(by15) > '2019-10-25' & date(by15) < '2019-11-13')

## par1 and 2 track very closely when they both have good data, but par2 is more noisy (esp. when light is low)
## probably ok to just use par1 and not par2 ??
plot(d3$by15, d3$par1_n, type='l')
lines(d3$by15, d3$par2_s, col='red')
plot(d3$par1_n, d3$par2_s)
abline(0,1,col='red')

# find bad par2 dates
# looks like 10/24-25 bad, and anything after 11/13
x=d2[abs(d2$par2_s) > 1300,]
table(date(x$by15))

# compare line_PAR and par_1/2 track together mostly
# NOTE: par1 noisy (and below zero during nighttime)
plot(d$by15, d$line_PAR_west_umol_m2_s, type='l')
lines(d2$by15, d2$par1_n, col='red')



# pyranometers closely matched, note odd downward trend in late November at night...
plot(d$by15, d$pyr1_n, type='l')
lines(d$by15, d$pyr2_s, col='red')

# compare pyranometer 1 and par 1
plot(d$by15, d$par1_n, type='l')
lines(d$by15, d$pyr1_n, col='red')

# wind speed fairly variable between consequtive readings, even using 15-minute means 
d=wind[order(wind$by15, wind$position), ]
d=wind[10000:10100, ]
table(d$position)
plot(wind_speed_m_s ~ by15, data=d[d$position=='bottom', ], type = 'b')
lines(wind_speed_m_s ~ by15, data=d[d$position=='middle', ], col = 'red')


# air tempearture
names(rh)
d=rh
d=subset(rh, date(by15) > "2019-11-01" & date(by15) < "2019-11-10")

# 'high' temperatures track closely
plot(am2320_high_temp ~ by15, data=d, type = 'l')
lines(sht1_high_temp ~ by15, data=d, col = 'red')

# compare 'low' and 'high' (upper and lower sensors)
# NOTE: high/low track well. Clearly the "low" sensor has lower temperatures than the "high" one, on hot/sunny days.
# But on cold/cloudy days, there is not really any temperature gradient from high to low.
plot(sht1_high_temp ~ by15, data=d, type = 'l')
lines(sht2_low_temp ~ by15, data=d, col = 'red')

# Add box temp. NOTE: IN daytime, Box temp generally is in-between 'high' and 'low', except on cold days.
plot(sht1_high_temp ~ by15, data=d, type = 'l')
lines(sht2_low_temp ~ by15, data=d, col = 'blue')
lines(bmp_box_temp ~ by15, data=d, col = 'red')


# Soil temperature
d=subset(soil_temp, date(by15) < '2019-11-20')
d=soil_temp
ggplot(d, aes(x=by15, y=soil_temp_C, color=treatment)) +
  geom_line()


# RH tracks closely between low/high. It is lowest during midday and highest at night.
d=rh
plot(sht1_high_rh ~ by15, data=d, type = 'l')
lines(sht2_low_rh ~ by15, data=d, col = 'red')
