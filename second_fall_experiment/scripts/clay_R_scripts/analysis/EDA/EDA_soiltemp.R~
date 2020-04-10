### Why do soil temperatures not look right (don't align with treatments?)

rh <- read.csv('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/RH_temp_PAR_logger_data/rh_15.csv')
rh$by15 <- as.POSIXct(rh$by15, tz='GMT')
head(rh)

## Key (supposedly)
# t1 = west block
# t2,t3 = east block
# t4 = middle block

# subset and reshap
st <- gather(rh[,c('by15','soil_t1','soil_t2','soil_t3','soil_t4')], 'sensor', 'temp_C', -by15)
st <- subset(st, date(by15) >= '2019-10-27' & date(by15) <= '2019-11-04')
st <- subset(st, date(by15) >= '2019-11-05' & date(by15) <= '2019-11-15')
st <- subset(st, date(by15) >= '2019-11-28')

ggplot(st, aes(x=by15, y=temp_C, color=sensor)) + geom_line() + 
  scale_x_datetime(breaks = '6 hours', date_labels = '%H')
  