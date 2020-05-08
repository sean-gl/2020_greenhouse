
dat = select(allData1, by15, par1_n, pyr1_n, line_PAR_west_umol_m2_s, line_PAR_east_umol_m2_s)
dat$date = lubridate::date(dat$by15) 
dat = dat[dat$date >= '2019-10-24',]


sundat <- ddply(dat, .(date), function(x) {
  x <- x[order(x$by15), ]
  sunup <- which(x$line_PAR_west_umol_m2_s > 5)
  start <- sunup[1]
  end <- sunup[length(sunup)]
  return(data.frame(date=unique(x$date), 
                    sunrise=hour(x$by15[start])+minute(x$by15[start])/60,
                    sunset=hour(x$by15[end])+minute(x$by15[end])/60))
})
# sundat <- sundat[-1,] # 1st day bad
plot(sundat$date, sundat$sunrise, type = 'b', main = "Sunrise (line_PAR_east_umol_m2_s > 1)", xlab='date', ylab='hour'); abline(v=as.Date('2019-11-03'))
plot(sundat$date, sundat$sunset, type = 'b', main = "Sunset (line_PAR_east_umol_m2_s < 1)", xlab='date', ylab='hour'); abline(v=as.Date('2019-11-03'))


# using arduino par sensor
# x = dat[dat$date == '2019-10-23',]

sundat <- ddply(dat, .(date), function(x) {
  print(unique(x$date))
  x <- x[order(x$by15), ]
  sunup <- which(x$pyr1_n > 0)
  start <- sunup[1]
  end <- sunup[length(sunup)]
  return(data.frame(date=unique(x$date), 
                    sunrise=hour(x$by15[start])+minute(x$by15[start])/60,
                    sunset=hour(x$by15[end])+minute(x$by15[end])/60))
})
sundat <- sundat[-1,] # 1st day bad
plot(sundat$date, sundat$sunrise, type = 'b', main = "Sunrise (par1_n > 10)", xlab='date', ylab='hour'); abline(v=as.Date('2019-11-03'))
plot(sundat$date, sundat$sunset, type = 'b', main = "Sunset (par1_n < 10)", xlab='date', ylab='hour'); abline(v=as.Date('2019-11-03'))


### Try another method, RLE

# lq
x = dat[dat$date == '2019-10-28',]
sundat <- ddply(dat, .(date), function(x) {
  x <- x[order(x$by15), ]
  x$diff <- c(diff(x$line_PAR_west_umol_m2_s),NA)
  x$diff_pos <- ifelse(x$diff > 5, 1, 0)
  rl <- rle(x$diff_pos)
  maxrun <- max(rl$lengths[which(rl$values==1)])
  i <- which(rl$lengths == maxrun & rl$values == 1)[1]
  sunup <- sum(rl$lengths[1:(i-1)])
  sunup
  # sunup <- which(x$line_PAR_west_umol_m2_s > 5)
  start <- sunup[1]
  end <- sunup[length(sunup)]
  return(data.frame(date=unique(x$date), 
                    sunrise=hour(x$by15[start])+minute(x$by15[start])/60,
                    sunset=hour(x$by15[end])+minute(x$by15[end])/60))
})
# sundat <- sundat[-1,] # 1st day bad
plot(sundat$date, sundat$sunrise, type = 'b', main = "Sunrise (line_PAR_east_umol_m2_s > 1)", xlab='date', ylab='hour'); abline(v=as.Date('2019-11-03'))
plot(sundat$date, sundat$sunset, type = 'b', main = "Sunset (line_PAR_east_umol_m2_s < 1)", xlab='date', ylab='hour'); abline(v=as.Date('2019-11-03'))

sub = subset(dat, date >= '2019-11-01' & date <= '2019-11-04')
sub = subset(dat, date >= '2019-10-24' & date <= '2019-10-28')

ggplot(sub, aes(x=by15)) +
  geom_line(aes(y=line_PAR_west_umol_m2_s), linetype = 'dashed') +
  geom_line(aes(y=pyr1_n)) 


### get hourly means to compare
dat$hour <- hour(dat$by15)
hourlyMeans <- ddply(dat, .(date, hour), function(x) {
  setNames(c(mean(x$line_PAR_west_umol_m2_s, na.rm = T), mean(x$line_PAR_east_umol_m2_s, na.rm = T), mean(x$par1_n, na.rm = T)),
             c('par_west','par_east','par1_n'))
})

x=hourlyMeans[hourlyMeans$date==hourlyMeans$date[2],]
maxSun <- ddply(hourlyMeans, .(date), function(x) {
  i <- which(x$par_east == max(x$par_east, na.rm = T))
  max_par_east <- x$hour[i]
  i <- which(x$par_west == max(x$par_west, na.rm = T))
  max_par_west <- x$hour[i]
  i <- which(x$par1_n == max(x$par1_n, na.rm = T))
  max_par1_n <- x$hour[i]
  setNames(c(max_par_east, max_par_west,max_par1_n), c('max_par_east','max_par_west','max_par1_n'))
})

ggplot(maxSun, aes(x=date)) +
  geom_line(aes(y=max_par_east), linetype = 'dashed') +
  geom_line(aes(y=max_par_west)) 

before <- maxSun[maxSun$date < '2019-11-03',]
after <- maxSun[maxSun$date >= '2019-11-03',]
before %>% select(-date) %>% boxplot()
before %>% select(-date) %>% boxplot()
after %>% select(-date) %>% boxplot()


# ---- Compare PAR to transpiration

df <- readRDS('/home/sean/github/2020_greenhouse/second_fall_experiment/data/combined_data/combdat_plant_level.rds')
df$date = date(df$by15)

sub = subset(df, date>='2019-10-28' & date<='2019-10-31' & plant_id=='W-7')
sub = subset(df, date=='2019-10-30' & plant_id=='W-7')

ggplot(sub, aes(x=by15)) +
  geom_line(aes(y=T_mg_s*30)) + 
  geom_line(aes(y=line_PAR_east_umol_m2_s), linetype='dashed') +
  geom_line(aes(y=par1_n), linetype='dotted')


df2 <- df
ind <- df2$by15 <= as.POSIXct('2019-11-03 02:00', tz="GMT")
df2$line_PAR_east_umol_m2_s[ind] <- df2$line_PAR_east_umol_m2_s[ind] - 