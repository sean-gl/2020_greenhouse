rm(list = ls())
packages <- c('lubridate','plyr','ggplot2')
lapply(packages, require, character.only = TRUE)
Sys.setenv(tz='GMT')

# read data
dat  <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/analysis/clay/output_data/combined_data_predict_psi_leaf_transpiration.rds')
names(dat)

# add hour column
dat$hour <- hour(dat$by15)

# change negative values  of T to zero
ind <- dat$mean_T_mg_s < 0 & !is.na(dat$mean_T_mg_s)
nrow(dat[ind, ]) / nrow(dat)
dat$mean_T_mg_s[ind] <- 0

# add air-leaf temperature diff
# using "top" position gives best correlation with T, by far....but many missing data.
dat$altd <- dat$temp_high_mean - dat$leaftemp_top
dat$altd <- dat$temp_high_mean - dat$leaftemp_highest_avail

length(dat$temp_high_mean[is.na(dat$temp_high_mean)]) / nrow(dat)
length(dat$leaftemp_top[is.na(dat$leaftemp_top)]) / nrow(dat)
length(dat$leaftemp_mean[is.na(dat$leaftemp_mean)]) / nrow(dat)
length(dat$leaftemp_highest_avail[is.na(dat$leaftemp_highest_avail)]) / nrow(dat)

# leaftemp_top is NA 38% of time....can we impute it based on leaftemp_mean?
cor(dat$leaftemp_mean, dat$leaftemp_top, use = 'complete.obs')
cor(dat$leaftemp_mean, dat$VPD_air, use = 'complete.obs')
cor(dat$leaftemp_bottom, dat$leaftemp_top, use = 'complete.obs')
cor(dat$leaftemp_middle, dat$leaftemp_top, use = 'complete.obs')
cor(dat$leaftemp_highest_avail, dat$leaftemp_mean, use = 'complete.obs')

# not sure I buy quadratic; fits different by treatment
ggplot(sub, aes(x=altd, y=mean_T_mg_s, color=treatment)) + geom_point() + geom_smooth(method = 'lm', formula = y ~ poly(x,1))
ggplot(sub, aes(x=leaftemp_mean, y=mean_T_mg_s, color=treatment)) + geom_point() + geom_smooth(method = 'lm', formula = y ~ poly(x,2))

m=lm(mean_T_mg_s ~ poly(altd,1) + mean_irrig_kg, dat[!is.na(dat$altd),]); summary(m)
m=lm(mean_T_mg_s ~ poly(altd,2) * mean_irrig_kg, dat[!is.na(dat$altd),]); summary(m)

m=lm(mean_T_mg_s ~ poly(leaftemp_mean,2) * mean_irrig_kg, dat[!is.na(dat$altd),]); summary(m)

### Very simple model with R2=0.71, note interaction
## NOTE: r2 MUCH lower if using mean leaf temp to calcualte altd (instead of top leaf temp)
m=lm(mean_T_mg_s ~ poly(altd,2)*mean_irrig_kg, dat[!is.na(dat$altd),]); summary(m)
m=lm(mean_T_mg_s ~ poly(VPD_air,2)*mean_irrig_kg, dat[!is.na(dat$altd),]); summary(m)

m=lm(mean_T_mg_s ~ poly(altd,2)*treatment, dat[!is.na(dat$altd),]); summary(m)

# add leaftemp_mean, r2=0.74
m=lm(mean_T_mg_s ~ poly(altd,1)*mean_irrig_kg + leaftemp_mean, dat[!is.na(dat$altd),]); summary(m)
m=lm(mean_T_mg_s ~ poly(altd,2)*pleaftemp_mean, dat[!is.na(dat$altd),]); summary(m)

m=lm(mean_T_mg_s ~ poly(altd,2)*mean_irrig_kg + leaftemp_mean +line_PAR_east_umol_m2_s, dat[!is.na(dat$altd),]); summary(m)
m=lm(mean_T_mg_s ~ leaftemp_mean*line_PAR_east_umol_m2_s, dat[!is.na(dat$altd),]); summary(m)
m=lm(mean_T_mg_s ~ altd*line_PAR_east_umol_m2_s, dat[!is.na(dat$altd),]); summary(m)

# 2-way interactions all significant....r2=0.76
m=lm(mean_T_mg_s ~ poly(leaftemp_mean,2)*mean_irrig_kg + line_PAR_west_umol_m2_s, dat[!is.na(dat$altd),]); summary(m)
# not as good?
m=lm(mean_T_mg_s ~ poly(altd,2)*mean_irrig_kg + line_PAR_west_umol_m2_s, dat[!is.na(dat$altd),]); summary(m)


m=lm(leaftemp_top~leaftemp_mean, dat); summary(m)
plot(dat$leaftemp_mean, dat$leaftemp_top, use = 'complete.obs')
abline(m$coefficients[1], m$coefficients[2], col='red')



plot(dat$leaftemp_mean, dat$mean_T_mg_s, use = 'complete.obs')
plot(dat$leaftemp_highest_avail, dat$mean_T_mg_s, use = 'complete.obs')
plot(dat$leaftemp_top, dat$mean_T_mg_s, use = 'complete.obs', col=dat$treatment)


cor(dat$altd, dat$mean_T_mg_s, use = 'complete.obs')
cor(dat$leaftemp_top, dat$mean_T_mg_s, use = 'complete.obs')

cor(dat$altd, dat$leaftemp_mean, use = 'complete.obs')
cor(dat$altd, dat$temp_high_mean, use = 'complete.obs')
cor(dat$altd, dat$mean_irrig_kg, use = 'complete.obs')

# subset to single block and date

sub <- dat
# sub <- subset(dat, hour(by15) > 9 & hour(by15) < 18)

head(sub)


# What is correlated with T?
predictors <- names(dat)[!names(dat) %in% c('by15','date','block','treatment','mean_T_mg_s')]
out <- lapply(predictors, function(p) {
  # print(p)
  tryCatch({
    return(c(p, cor(sub[[p]], sub$mean_T_mg_s, use = 'complete.obs')))
  }, error = function(e) {
    print(e); return(NA)
  })
})

t.cor <- as.data.frame(do.call(rbind, out), stringsAsFactors = F)
names(t.cor) <- c('predictor', 'r')
t.cor$r <- round(as.numeric(t.cor$r),2)
t.cor[order(t.cor$r, decreasing = T),]

## ---- Let's Examine and Clean up the predictor variables....

# first split data into DAY and NIGHT
day <- subset(dat, hour >= 9 & hour <= 16)
night <-subset(dat, hour >= 17 | hour <= 8)
nrow(day)+nrow(night)==nrow(dat)


# 1. line PAR 

# east/west track together, west is generally greater than east.
plot(dat$by15, dat$line_PAR_east_umol_m2_s, type = 'l', col = 'blue')
lines(dat$line_PAR_west_umol_m2_s, type = 'l', col = 'red')

plot(day$by15, day$line_PAR_east_umol_m2_s, type = 'p', col = 'blue')
lines(dat$line_PAR_west_umol_m2_s, type = 'l', col = 'red')

plot(density(dat$line_PAR_east_umol_m2_s, na.rm = T))
plot(density(day$line_PAR_east_umol_m2_s, na.rm = T))
plot(density(night$line_PAR_east_umol_m2_s, na.rm = T))

plot(density(dat$line_PAR_west_umol_m2_s, na.rm = T))
plot(density(day$line_PAR_west_umol_m2_s, na.rm = T))
plot(density(night$line_PAR_west_umol_m2_s, na.rm = T))

# NOTE: Sean says in greenhouse log that data from WEST on 11/4 was sitting on lab bench 
# from ca. 15:00 until it was downloaded at 16:00
x = subset(dat, date >= '2019-11-02' & date <='2019-11-06')
plot(x$line_PAR_west_umol_m2_s~x$by15)
lines(x$line_PAR_east_umol_m2_s~x$by15, col='red')
# I don't think this matters actually.


# --- 2. par1 (NOTE: I removed par2 in "cobmining" script since data looked bad.)

plot(dat$by15, dat$par1_n, type = 'l', col = 'blue')

# note: looks messier than line_par data, especially at night. Let's compare to line par:
lines(dat$by15, dat$line_PAR_east_umol_m2_s, col = 'red')

# conclusion: looks comparable to line_par, I think I'd just use the line par instead since it's less noisy
# (goes to zero at nighttime.) They're highly correlated
cor(dat$line_PAR_east_umol_m2_s, dat$line_PAR_west_umol_m2_s, use = 'complete.obs')



plot(dat$by15, dat$mean_T_mg_s, type = 'l', col = 'blue')

cor(dat$line_PAR_east_umol_m2_s, dat$par1_n, use = 'complete.obs')

# --- 3. Pyranometers N/S

# N/S highly correlated.
plot(dat$by15, dat$pyr1_n, type = 'l', col = 'blue')
lines(dat$by15, dat$pyr2_s, col = 'red')
cor(dat$pyr2_s, dat$pyr1_n, use = 'complete.obs')
# very good correlation with par
cor(dat$par1_n, dat$pyr1_n, use = 'complete.obs')




cor(sub$par1_n, sub$mean_T_mg_s, use = 'complete.obs')
plot(sub$par1_n, sub$mean_T_mg_s, col = sub$treatment)
plot(sub$leaftemp_mean, sub$mean_T_mg_s, col = sub$treatment)
plot(sub$leaftemp_mean, sub$par1_n, col = sub$treatment)

# PAR vs. T
# all data: looks like quadratic
ggplot(sub, aes(x=line_PAR_east_umol_m2_s, y=mean_T_mg_s, color=treatment)) + geom_point() + geom_smooth(method = 'lm', formula = y ~ poly(x,2))
ggplot(sub, aes(x=leaftemp_mean, y=line_PAR_east_umol_m2_s, color=treatment)) + geom_point() + geom_smooth(method = 'lm')

# time series for a couple day
# interesting, PAR east/west lag behind transpiration!
# this seems to be a mistake in the times before 12/1 (at least)
sub2 <- subset(sub, block == 'M' &date >= '2019-10-10' & date <='2019-11-06')
sub2 <- subset(sub, block == 'M')

plot(line_PAR_west_umol_m2_s~by15, type='l',data=sub2)
lines(mean_T_mg_s*5~by15, type='l', col='red', data=sub2)
lines(line_PAR_east_umol_m2_s~by15, type='l', col='blue', data=sub2)

# get peak time of day for both PAR and T
x = subset(sub2, date == '2019-10-27')
options(warn = 1)
y = ddply(sub2, .(date), function(x) {
  # print(unique(x$date))
  mpw = which(x$line_PAR_west_umol_m2_s == max(x$line_PAR_west_umol_m2_s, na.rm = T))
  mpw = hour(x$by15[mpw]) + minute(x$by15[mpw])/60
  mpe = which(x$line_PAR_east_umol_m2_s == max(x$line_PAR_east_umol_m2_s, na.rm = T))
  mpe = hour(x$by15[mpe]) + minute(x$by15[mpe])/60
  mt = which(x$mean_T_mg_s == max(x$mean_T_mg_s, na.rm = T))
  mt = hour(x$by15[mt]) + minute(x$by15[mt])/60
  if(length(mt)==0) mt = NA
  return(setNames(c(mpw, mpe, mt), c('mpw','mpe','mt')))
})

plot(mt ~ date, y, type='l')
lines(mpe ~ date, y, col='blue')
lines(mpw ~ date, y, col='red')
cor(y[y$date < '2019-11-04',-1], use = 'pairwise.complete.obs')

# legend('topleft', legend = unique(sub$treatment), col = unique(sub$treatment))

summary(lm(mean_T_mg_s ~ poly(line_PAR_east_umol_m2_s, 2) + treatment, 
           data = subset(sub, !is.na(mean_T_mg_s) & !is.na(line_PAR_east_umol_m2_s))))

summary(lm(mean_T_mg_s ~ poly(par1_n, 2) + treatment, 
           data = subset(sub, !is.na(mean_T_mg_s) & !is.na(par1_n))))

summary(lm(mean_T_mg_s ~ poly(sht1_high_rh, 1) + poly(par1_n, 2) + treatment, 
           data = sub[complete.cases(sub[,c('sht1_high_rh','par1_n','mean_T_mg_s')]),]))

summary(lm(mean_T_mg_s ~ poly(sht1_high_rh, 1) + poly(line_PAR_east_umol_m2_s, 2) + treatment + block +
             leaftemp_mean + minutes, 
           data = sub[complete.cases(sub[,c('sht1_high_rh','line_PAR_east_umol_m2_s','mean_T_mg_s')]),]))

x=diff(sub$par1_n)
y=diff(sub$mean_T_mg_s)
plot(x,y)
cor(x, y, use = 'complete.obs')

ggplot(sub, aes(x=by15)) +
  geom_line(aes(y=par1_n, color='red')) +
  geom_line(aes(y=mean_T_mg_s*100, color='blue'))


## ------ look  at T vs. psi_leaf
# measured
ggplot(sub, aes(x=mean_psi_MPa, y=mean_T_mg_s, color=treatment)) + geom_point() + geom_smooth(method = 'lm', formula = y ~ poly(x,2))
summary(lm(mean_T_mg_s~poly(mean_psi_MPa,2) + treatment, dat[!is.na(dat$mean_psi_MPa),]))

# modeled
ggplot(sub, aes(x=predicted_psi_leaf, y=mean_T_mg_s, color=treatment)) + geom_point() #+ geom_smooth(method = 'lm')
summary(lm(mean_T_mg_s~poly(predicted_psi_leaf,2) + treatment, dat[!is.na(dat$predicted_psi_leaf),]))

# INTERESTING!  SEE DIFF. REPSONSES TO LIGHT/TEMPERATURE BY TREATMENT....
# LOOKS LIKE Transp. tapers off in response to temp/light in M/D treatments but not Wet.
sub2 <- subset(sub, block == 'M' &date >= '2019-10-10' & date <='2019-11-06')
plot(line_PAR_west_umol_m2_s~by15, type='l',data=sub2, main='moderate trt')
lines(mean_T_mg_s*5~by15, type='l', col='blue', data=sub2)
lines(temp_high_mean*10~by15, type='l', col='red', data=sub2)

sub2 <- subset(sub, block == 'W' &date >= '2019-10-10' & date <='2019-11-06')
plot(line_PAR_west_umol_m2_s~by15, type='l',data=sub2, main='wet trt')
lines(mean_T_mg_s*5~by15, type='l', col='blue', data=sub2)
lines(temp_high_mean*10~by15, type='l', col='red', data=sub2)


sub2 <- subset(sub, block == 'M' &date >= '2019-10-10' & date <='2019-11-06')
plot(mean_T_mg_s~by15, type='l',data=sub2, main='moderate trt',col='blue')
lines(soil_temp_C~by15, type='l', col='black', data=sub2)
lines(temp_high_mean~by15, type='l', col='red', data=sub2)

sub2 <- subset(sub, date >= '2019-11-04' & date <='2019-11-14')
ggplot(sub2, aes(x=by15, y=soil_temp_C, color=block)) + geom_line()
