

shifted = dat
par = select(dat, c(by15, line_PAR_mean_umol_m2_s))
head(par$by15)
par$by15 = par$by15 - 3600
head(par$by15)
nonpar = select(dat, -line_PAR_mean_umol_m2_s)
shifted = merge(nonpar, par, by = 'by15', all = T)

mydate = '2019-12-07'
myplant = 'D-6'

# non-shifted plot
sub = subset(dat, plant_id == myplant & date == mydate)
plot(line_PAR_mean_umol_m2_s/10 ~ by15, sub, col='red', type='b', 
     main = paste(myplant, mydate, sep = ', '))
lines(T_mg_m2_s ~ by15, sub)
points(T_mg_m2_s ~ by15, sub)
abline(v=seq.POSIXt(min(sub$by15), max(sub$by15), '1 hour'), lty='dotted')
cor(sub$T_mg_m2_s, sub$line_PAR_mean_umol_m2_s, use = 'complete.obs')
m <- lm(T_mg_m2_s ~ line_PAR_mean_umol_m2_s, sub); summary(m)
plot(sub$line_PAR_mean_umol_m2_s, sub$T_mg_m2_s, main = paste(myplant, mydate, sep = ', '))
abline(m$coef[1], m$coef[2])
m <- lm(T_mg_m2_s ~ poly(line_PAR_mean_umol_m2_s, 2), sub); summary(m)


# shifted plot
# sub = subset(shifted, plant_id == myplant & date == mydate)
# plot(line_PAR_mean_umol_m2_s/10 ~ by15, sub, col='red', type='b', 
#      main = paste('shifted', unique(sub$date)))
# lines(T_mg_m2_s ~ by15, sub)
# points(T_mg_m2_s ~ by15, sub)
# abline(v=seq.POSIXt(min(sub$by15), max(sub$by15), '1 hour'), lty='dotted')


by(dat, dat$plant_id, function(x) length(which(is.na(x$line_PAR_mean_umol_m2_s)))/nrow(x))

                          
### -- Modeling

# only keep data with not NA
sub = subset(dat, !(is.na(line_PAR_mean_umol_m2_s) | is.na(T_mg_m2_s)))

# only middday
sub = periodList$`midday (11-14)`
sub = subset(sub, !(is.na(line_PAR_mean_umol_m2_s) | is.na(T_mg_m2_s)))

sub = subset(dat, minutes >= 11*60 & minutes <= 14*60)


# change levels of treatment
dat$treatment <- factor(dat$treatment, levels = c('well_watered','moderate_drought','virgin_drought','full_drought'))

dat$treatment <- relevel(dat$treatment, 'well_watered')

levels(dat$treatment) <- c('well_watered','moderate_drought','virgin_drought','full_drought')


# simple model with PAR only
m1 = lm(T_mg_m2_s ~ line_PAR_mean_umol_m2_s + VPD_air_low, sub); summary(m1)

# quadratic
m2 = lm(T_mg_m2_s ~ poly(line_PAR_mean_umol_m2_s, 2), sub); summary(m2)

# treatment interaction, R2=0.62
m_int = lm(T_mg_m2_s ~ line_PAR_mean_umol_m2_s*treatment, sub); summary(m_int)
m_int = lm(T_mg_m2_s ~ line_PAR_mean_umol_m2_s*soil_water_potential_kPa, sub); summary(m_int)

# add period
m_int = lm(T_mg_m2_s ~ line_PAR_mean_umol_m2_s*soil_water_potential_kPa, sub); summary(m_int)
m_int = lm(T_mg_m2_s ~ line_PAR_mean_umol_m2_s*soil_water_potential_kPa+period, sub); summary(m_int)

# treatment interaction + quadratic
m_int2 = lm(T_mg_m2_s ~ poly(line_PAR_mean_umol_m2_s, 2)*treatment, sub); summary(m_int2)
m_int2 = lm(T_mg_m2_s ~ poly(line_PAR_mean_umol_m2_s, 2)*soil_water_potential_kPa, sub); summary(m_int2)


## What up with wind?
wind <- dat %>% select('date','by15', 'block', starts_with('wind'))
wind <- pivot_longer(wind, cols = -c(date, by15, block), names_to = 'position', values_to = 'velocity_m_s')
subwind=wind
subwind = subset(wind, date == '2019-11-01')

# plot 1 day, diff hts
ggplot(subwind, aes(x=by15, y=velocity_m_s, color=position)) +
  geom_line()

# plot 1 day, diff blocks
subwind = subset(wind, date >= '2019-11-04' & date <= '2019-11-10' & position == 'windspeed_top')
ggplot(subwind, aes(x=by15, y=velocity_m_s, color=block)) +
  geom_line()

#--- Explore linear correlations
nm <- names(dat)
nm <- nm[!nm%in%c('by15','date','block','treatment','day','minutes','period','irrig','plant_id')]
nm
corr <- sapply(nm, function(x) cor(dat[[x]], dat$windspeed_mean, use = 'complete.obs'))
ss <- sapply(nm, function(x) { # get sample sizes
  cc = complete.cases(dat[,c(x,'windspeed_mean')])
  length(cc[cc])
})
cormat <- data.frame(r=round(corr[order(corr, decreasing = T)],2),
                     n=ss[order(corr, decreasing = T)])
cormat
