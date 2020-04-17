# 
rm(list = ls())
packages <- c('lubridate','plyr','ggplot2','tidyr')
lapply(packages, require, character.only = TRUE)
Sys.setenv(tz='GMT')

# read in raw scale data
dat <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/scale_data_raw_compiled_long.rds')

dat$file <- NULL
# don't care about border plants
dat <- subset(dat, !grepl('border', plant_id))
# 
dat$hour <- hour(dat$timestamp)

# x=with(dat, table(date, block))
# which(x[,1]==0)
# x[,1]
# y=with(dat, table(date, scale))
# y


# NOTE: irrigation treatments actually start at 18:00 on 10/23 (NOT 10/24)
# x =subset(dat, scale== 12 & date >= '2019-10-20' & date <= '2019-10-25')
# plot(weight~timestamp, data=x)
# identify(x$timestamp, x$weight, labels = x$timestamp, pos=4)

# test case
x = subset(dat, scale== 1 & date == '2019-11-04')
options(warn = 1) # print warnings as occur
# dat=dat[dat$scale==1,]
irrigAmt <- ddply(dat, .(scale, date, plant_id, block, treatment), function(x) {
  
  # print(head(x))
  
  # first, determine the hour in which plants were irrigated
  dt <- as.Date(unique(x$date))

  print(paste0('date = ', dt))
  print(paste0('scale = ', unique(x$scale)))
  
  if(dt < '2019-10-24') {
    irrig_hr <- 0
  } else if(dt < '2019-10-29') {
     irrig_hr <- 18
  } else if(dt < '2019-11-28') {
    irrig_hr <- 19
  } else irrig_hr <- 20
  
  # for zero hour irrigation, we use the same day; for all others we use the previous day
  if(irrig_hr == 0) {
    d <- subset(dat, scale == unique(x$scale) & date %in% c(dt, dt-1))
    target <- d$timestamp[d$date == dt & d$hour == 0][1]
  } else {
    d <- subset(dat, scale == unique(x$scale) & date %in% c(dt-1, dt-2))
    target <- d$timestamp[d$date == (dt-1) & d$hour == irrig_hr][1]
  }
  
  # if target cant be found, return NA
  if(is.na(target)) return(NA)
  
  target.ind <- which(d$timestamp == target)
  # get 20 minutes (= 40 measurements) on either side of target
  d <- d[(target.ind - 40):(target.ind + 100), ]
  
  # plot(d$timestamp, d$weight, main = paste0('scale ', unique(x$scale), ',  ', dt))
  # identify(d$timestamp, d$weight)
  
  # now, identify actual watering point, based on differences in wt.
  d$diff <- c(diff(d$weight), NA)
  rl <- rle(d$diff >= 0.003)
  # rl$lengths; rl$values
  imax <- max(rl$lengths[which(rl$values)]) # get the largest increasing run length
  ind <- which(rl$lengths == imax & rl$values) # find index of imax in all runs
  # rl$values[ind]; rl$lengths[ind] # check 
  start <- sum(rl$lengths[1:(ind-1)]) + 1
  end <- start + imax
  
  plot(d$timestamp, d$weight, main = paste0('scale ', unique(d$scale), ',  ', dt))
  points(weight ~ timestamp, data = d[start:end,], col = 'red', lwd = 3)
  
  # --- OLD METHOD ---
  # iPoints <- which(d$diff > 0.01) # increasing points
  # # iPoints
  # start <- iPoints[1] 
  # end <- iPoints[length(iPoints)] + 2 # skip 2 to be safe
  # get mean values for before/after watering
  # d$weight[(start-5):start]
  # d$weight[end:(end+5)]
  start_mean <- mean(d$weight[(start-3):start], na.rm = T)
  # end_mean <- mean(d$weight[end:(end+5)], na.rm = T)
  
  irrig <- d$weight[end] - start_mean
  
  # plot(d$timestamp, d$weight)
  # identify(d$timestamp, d$weight)
  # irrig <- end_mean - start_mean
  
  return(irrig)
  
})

names(irrigAmt)[names(irrigAmt)=='V1'] <- 'irrig_kg'

# values < 0.1 are errors
# irrigAmt$irrig_kg[irrigAmt$irrig_kg < 0.1] <- NA

ggplot(irrigAmt, aes(x=date, y=irrig_kg, color=scale)) + geom_point() + geom_line() + facet_wrap(~block)

# save irrigation data
saveRDS(irrigAmt, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/analysis/clay/output_data/irrigation_by_plant.rds')

# read back in
# irrigAmt <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/analysis/clay/output_data/irrigation_by_plant.rds')

# fill in NA with next or previous value, see below
naDat <- irrigAmt[is.na(irrigAmt$irrig_kg),]
table(naDat$date, naDat$block)

x=subset(dat, date>='2019-10-21' & date<='2019-10-24' & scale == 1)
plot(weight~timestamp, x)
identify(x$timestamp, x$weight, labels = x$timestamp, pos=4)
# period 1: 9/13-10/16 (630 ml/day), at midnight (same day)
# period 2: 10/17-10/23 (increased to 1 l/day)
# per 3: trt start 10/24 (750 ml/day), watering on previous day now
# per 4: 10/30 to end (1 l/day)

# for 10-23 use 10-22; for all other dates, use next available date
ind <- which(is.na(irrigAmt$irrig_kg))
for(i in ind) {
  d <- irrigAmt$date[i]
  if(d == '2019-10-23') {
    wt <- irrigAmt$irrig_kg[irrigAmt$date == d-1 & irrigAmt$scale==irrigAmt$scale[i]]
  } else {
    wt <- irrigAmt$irrig_kg[irrigAmt$date==d+1 & irrigAmt$scale==irrigAmt$scale[i]]
  }
  if(is.na(wt)) wt <- irrigAmt$irrig_kg[irrigAmt$date==d+2 & irrigAmt$scale==irrigAmt$scale[i]]
  irrigAmt$irrig_kg[i] <- wt
}
irrigAmt[ind,]
any(is.na(irrigAmt$irrig_kg))
ggplot(irrigAmt, aes(x=date, y=irrig_kg, color=scale)) + geom_point() + geom_line() + facet_wrap(~block)

# re-save irrigation data
saveRDS(irrigAmt, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/analysis/clay/output_data/irrigation_by_plant.rds')


# get means by block
irrigMeans <- ddply(irrigAmt, .(date, block, treatment), function(x) {
  setNames(mean(x$irrig_kg, na.rm = T), 'mean_irrig_kg')
})
with(irrigMeans, table(date, block))

# now, fill in missing dates/scales (no data available)
im <- tidyr::complete(irrigMeans, date, nesting(block))

# but we need to fill in treatments
im[is.na(im$treatment),]
im$treatment[im$date < '2019-10-24' & is.na(im$treatment)] <- 'full_drought'
im[is.na(im$treatment),]
im$treatment[im$date > '2019-10-24' & is.na(im$treatment) & im$block == 'M'] <- 'moderate_drought'
im$treatment[im$date > '2019-10-24' & is.na(im$treatment) & im$block == 'W'] <- 'well_watered'
im$treatment[im$date > '2019-10-24' & is.na(im$treatment) & im$block == 'D'] <- 'full_drought'
any(is.na(im$treatment))

# finally, fill in missing irrigation using imputeTS

ggplot(im, aes(x=date, y=mean_irrig_kg, color=block)) + geom_point()

require(imputeTS)
w <- subset(im, block=='W')
w.vec <- w$mean_irrig_kg
w.fill = na_interpolation(w.vec)
plot(w.vec, type='l'); points(w.fill, col = 'red')
identify(w.vec)

m <- subset(im, block=='M')
m.vec <- m$mean_irrig_kg
m.fill = na_interpolation(m.vec)
plot(m.vec, type = 'l'); points(m.fill, col = 'red')
identify(m.vec)

d <- subset(im, block=='D')
d.vec <- d$mean_irrig_kg
d.fill = na_interpolation(d.vec)
plot(d.vec, type='l'); points(d.fill, col = 'red')
identify(d.vec)

# manually fill some poorly interpolated values
d.fill[32:36]
d.fill[33:35] <- d.vec[36]
d.fill[48:50]
d.fill[49] <- d.vec[48]

# recombine data
w$mean_irrig_kg <- w.fill
m$mean_irrig_kg <- m.fill
d$mean_irrig_kg <- d.fill
im2 <- rbind(w, m, d)
any(is.na(im2)) # no NAs


# lastly, compute running mean irrigation by 



# save complete irrigation data (by block)
saveRDS(im2, '/home/wmsru/github/2020_greenhouse/second_fall_experiment/analysis/clay/output_data/mean_irrigation_by_block.rds')
# im2 <- readRDS('/home/wmsru/github/2020_greenhouse/second_fall_experiment/analysis/clay/output_data/mean_irrigation_by_block.rds')
