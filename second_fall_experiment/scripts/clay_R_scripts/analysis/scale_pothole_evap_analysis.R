
rm(list = ls())
wd <- "/home/wmsru/github/2020_greenhouse/second_fall_experiment/data/scale_output/read_only/post_experiment_data/"
setwd(wd)

Sys.setenv(tz='GMT')

require(ggplot2)
require(lubridate)
require(plyr) 
require(dplyr)
require(tidyr)

files <- dir(wd)
files <- files[!grepl('_saturation', files)]
files


datList <- lapply(files, function(x) read.csv(x, header = F, skipNul = F, stringsAsFactors = F))
names(datList) <- files
for(i in 1:length(datList)) {
  datList[[i]][,4] <- files[i]
  colnames(datList[[i]]) <- c('scale','timestamp','weight','file')
  datList[[i]]$timestamp <- as.POSIXct(datList[[i]]$timestamp, format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
}


### ------Preliminary combine data and check for duplicate rows
scaledat <- do.call(rbind, datList)
rownames(scaledat) <- NULL



### ------ Change data types and further data "massaging"
scaledat$scale <- factor(scaledat$scale)

# order by scale and then time
scaledat <- scaledat[order(scaledat$scale, scaledat$timestamp), ]

# omit data before 11:58 on first day, scales were not all on pots till then
scaledat <- subset(scaledat, timestamp > "2019-12-13 11:58")

# remove file
scaledat$file=NULL

### add variable for covered vs. uncovered
scaledat$holes_covered[scaledat$scale %in% c(1,2,3,6,7,10,12,14,15,16)] <- 'y'
scaledat$holes_covered[scaledat$scale %in% c(4,5,8,9)] <- 'n'
table(scaledat$holes_covered, scaledat$scale, useNA = 'always')



### Remove 1st day, has some funky stuff going on
# scales 8, 10 have weights near zero
# s8 = subset(scaledat, scale == 8) # only 1st obs
# s10 = subset(scaledat, scale == 10)
scaledat <- subset(scaledat, date(timestamp) > '2019-12-13')







# plot all data
ggplot(scaledat, aes(x=timestamp, y=weight, color=scale)) +
  geom_line() +
  scale_x_datetime(date_labels = "%H", breaks = '1 hours') +
  ylim(c(12,16))

# plot subset
sub <- subset(scaledat, date(timestamp) <= "2019-12-16")
ggplot(sub, aes(x=timestamp, y=weight, color=scale)) +
  geom_line() +
  scale_x_datetime(date_labels = "%H", breaks = '6 hours') +
  facet_wrap(~holes_covered)
# 
# # check for missing data
# by(scaledat, scaledat$scale, function(x) length(which(is.na(x$weight))))
# 
# # scale 3 drops out after 12-15 05:57
# s=subset(scaledat, scale==3)
# i=which(is.na(s$weight)); summary(i)
# s$timestamp[i[1]]
# plot(weight~timestamp, s[(i[1]-1000):nrow(s),])
# 
# # scale 6 data is crap before 12-13 15:19
# s=subset(scaledat, scale==6)
# i=which(is.na(s$weight)); summary(i)
# s$timestamp[130]; s$timestamp[598]
# plot(weight~timestamp, s)


# round to nearest 30-seconds
scaledat$timestamp <- round_date(scaledat$timestamp, unit = '30 seconds')

dups_ind <- duplicated(scaledat[ ,c('timestamp','scale')])
length(which(dups_ind))

# small fraction of duplicates, delete these rows
scaledat <- scaledat[!dups_ind, ]


### OMIT SCALE 3, MOST OF DATA MISSING ==================
scaledat <- subset(scaledat, scale  != 3)


scaledat$by15 <- floor_date(scaledat$timestamp, unit = '15 minutes')


scaledat$date <- date(scaledat$timestamp)
head(scaledat)

# calculate 15-min means
scaledatMeans = ddply(scaledat, .(scale, by15, date, holes_covered), function(x) {
  setNames(mean(x$weight, na.rm=T), 'weight')
})
head(scaledatMeans)


### standardize weights 
minwt <- by(scaledatMeans, scaledatMeans$scale, function(x) min(x$weight, na.rm = T))
for(s in names(minwt)) {
  scaledatMeans$weight[scaledatMeans$scale == s] <- scaledatMeans$weight[scaledatMeans$scale == s] - minwt[s]
}

summary(scaledatMeans$weight)

# plot 15-minute means
ggplot(subset(scaledatMeans, date <= '2019-12-16'), aes(x=by15, y=weight, color=scale)) +
  geom_line()
ggplot(scaledatMeans, aes(x=by15, y=weight, color=scale)) +
  geom_line()

# simplest approach: fit 2 lines through data 
m1 = lm(weight ~ by15 + holes_covered, scaledatMeans)
summary(m1)
m1$coefficients['holes_coveredy'] # on avg, covered hole pots lose 78 mL less (over study period)
length(unique(scaledatMeans$date)) # days in study period
78/18 # so they lose 4 ml per day less
  
# daily max minus min (NOT really correct since diurnal fluctuations exist)
# scaledatDiffs = ddply(scaledatMeans, .(scale, date, holes_covered), function(x) {
#   setNames(max(x$weight, na.rm=T) - min(x$weight, na.rm=T), 'daily_diff')
# })
# head(scaledatDiffs)
x = subset(scaledatMeans, scale == 1 & date == '2019-12-15')
summary(x)

maxdiff <- ddply(scaledatMeans, .(date, scale, holes_covered), function(x) {
  today = unique(x$date)
  if(today == '2019-12-14') 
    out = NA
  else {
    yesterday_max = max(scaledatMeans$weight[scaledatMeans$date == (today-1)], na.rm = T)
    out = yesterday_max - max(x$weight, na.rm = T)
  }
  return(setNames(out, 'max_daily_diff'))
})
maxdiff <- maxdiff[complete.cases(maxdiff),]

mindiff <- ddply(scaledatMeans, .(date, scale, holes_covered), function(x) {
  today = unique(x$date)
  if(today == '2019-12-14') 
    out = NA
  else {
    yesterday_min = min(scaledatMeans$weight[scaledatMeans$date == (today-1)], na.rm = T)
    out = yesterday_min - min(x$weight, na.rm = T)
  }
  return(setNames(out, 'min_daily_diff'))
})
mindiff <- mindiff[complete.cases(mindiff),]

# mean daily diff
summary(maxdiff$max_daily_diff)
summary(mindiff$min_daily_diff)

### Significant BUT TINY effect of covering holes
mod <- lm(max_daily_diff ~ holes_covered, maxdiff)
summary(mod); anova(mod)

### There is just as much variation between scales as there is in covering holes!
mod <- lm(daily_diff ~ scale, scaledatDiffs)
summary(mod); anova(mod)



