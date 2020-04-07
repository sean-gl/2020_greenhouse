# Clayton Bliss, last revision: 2/17/2015
# Import raw output data from lab scales (.csv file) and graph it

library(ggplot2)
library(lubridate)
library(reshape2)
library(scales)
library(doBy)

theme_set(theme_classic(base_size = 20))  # change plot theme & font size
setwd("Y:/WMU/STAFF FOLDERS/Gleason, Sean/Greenhouse Studies/Fall 2015/Greenhouse_Scales_Data/Data")

#pdf(height=9, width=11)

#plotWindow <- 60*48 # plot window in minutes

# find all files and combine into sorted data frame
files <- list.files(path="Y:/WMU/STAFF FOLDERS/Gleason, Sean/Greenhouse Studies/Fall 2015/Greenhouse_Scales_Data/Data",
                           pattern="scale_output_2016.*\\.csv")
for (i in seq_along(files)) {
  # If first file in files, just import. Else, import & combine.
  if (i==1) {
    d <- read.csv(files[i], header = F, col.names = c('scale', 'date_time', 'kg'),
              colClasses = c('factor', 'character', 'character'))
  } 
  else {
      d <- rbind(d, read.csv(files[i], header = F, col.names = c('scale', 'date_time', 'kg'),
              colClasses = c('factor', 'character', 'character')))
    }
  # sort by date
  #d <- d[order(d$date_time),]
}

# remove bad data (prior to Jan 23rd, midnight)
d <- subset(d, d$date_time > "2016-01-23 00:00")

d$kg <- as.numeric(d$kg)
d$date_time <- as.POSIXct(d$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver")
#d$time <- strftime(d$date_time, "%H:%M")
d$line[d$scale==1 | d$scale==3 | d$scale==5 | d$scale==8] <- 'B73'
d$line[d$scale==2 | d$scale==4 | d$scale==6 | d$scale==7] <- 'CML103'

# for pretreatment dry-down
d$tx <- 'dry'
d$tx[d$scale==2 | d$scale==5 | d$scale==6] <- 'wet'

# for when treatments begin
# d$tx[d$scale==1 | d$scale==3 | d$scale==4 | d$scale==7] <- 'dry'
# d$tx[d$scale==2 | d$scale==5] <- 'mid'
# d$tx[d$scale==6 | d$scale==8] <- 'wet'

d$line <- as.factor(d$line)
d$tx <- as.factor(d$tx)

## REMOVE OUTLIERS (DOENS'T WORK RIGHT)
# # add "diff" column, showing weight difference by scale
# d <- transformBy(~scale, data=d, diff=c(NA,diff(kg)))
# which(na.exclude(sub$diff > .5))
# sub[which(na.exclude(sub$diff > .5))-1, "kg"] <- NA
# sub <- sub[which(complete.cases(sub)),]

# remove two outlier points (**I put emitter in plant and weight spiked)
d$kg[which(d$kg > 14 & d$scale == 6) + 8] <- NA
d$kg[which(d$kg > 14 & d$scale == 6)] <- NA
d$kg[which(d$kg[day(d$date_time)==18 & d$scale==1] == max(d$kg[day(d$date_time)==18 & d$scale==1]))] <- NA
d <- d[which(complete.cases(d)),]

# plot all data
ggplot(d, aes(x = date_time, y = kg, by = scale, color = scale)) +
  geom_point(size = 2) +
  scale_x_datetime(labels = date_format("%m-%d"), breaks="2 days") +
  coord_cartesian(ylim = c(0, 15)) 

# remove tail end of data (today's download)
d <- subset(d, d$date_time < "2016-02-18 00:00")

# remove days we hand watered them
d <- subset(d, day(d$date_time) != 10 & day(d$date_time) != 14)

# examine ONLY day we watered them (watering time end = 11:45)
d <- subset(d, d$date_time > "2016-02-10 11:00" & d$date_time < "2016-02-10 12:00")

# subset between 8:50 am & 3:50 pm 
sub <- subset(d, (hour(d$date_time)*60 + minute(d$date_time) >= 530) & (hour(d$date_time)*60 + minute(d$date_time) <= 950))

# subset pre- and post-emitter removal
sub <- subset(sub, sub$date_time < "2016-02-03 16:00")
sub <- subset(d, d$date_time >= "2016-02-03 19:00" & d$date_time <= "2016-02-11 19:00")

# subset afternoon watering only (and 1-minute data)
sub <- subset(d, d$date_time >= "2016-02-08 15:30")
sub <- subset(sub, (hour(sub$date_time)*60 + minute(sub$date_time) >= 930) & (hour(sub$date_time)*60 + minute(sub$date_time) <= 990))

# plot subset of data
ggplot(sub, aes(x = date_time, y = kg, by = scale, color = scale)) +
  geom_point(size = 2)


############# COMPUTE DAILY SLOPE OF WATER LOSS #############

dates <- unique(day(sub$date_time))
# B73 <- sub[sub$line == 'B73', ]
# CML103 <- sub[sub$line == 'CML103', ]
nrows <- length(dates) * length(unique(sub$scale))
slopes <- data.frame(date= integer(nrows), scale= integer(nrows), tx = character(nrows),
                     slope = numeric(nrows), rsq = numeric(nrows))
slopes$date <- as.Date(slopes$date, origin = "1970-01-01")
slopes$tx <- as.character(slopes$tx)
row <- 1
for (date in dates) {
  dailyData <- subset(sub, day(sub$date_time) == date)
  for (scale in unique(dailyData$scale)) {
    scaleData <- dailyData[dailyData$scale == scale,]
    fit <- lm(scaleData$kg ~ scaleData$date_time)
    slopes$date[row] <- as.Date(scaleData$date_time[1])
    slopes$scale[row] <- scale
    slopes$tx[row] <- as.character(scaleData$tx[1])
    slopes$slope[row] <- coefficients(fit)[2]
    slopes$rsq[row] <- cor(scaleData$kg, as.numeric(scaleData$date_time))
    row <- row + 1
  }
}
slopes$tx <- as.factor(slopes$tx)
# convert from kg per second to g per hour (1000 * 60 sec. * 60 min.)
slopes$g.per.hr <- abs(3600000*slopes$slope)
# convert from from kg per second to kg per day (daytime loss only)
slopes$line[slopes$scale==1 | slopes$scale==3 | slopes$scale==5 | slopes$scale==8] <- 'B73'
slopes$line[slopes$scale==2 | slopes$scale==4 | slopes$scale==6 | slopes$scale==7] <- 'CML103'
by(slopes$g.per.hr, slopes$tx, summary)
ggplot(slopes, aes(x = date, y = g.per.hr, by=scale, color=tx)) +
  geom_point(size = 2) +
  geom_line() +
  scale_x_date(breaks=seq(slopes$date[1], slopes$date[length(slopes$date)], 2),
               labels = date_format("%m/%d"))

# if the above line gives "Error in .Call.graphics(C_palette2,...." then run this line:
#dev.off()

boxplot(slopes$g.per.hr ~ slopes$scale, main='Post 6')
boxplot(slopes$g.per.hr ~ slopes$tx, main='Post 6')
boxplot(slopes$g.per.hr ~ slopes$line, main='Post 6')

#############################################################

############# COMPUTE DAILY ABSOLUTE WATER LOSS #############

dates <- unique(day(sub$date_time))
# B73 <- sub[sub$line == 'B73', ]
# CML103 <- sub[sub$line == 'CML103', ]
nrows <- length(dates) * length(unique(sub$scale))
loss <- data.frame(date= integer(nrows), scale= integer(nrows), tx = character(nrows), loss = numeric(nrows))
loss$date <- as.Date(loss$date, origin = "1970-01-01")
loss$tx <- as.character(loss$tx)
row <- 1
for (date in dates) {
  dailyData <- subset(sub, day(sub$date_time) == date)
  for (scale in unique(dailyData$scale)) {
    scaleData <- dailyData[dailyData$scale == scale,]
    loss$date[row] <- as.Date(scaleData$date_time[1])
    loss$scale[row] <- scale
    loss$tx[row] <- as.character(scaleData$tx[1])
    loss$loss[row] <- max(scaleData$kg, na.rm=T) - min(scaleData$kg, na.rm=T)
    row <- row + 1
  }
}
loss$tx <- as.factor(loss$tx)
loss$scale <- as.factor(loss$scale)
loss$line[loss$scale==1 | loss$scale==3 | loss$scale==5 | loss$scale==8] <- 'B73'
loss$line[loss$scale==2 | loss$scale==4 | loss$scale==6 | loss$scale==7] <- 'CML103'
by(loss$loss, loss$tx, summary)
tx_days <- as.numeric(as.Date(c("2016-02-03", "2016-02-08", "2016-02-10", "2016-02-14")))
labels <- c("\n emitters removed", "\n emitter added to scale 6", "\n hand watered", "\n hand watered")
ggplot(loss, aes(x = date, y = loss, by=scale, color=scale)) +
  geom_point(size = 2) + 
  geom_line() +
  theme(axis.title.x = element_text(face="bold", size=17),
        axis.title.y = element_text(face="bold",size=17)) +
  labs(x="Date", y="Daily water loss, kg, 08:50-15:50") +
  scale_x_date(breaks=seq(loss$date[1], loss$date[length(loss$date)], 2),
               labels = date_format("%m/%d")) +
  geom_vline(xintercept=tx_days) +
  geom_text(aes(x=as.Date(tx_days[1]), label=labels[1], y=0.12), colour="black", angle=90, text=element_text(size=1)) +
  geom_text(aes(x=as.Date(tx_days[2]), label=labels[2], y=0.12), colour="black", angle=90, text=element_text(size=1)) +
  geom_text(aes(x=as.Date(tx_days[3]), label=labels[3], y=0.12), colour="black", angle=90, text=element_text(size=1)) +
  geom_text(aes(x=as.Date(tx_days[4]), label=labels[4], y=0.12), colour="black", angle=90, text=element_text(size=1))

# FIND DIFFERENCE IN LOSS BETWEEN DAYS (WORK IN PROGRESS) ###

loss.by <- by(loss$loss, loss$scale, diff)
loss.trend <- data.frame(date = unique(loss$date))
for (i in as.integer(unlist(dimnames(loss.by)))) {
  loss.trend[ , i+1] <- c("NA", unlist(loss.by[i]))
  colnames(loss.trend)[i+1] <- i
}
loss.trend <- melt(loss.trend, id.vars="date",variable.name="scale", value.name="lossDiff")
loss.trend$lossDiff <- as.numeric(loss.trend$lossDiff)

loss.trend$tx[loss.trend$scale==2 | loss.trend$scale==5] <- 'wet'
loss.trend$tx[loss.trend$scale!=2 & loss.trend$scale!=5] <- 'dry'

ggplot(loss.trend, aes(x = date, y = lossDiff, by=scale, color=tx)) +
  geom_line() +
  geom_point(size = 2)  

#############

# bar plot
qplot(x=date, y=loss, fill=tx, data=loss, geom="bar", stat="identity",
      position="dodge")

# if the above line gives "Error in .Call.graphics(C_palette2,...." then run this line:
#dev.off()

boxplot(loss$loss ~ loss$scale, main='Post')
boxplot(loss$loss ~ loss$tx, main='Post')
boxplot(loss$loss ~ loss$line, main='Post')


## WORK IN PROGRESS
# Find slope of water loss across time

loss.trend <- data.frame(scale = integer(8), tx = character(8), slope = numeric(8))
row <- 1
loss.trend$tx <- as.character(loss.trend$tx)


for (scale in unique(loss$scale)) {
  scaleData <- loss[loss$scale == scale, ]
  fit <- lm(scaleData$loss ~ scaleData$date)
  loss.trend$scale[row] <- scale
  loss.trend$tx[row] <- as.character(scaleData$tx[1])
  loss.trend$slope[row] <- coefficients(fit)[2]
  row <- row + 1
}

loss.trend.trend$tx <- as.factor(loss.trend.trend$tx)
loss.trend$scale <- as.factor(loss.trend$scale)
loss.trend$line[loss.trend$scale==1 | loss.trend$scale==3 | loss.trend$scale==5 | loss.trend$scale==8] <- 'B73'
loss.trend$line[loss.trend$scale==2 | loss.trend$scale==4 | loss.trend$scale==6 | loss.trend$scale==7] <- 'CML103'
by(loss.trend$slope, loss.trend$scale, summary)
ggplot(loss.trend, aes(x = date, y = slope, by=scale, color=tx)) +
  geom_point(size = 2) + 
  geom_line(size = 1)

#############################################################

############# COMPUTE DAILY ABSOLUTE WATER GAIN  #############

dates <- unique(day(sub$date_time))
# B73 <- sub[sub$line == 'B73', ]
# CML103 <- sub[sub$line == 'CML103', ]
nrows <- length(dates) * length(unique(sub$scale))
gain <- data.frame(date= integer(nrows), scale= integer(nrows), tx = character(nrows), gain = numeric(nrows))
gain$date <- as.Date(gain$date, origin = "1970-01-01")
gain$tx <- as.character(gain$tx)
row <- 1
for (date in dates) {
  dailyData <- subset(sub, day(sub$date_time) == date)
  for (scale in unique(dailyData$scale)) {
    scaleData <- dailyData[dailyData$scale == scale,]
    gain$date[row] <- as.Date(scaleData$date_time[1])
    gain$scale[row] <- scale
    gain$tx[row] <- as.character(scaleData$tx[1])
    gain$gain[row] <- max(scaleData$kg) - min(scaleData$kg)
    row <- row + 1
  }
}
gain$tx <- as.factor(gain$tx)
gain$scale <- as.factor(gain$scale)
gain$line[gain$scale==1 | gain$scale==3 | gain$scale==5 | gain$scale==8] <- 'B73'
gain$line[gain$scale==2 | gain$scale==4 | gain$scale==6 | gain$scale==7] <- 'CML103'
by(gain$gain, gain$scale, summary)
ggplot(gain, aes(x = date, y = gain, by=scale, color=scale)) +
  geom_point(size = 2) + 
  geom_line(size = 1)

# bar plot
qplot(x=date, y=gain, fill=tx, data=gain, geom="bar", stat="identity",
      position="dodge")

# if the above line gives "Error in .Call.graphics(C_palette2,...." then run this line:
#dev.off()

# Are scales 2 and 5 differnet than others, before and after Tx?
summary(gain$gain)
summary(gain$gain[gain$tx == "dry"])
summary(gain$gain[gain$tx == "wet"])

gain.pre <- gain[gain$date < "2016-02-04", ]
gain.post <- gain[gain$date >= "2016-02-04", ]

# mean daily water gain by tx
by(gain$gain, gain$tx, mean)
# mean daily water gain by tx, pre-tx
by(gain.pre$gain, gain.pre$tx, mean)
# mean daily water gain by tx, post-tx
by(gain.post$gain, gain.post$tx, mean)
# difference between post and pre tx mean daily water gain
by(gain.post$gain, gain.post$tx, mean) - by(gain.pre$gain, gain.pre$tx, mean)

# subset gain by pre-tx dates onlyl
gain <- gain[gain$date < "2016-02-03", ]

boxplot(gain$gain ~ gain$scale, main = "AM water gain, kg")
boxplot(gain$gain/0.1892705 ~ gain$scale, main = "AM water gain, gal/hr")
boxplot(gain$gain ~ gain$line, main = "AM water gain, kg")

#############################################################


# # verify that subset contains all maxima/minima of original dataset
# by(sub$kg, sub$scale, min) == by(d$kg, d$scale, min)
# by(sub$kg, sub$scale, max) == by(d$kg, d$scale, max)

# daily water loss, averaged by genotype
dates <- unique(day(sub$date_time))
B73 <- sub[sub$line == 'B73', ]
CML103 <- sub[sub$line == 'CML103', ]
diffs <- data.frame(day = dates)
for (day in dates) {
  #print(day)
  b73 <- (by(B73$kg[day(B73$date_time)==day], B73$scale[day(B73$date_time)==day], max)
        - by(B73$kg[day(B73$date_time)==day], B73$scale[day(B73$date_time)==day], min))
  cml103 <- (by(CML103$kg[day(CML103$date_time)==day], CML103$scale[day(CML103$date_time)==day], max)
        - by(CML103$kg[day(CML103$date_time)==day], CML103$scale[day(CML103$date_time)==day], min))
  diffs$geno.diff[diffs$day == day] <- mean(b73, na.rm=T) - mean(cml103, na.rm=T)
  diffs$b73[diffs$day == day] <- mean(b73, na.rm=T)
  diffs$cml103[diffs$day == day] <- mean(cml103, na.rm=T)
}