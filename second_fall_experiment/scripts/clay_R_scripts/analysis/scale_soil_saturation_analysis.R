
rm(list = ls())
wd <- "/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/scale_output/read_only/post_experiment_data/"
setwd(wd)

Sys.setenv(tz='GMT')

require(ggplot2)
require(lubridate)
require(plyr) 
require(dplyr)
require(tidyr)

files <- dir(wd)
files <- files[grepl('_saturation', files)]
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
anyDuplicated(scaledat[,c('scale','timestamp')])



### ------ Change data types and further data "massaging"
scaledat$scale <- factor(scaledat$scale)

scaledat$file = NULL
# NOTE: these are blocks, not treatments!!
# scaledat$date <- date(scaledat$timestamp)
# scaledat$hour <- hour(scaledat$timestamp)
# scaledat$minute <- minute(scaledat$timestamp)
# scaledat$timeofday <- scaledat$hour * 60 + scaledat$minute

# plot all data
ggplot(scaledat, aes(x=timestamp, y=weight, color=scale)) +
  geom_line() +
  scale_x_datetime(date_labels = "%H", breaks = '1 hours') +
  ylim(c(12,16))

# plot subset
sub <- subset(scaledat, hour < 18 & hour > 15 & scale %in% 1:4)
sub <- subset(scaledat, scale %in% 1:4)

ggplot(sub, aes(x=timestamp, y=weight, color=scale)) +
  geom_line() +
  scale_x_datetime(date_labels = "%H", breaks = '1 hours') +
  ylim(c(12,16))


# Get saturated weights (after pot drainage)
sub <- subset(scaledat, timestamp < '2019-12-12 18:00')
ggplot(sub, aes(x=timestamp, y=weight, color=scale)) +
  geom_line() +
  scale_x_datetime(date_labels = "%H", breaks = '1 hours') +
  ylim(c(12,16))

# looks like they've drained for sure by 18:00
subset(scaledat, date(timestamp) == '2019-12-12' &
         hour(timestamp) == 18 & minute(timestamp) == 0 &
         scale == 15)



# get the fully-drained wet weight for each pot (at 6 pm on 12/12)
ww <- subset(scaledat, date(timestamp) == '2019-12-12' &
               hour(timestamp) == 18 & minute(timestamp) == 0)

# first, add all pot IDs regardless of date.
ww$plant_id <- sapply(as.character(ww$scale), function(x) 
  switch(x,
         '1'='W-26', '2'='W-28','3'='W-25','4'='W-27','5'='M-6','6'='M-7','7'='M-10','8'='M-11',
         '9'='D-6','10'='D-7','12'='D-10','14'='D-11','15'='W-6','16'='W-7'))
# check work
table(ww$plant_id, ww$scale)
ww <- ddply(ww, .(plant_id), function(x) setNames(mean(x$weight), 'pot_saturated_weight_kg'))
ww

# Save the saturated weights to a file
saveRDS(ww, '/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/scripts/clay_R_scripts/analysis/mass_balance_saturated_pot_weights.rds')

### ----- I don't remember exactly what I was trying to do here...
scales = sort(as.numeric(as.character(unique(scaledat$scale))))
df = data.frame(matrix(data = , nrow = length(scales), ncol = 4))
colnames(df) = c('start','end','start_wt','end_wt')
s=1
for(i in 1:length(scales)) {
  s = scales[i];   print(s)
  d = scaledat[scaledat$scale==s,]
  d = d[order(d$timestamp),]
  # head(d)
  ind = which(d$weight == max(d$weight))
  peaktime = d$timestamp[ind]
  start_ind = ind + 120*5
  end_ind = length(d$timestamp)
  df$start[i] = d$timestamp[start_ind] # plus 1 hour
  df$end[i] = d$timestamp[end_ind]
  df$start_wt[i] = mean(d$weight[start_ind:(start_ind + 20)])
  df$end_wt[i] = mean(d$weight[(end_ind - 20):end_ind])
}

df$start <- as.POSIXct(df$start, origin = '1970-01-01 00:00.00 GMT')
df$end <- as.POSIXct(df$end, origin = '1970-01-01 00:00.00 GMT')
df$wt_diff <- df$start_wt - df$end_wt
df
mean(df$wt_diff)




