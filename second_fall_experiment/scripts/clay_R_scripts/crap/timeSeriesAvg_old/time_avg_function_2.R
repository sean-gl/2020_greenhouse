### Function to calculate means in "n-minute window"

require(plyr); require(lubridate)

dat <- subset(thermdat, file == 'THERM_dry_10-28.TXT')
# dat <- thermdat
head(dat)



# function to round up a number (or vector of numbers) to the "to" argument. 
roundUp <- function(x, to = NULL)
{
  if(is.null(to)) stop("'to' argument must be specified!")
  to*(x %/% to + as.logical(x %% to))
}

# tsCol <- 'timestamp'
# interval <- 15
# measCols <- colnames(dat)[3:10]


SummarizeTimeseries <- function(dat, tsCol, measCols, interval, fillGaps = TRUE) {
  
  dat$date <- date(dat$timestamp)
  # dat$hour <- hour(dat$timestamp)
  # dat$minute <- minute(dat$timestamp)
  dat$total_minutes <- 60 * hour(dat[ , tsCol]) + minute(dat[ , tsCol])
  dat$interval <- roundUp(dat$total_minutes, to = interval)
  # dat$interval <- as.factor(roundUp(dat$total_minutes, to = interval))
  # dat$interval_hour <- dat$interval / 60
  
  # compute means 
  # NOTE: THIS OMITS ANY NA VALUES
  means <- ddply(dat, .(date, interval), function(x, ...) {
    colMeans(x[ , measCols], na.rm = TRUE)
  })
  
  # compute sample sizes
  sampleSizes <- ddply(dat, .(date, interval), function(x, ...) {
    apply(x[ , measCols], 2, function(y) length(y[!is.na(y)]))
  })
  
  # fill in time gaps 
  allIntervals <- seq(0, 1440, interval)
  tsGrid <- expand.grid(date = unique(means$date), interval = allIntervals)
  
  # merge grid of all possible dates/intervals to summary data
  if(fillGaps) {
    means <- merge(means, tsGrid, by = c('date', 'interval'), all = TRUE)
    sampleSizes <- merge(sampleSizes, tsGrid, by = c('date', 'interval'), all = TRUE)
    sampleSizes[is.na(sampleSizes)] <- 0
  }
  
  return(list(means = means, sampleSizes = sampleSizes))
}


summaryDat <- SummarizeTimeseries(dat, tsCol = 'timestamp', measCols = colnames(dat)[3:10], interval = 15)

means <- summaryDat$means

### Reconstruct the timeseries
means$hour <- means$interval %/% 60
means$minute <- means$interval %% 60
means$timestamp <- paste0(means$date, ' ', means$hour, ':', means$minute)
means$timestamp <- lubridate::ymd_hm(means$timestamp, tz = 'MST')

meansLong <- gather(means, key = id, value = temperature, '1_lower':'4_upper')
ggplot(meansLong, aes(x = timestamp, y = temperature, group = id, color = id)) +
  geom_line()

which(!complete.cases(summaryMerge))

# examine time gaps in original data
dat2 <- dat
dat2$timediff <- c(0, diff(dat2$timestamp))
head(dat2$timediff, 100)
