### Function to calculate means in "n-minute window"

require(plyr)
dat <- subset(thermdat, file == 'THERM_dry_10-28.TXT')
# dat <- thermdat
head(dat)



# function to round up a number (or vector of numbers) to the "to" argument. 
roundUp <- function(x, to = NULL)
{
  if(is.null(to)) stop("'to' argument must be specified!")
  to*(x %/% to + as.logical(x %% to))
}

require(lubridate)
tsCol <- 'timestamp'
interval <- 15
measCols <- colnames(dat)[3:10]
dat$date <- date(dat$timestamp)
dat$minutes <- 60 * hour(dat[ , tsCol]) + minute(dat[ , tsCol])
dat$interval <- roundUp(dat$minutes, to = interval)
dat$interval_hour <- dat$interval / 60

# compute means 
means <- ddply(dat, .(date, interval, interval_hour), function(x, ...) {
  colMeans(x[ , measCols])
})

# compute sample sizes
sampleSizes <- ddply(dat, .(date, interval, interval_hour), function(x, ...) {
  apply(x[ , measCols], 2, length)
})

colMeans(summaryDat2[,5:ncol(summaryDat2)])

# fill in time gaps 
allIntervals <- seq(0, 1440, interval)
tsGrid <- expand.grid(date = unique(summaryDat$date), interval = allIntervals)

# merge grid of all possible dates/intervals to summary data
summaryMerge <- merge(summaryDat, tsGrid, by = c('date', 'interval'), all = TRUE)

which(!complete.cases(summaryMerge))

# examine time gaps in original data
dat2 <- dat
dat2$timediff <- c(0, diff(dat2$timestamp))
head(dat2$timediff, 100)
