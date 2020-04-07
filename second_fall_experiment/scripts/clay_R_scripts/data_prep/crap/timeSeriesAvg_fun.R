
# load needed packages
require(plyr); require(lubridate)


# function to round up a number (or vector of numbers) to the "to" argument. 
roundUp <- function(x, to = NULL)
{
  if(is.null(to)) stop("'to' argument must be specified!")
  to*(x %/% to + as.logical(x %% to))
}

# main function to do averaging
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
  
  # recreate back timestamp column
  means$hour <- means$interval %/% 60
  means$minute <- means$interval %% 60
  means$timestamp <- as.character(paste0(means$date, ' ', means$hour, ':', means$minute))
  means$timestamp <- as.POSIXct(means$timestamp, format="%Y-%m-%d %H:%M")
  
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
