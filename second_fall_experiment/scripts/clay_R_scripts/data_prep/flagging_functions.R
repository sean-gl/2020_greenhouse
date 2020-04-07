### Code for flaggins scale data

# rm(list = ls())
# 
# ### Long data format
# 
# # raw data
# dat <- readRDS("/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/scale_output/scale_data_raw_compiled_long.rds")
# dat$by15 <- NULL
# 
# 
# # subset of data to test function
# dsub <- subset(dat, scale %in% 1:2 & date == '2019-10-11')
# 
# df = dsub[ , c('timestamp', 'scale', 'weight')]
# rownames(df)=NULL

# Function to get max - min of a vector (used below)
maxFn <- function(x) {
  out <- max(x, na.rm = T) - min(x, na.rm = T)
  if(is.infinite(out)) out <- NaN
  return(out)
}

### FUNCTION ARGUMENTS
# df = df
# timeCol = 'timestamp'
# idCol = 'scale'
# measCol = 'weight' 
# threshVec = c(.03, .04, .05)
# interval = 15
# diffFn = maxFn

### function to aggregate raw data and apply flags
### note: data should be in LONG format (1 row per observation)
### arguments:
# df = dataframe
# timeCol = timestamp column name
# idCols = other unique columns to aggregate over
# measCol = measurement column name
# threshVec = vector of increasing thresholds (used for flagging data)
# interval = interval (minutes) to aggregate over
# diffFn = differencing function (used for flagging)
aggAndFlag <- function(df, timeCol, idCols, measCol, 
                       threshVec, interval, diffFn) {
  # add rounded timestamp column
  df$roundTime <- lubridate::ceiling_date(df[[timeCol]], unit = paste0(interval, ' minutes'))
  
  # check that threshVec is strictly increasing
  if(any(diff(threshVec) <= 0)) stop("threshVec must be strictly increasing!")
  
  # add new column name
  newColName <- paste0('max_diff_', measCol)
  
  # apply differencing function to each combination of interval & idCol
  out <- plyr::ddply(df, .variables = c('roundTime', idCols), function(x) {
    meas <- x[[measCol]]
    setNames(c(mean(meas, na.rm = T), diffFn(meas)), c('mean', newColName))
  })
  
  # create flags based on thresholds
  # Each threshold will have a numeric value from 1 to n, where n is the number of thresholds 
  out$flag <- 0 # Note: unflagged data will have 0 value
  ind <- seq_along(threshVec)
  for(i in ind) {
    if(i < length(threshVec)) {
      out[out[[newColName]] > threshVec[i] &
            out[[newColName]] <= threshVec[i+1] , 'flag'] <- i
    } else {
      out[out[[newColName]] > threshVec[i], 'flag'] <- i
    }
  }
  
  return(out)
} # end fn


# 
# # Call function 
# aggDat <- aggAndFlag(df = dat, timeCol = 'timestamp', idCol = 'scale', measCol = 'weight',
#                      threshVec = c(.03, .04, .05), interval = 60, diffFn = maxFn)
# 
# table(aggDat$flag)
# 
# # check flagging
# for(i in 0:3) print(range(aggDat$max_diff_weight[aggDat$flag==i]))
# 
# head(aggDat)
# 
# rawData <- dsub
# diffsDat <- maxDiffs

