# Quality Checks of Scale data

# set timezone!
Sys.setenv(tz='GMT')

# read in raw saved data
scaledat <- readRDS('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/scale_output/scale_data_compiled_raw_long.rds')

# check for duplicate rows
d = duplicated(subset(scaledat, select = -file)); any(d)
nas = is.na(scaledat$scale); any(nas)

# check differences for smaller than 30 seconds
x = split(scaledat, scaledat$scale)
l = lapply(x, function(y) {
  w = y$timestamp[order(y$timestamp)]
  y$d = c(NA, as.numeric(diff(w)))
  ind = which(with(y, d < 26 & !is.na(d)))
  ind = sort(as.numeric(sapply(ind, function(x) {
    if(x > 1) {
      if(x < length(w)) {
        z = (x-1):(x+1)
      } else z = (x-1):x
    } else z = x
    return(z)
    })))
  return(y[ind,])
})

l



