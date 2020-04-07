# Clayton Bliss, last revision: 12/9/2015
# Import raw output data from lab scales (.csv file) and graph it

library(ggplot2)
x11()
theme_set(theme_classic(base_size = 20))  # change plot theme & font size
setwd("/home/pi/python_scripts")

plotWindow <- 10*60 # plot window in minutes
plotInterval <- 0.5 # plot refresh interval in minutes 

# function to read in data from csv and plot, repeated by 'interval' in minutes
readAndPlot <- function(plotWindow) {
  
  d <- read.csv("scale_output.csv", header = F, col.names = c('scale', 'date_time', 'kg'),
                colClasses = c('factor', 'character', 'character'))
  d$kg <- as.numeric(d$kg)
  d$date_time <- as.POSIXct(d$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver")
  
  # # plot all data
  # ggplot(d, aes(x = date_time, y = kg, color = scale)) +
  #   geom_line(size = 1)
  
  # plot subset of data by 'plotWindow' (in minutes)
  sub <- subset(d, as.numeric(d$date_time[length(d$date_time)]) - as.numeric(d$date_time) < plotWindow * 60) 
  p <- ggplot(sub, aes(x = date_time, y = kg, color = scale)) +
    geom_line(size = 1)
  return(p)
  
}

repeat {
  p <- readAndPlot(plotWindow)
  print(p)
  Sys.sleep(plotInterval*60)
  }
