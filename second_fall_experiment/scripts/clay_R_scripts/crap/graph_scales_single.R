# Clayton Bliss, last revision: 12/9/2015
# Import raw output data from lab scales (.csv file) and graph it

library(ggplot2)
theme_set(theme_classic(base_size = 20))  # change plot theme & font size
setwd("/home/pi")

plotWindow <- 16*60 # plot window in minutes
plotInterval <- 5 # plot refresh interval in minutes 

# read in data from csv and plot, repeated by 'plotInterval' in minutes

d <- read.csv("scale_output.csv", header = F, col.names = c('scale', 'date_time', 'kg'),
                colClasses = c('factor', 'character', 'character'))
d$kg <- as.numeric(d$kg)
d$date_time <- as.POSIXct(d$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver")
d$pct_loss <- 1
  
# plot all data
x11()
View(d)
ggplot(d, aes(x = date_time, y = kg, color = scale)) +
  geom_line(size = 1)
  
# plot subset of data by 'plotWindow' (in minutes)
sub <- subset(d, as.numeric(d$date_time[length(d$date_time)]) - as.numeric(d$date_time) < plotWindow * 60) 
#p <- ggplot(sub, aes(x = date_time, y = kg, color = scale)) +
#  geom_line(size = 1)
#x11()
#print(p)
  
# plot single scale
for(s in unique(sub$scale)) {
	scale <- subset(sub, sub$scale == s)
	x11()	# open new plot window
	plot(scale$kg ~ scale$date_time)
	title(unique(scale$scale))
}

# keep windows open for X seconds before terminating R program
Sys.sleep(60*5)

