require(readODS); require(ggplot2); require(plyr)
led <- read_ods("/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/LED_PAR.ods",
                sheet = 1, col_names = T)
head(led)

# plot of all measurements
ggplot(led, aes(x=height_m, y=PAR, color=bench_position)) + geom_point() + geom_line()

# mean PAR by canopy height
# TODO: Need to adjust canopy height to account for measurement position  
means <- ddply(led, .(height_m), function(x) mean(x$PAR, na.rm = T))
means
