# script to sumamrize leaf water potential data

require(dplyr); require(lubridate); require(ggplot2)

wp <- read.csv('/home/wmsru/Documents/Clay/greenhouse/2019 greenhouse data/experiment2/pressurebomb_greenhouse2019.csv')
head(wp)
wp$datetime <- as.POSIXct(wp$datetime, format="%m/%d/%Y %H:%M:%S", tz='GMT')
wp$date <- lubridate::date(wp$datetime)
wp$hour <- lubridate::hour(wp$datetime)

# check treatment assignments (done manually in excel)
which(substr(wp$plant, 1, 1) == 'd' & wp$current_treatment != 'wet')
which(substr(wp$plant, 1, 1) == 'w' & wp$current_treatment != 'dry')
which(substr(wp$plant, 1, 1) == 'm' & wp$current_treatment != 'moderate')

# these are suspect and have funny times; omit them for now
ind <- wp$date == '2019-11-15' & wp$plant == 'd20'
which(ind)
wp <- wp[!ind, ]

# create summary by date & hour
wpSummary <- wp %>% 
  group_by(date, hour, current_treatment) %>%
  summarise(datetime=min(datetime), mean=mean(psi_mPa, na.rm = T), sd=sd(psi_mPa, na.rm = T))

# add time of day column
wpSummary$tod[wpSummary$hour < 10] <- '< 10:00'
wpSummary$tod[wpSummary$hour >= 10 & wpSummary$hour < 12] <- '10:00-12:00'
wpSummary$tod[wpSummary$hour >= 12 & wpSummary$hour < 14] <- '12:00-14:00'
wpSummary$tod[wpSummary$hour >= 14] <- '> 14:00'
wpSummary$tod <- as.factor(wpSummary$tod)

ggplot(wpSummary, aes(x=datetime, y=mean, color=current_treatment)) +
  geom_point() + 
  geom_text(aes(label=hour),hjust=-1, vjust=0) +
  geom_errorbar(aes(ymin=mean - sd, ymax=mean + sd), width=.2, position=position_dodge(.9)) +
  scale_x_datetime(date_labels = "%d", breaks = '1 day')

# plot only date after treatment began
wpSummarySub <- subset(wpSummary, date > '2019-11-04')
# wpSummarySub <- wpSummary
ggplot(wpSummarySub, aes(x=datetime, y=mean, color=current_treatment)) +
  geom_point() + 
  geom_text(aes(label=hour),hjust=-1, vjust=0) +
  geom_errorbar(aes(ymin=mean - sd, ymax=mean + sd), width=.2, position=position_dodge(.9)) +
  scale_x_datetime(date_labels = "%d", breaks = '1 day')

# lattice-style plot
ggplot(wpSummarySub, aes(x=datetime, y=mean, color=current_treatment)) +
  geom_point() + 
  geom_text(aes(label=hour),hjust=-0.5, vjust=0) +
  geom_errorbar(aes(ymin=mean - sd, ymax=mean + sd), width=.2, position=position_dodge(.9)) +
  scale_x_datetime(date_labels = "%d", breaks = '1 day') +
  facet_wrap(~tod) 
  
