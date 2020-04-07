### Clay Bliss
###
### Script to process, plot and get 15-min means for Garret's watermark and decagon
### soil water potential sensors, from Sean's greenhouse 2019 experiment.
###

rm(list = ls())

### IMPORTANT: SET SYSTEM TIMEZONE TO GMT, THIS IS REQUIRED FOR CODE TO WORK.
Sys.setenv(TZ='GMT')
Sys.getenv('TZ') # make sure it got set

# read in soil water potential data

# "A" file = block D (west side)
swp_block_D <- read.csv('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/soil_water_potential/read_only/CR1000_GH_A_Matric_159.dat', 
                   skip = 1, header = T, stringsAsFactors = F) 
swp_block_D <- swp_block_D[-c(1:2),] # omit 1st row, is units
head(swp_block_D); tail(swp_block_D)

# "D" file = block M (middle)
swp_block_M <-  read.csv('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/soil_water_potential/read_only/CR1000_GH_D_Matric_158.dat', 
                         skip = 1, header = T, stringsAsFactors = F) 
swp_block_M <- swp_block_M[-c(1:2),] # omit 1st row, is units
head(swp_block_M); tail(swp_block_M)

# "B_C" file = block W (east side)
swp_block_W <-  read.csv('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/soil_water_potential/read_only/CR1000_GH_BC_Matric_901.dat', 
                         skip = 1, header = T, stringsAsFactors = F) 
swp_block_W <- swp_block_W[-c(1:2),] # omit 1st row, is units
head(swp_block_W); tail(swp_block_W)


# add block names
swp_block_D$block <- 'D'
swp_block_M$block <- 'M'
swp_block_W$block <- 'W'

# combine datasets
names(swp_block_D)
names(swp_block_M)
names(swp_block_W)

# combine Teros21 ('decagon') matric potential and other vars.
# note: soil temperature recorded by Teros21 sensor thermistor
d.teros21 <- swp_block_D[,c('TIMESTAMP','PTemp','Mp_1_Avg','Tp_1_Avg','block')]
m.teros21 <- swp_block_M[,c('TIMESTAMP','PTemp','Mp_1_Avg','Tp_1_Avg','Mp_2_Avg','Tp_2_Avg','block')]
w.teros21 <- swp_block_W[,c('TIMESTAMP','PTemp','Mp_1_Avg','Tp_1_Avg','Mp_2_Avg','Tp_2_Avg','block')]
d.teros21$Mp_2_Avg <- d.teros21$Tp_2_Avg <- NA
teros21 <- rbind(d.teros21, m.teros21, w.teros21)

# combine Watermark data (plus soil temp)
d.wm <- swp_block_D[,c('TIMESTAMP','WM_Avg','block')]
colnames(d.wm)[colnames(d.wm) %in% 'WM_Avg'] <- 'WM_Avg.1.'
m.wm <- swp_block_M[,c('TIMESTAMP','WM_Avg.1.','WM_Avg.2.','block')]
w.wm <- swp_block_W[,c('TIMESTAMP','WM_Avg.1.','WM_Avg.2.','block')]
d.wm$WM_Avg.2. <- NA
wm <- rbind(d.wm, m.wm, w.wm)

# combine all data (watermark + teros)
swp <- merge(teros21, wm, by = c('TIMESTAMP','block'), all = T)
anyDuplicated(swp)
head(swp)
names(swp)
names(swp) <- c('by15','block','campbell_panel_temp_C','teros_MP_1_kPa','teros_soil_temp_1_C',
                'teros_soil_temp_2_C','teros_MP_2_kPa','watermark_MP_1_kPa','watermark_MP_2_kPa')

# format columns
swp$by15 <- as.POSIXct(swp$by15, tz='GMT', format='%Y-%m-%d %H:%M:%S')
for(i in 3:ncol(swp)) swp[,i] <- as.numeric(swp[,i])

# add date column (for convenience)
swp$date <- lubridate::date(swp$by15)

# omit data after end of experiment
swp <- subset(swp, date <= "2019-12-12")

#----------------
# ====== Plotting and removal of bad/suspect data =============

require(ggplot2)

### Teros soil temp

# soil temp 2:
ggplot(swp) + geom_line(aes(x=by15, y=teros_soil_temp_2_C, color=block)) 

ggplot(swp[swp$block=='D',]) + 
  geom_point(aes(x=by15, y=teros_soil_temp_1_C)) +
  geom_point(aes(x=by15, y=teros_soil_temp_2_C, color = 'red')) 

# Data from block W, soil_temp_2 is all zeros, let's NA it
summary(swp$teros_soil_temp_2_C[swp$block == 'W'])
swp$teros_soil_temp_2_C[swp$block == 'W'] <- NA

# soil temp 1
ggplot(swp) + geom_point(aes(x=by15, y=teros_soil_temp_1_C, color=block)) 
ggplot(swp) + geom_line(aes(x=by15, y=teros_soil_temp_1_C, color=block))

# I think temp flatlines when we switched virgin plants, let's check
# one of W block reads zero after ~ Nov 1
ggplot(subset(swp, date > '2019-11-20' & date < '2019-11-30')) +
  geom_point(aes(x=by15, y=teros_soil_temp_1_C, color=block)) 

# yep, let's omit data after this time when I removed sensor from pot
# and swapped a virgin out
tail(swp$teros_soil_temp_1_C[swp$by15 <= '2019-11-27 14:22' & swp$block=='W'], 100)
head(swp$teros_soil_temp_1_C[swp$by15 >= '2019-11-27 14:22' & swp$block=='W'], 100)

# notebook says 14:22, let's do 14:00 just to be safe.
swp$teros_soil_temp_1_C[swp$by15 >= '2019-11-27 14:00' & swp$block=='W'] <- NA

# remove some data on last day, block D after 12:00
ggplot(subset(swp, date > '2019-12-11' & block == 'D')) +
  geom_point(aes(x=by15, y=teros_soil_temp_1_C, color=block)) 
tail(swp$teros_soil_temp_1_C[swp$by15 <= '2019-12-12 12:00' & swp$block=='D'], 100)
head(swp$teros_soil_temp_1_C[swp$by15 >= '2019-12-12 12:00' & swp$block=='D'], 100)

# omit data
swp$teros_soil_temp_1_C[swp$by15 >= '2019-12-12 12:00' & swp$block=='D'] <- NA

# omit outliers
x=subset(swp, block=='M')
plot(x$by15, x$teros_soil_temp_1_C)
identify(x$by15, x$teros_soil_temp_1_C)
x$by15[75]
x$teros_soil_temp_1_C[70:80]
swp$teros_soil_temp_1_C[swp$block=='M' & swp$by15=="2019-10-24 11:00:00 GMT"] <- NA

# same point bad for soil_temp_2
x=subset(swp, block=='M')
plot(x$by15, x$teros_soil_temp_2_C)
identify(x$by15, x$teros_soil_temp_2_C)
x$by15[75]
swp$teros_soil_temp_2_C[swp$block=='M' & swp$by15=="2019-10-24 11:00:00 GMT"] <- NA

### Compare soil temp 1, 2 (block M only)...looks similar.
ggplot(swp[swp$block=='M',]) +
  geom_line(aes(x=by15, y=teros_soil_temp_1_C, color='1')) +
  geom_line(aes(x=by15, y=teros_soil_temp_2_C, color='2'))
  
# ===== Soil matric potential

# NOTE: Garrett says watermark only good down to - 200 kPa
# and Teros21 only reliable above -1000 kPa

### Teros matric potential

# all data
ggplot(swp) +
  # geom_line(aes(x=by15, y=teros_MP_1_kPa, color=block)) +
  geom_line(aes(x=by15, y=teros_MP_2_kPa, color=block)) 

# M block only has data until ~ Nov 4
#
ggplot(swp) +
  geom_line(aes(x=by15, y=teros_MP_kPa, color=block)) +ylim(c(-50,0))
  # geom_line(aes(x=by15, y=teros_MP_2_kPa, color='2')) 

ggplot(swp) +
  # geom_line(aes(x=by15, y=teros_MP_kPa, color=block)) +
  geom_line(aes(x=by15, y=watermark_MP_kPa, color=block)) + ylim(c(-50,0))
### LOOKS LIKE DATA FROM TEROS_2 IS BLOCK M IS BAD...
ggplot(swp[swp$block=='M',]) +
  geom_line(aes(x=by15, y=teros_MP_2_kPa, color='2')) 

### OMIT THAT DATA
swp[swp$block=='M', 'teros_MP_2_kPa'] <- NA

ggplot(swp[swp$block=='D',]) +
  geom_line(aes(x=by15, y=teros_MP_1_kPa, color=block)) +
  geom_line(aes(x=by15, y=teros_MP_2_kPa, color=block)) 

ggplot(swp[swp$block=='W',]) +
  geom_line(aes(x=by15, y=teros_MP_1_kPa, color=block)) +
  geom_line(aes(x=by15, y=teros_MP_2_kPa, color=block)) 


### Watermark matric potential
### NOTE: values are much larger than Teros (hundreds of kPa not thousands)

# watermark 1
ggplot(swp) + geom_line(aes(x=by15, y=watermark_MP_1_kPa, color=block)) 



# NOTE: Data after 11/27, 14:30 are bad, as confirmed by "ReadMe" file
ggplot(swp[swp$date == '2019-11-27',]) + geom_line(aes(x=by15, y=watermark_MP_1_kPa, color=block)) 
View(swp[swp$date == '2019-11-27' & swp$block == 'W',])

# CHANGE THESE BAD VALUES TO NA
swp[swp$by15 >= '2019-11-27 14:30' & swp$block == 'W', 'watermark_MP_1_kPa'] <- NA


# watermark 2
ggplot(swp) + geom_line(aes(x=by15, y=watermark_MP_2_kPa, color=block)) 


# is data from block M, sensor 1 crap?
ggplot(swp[swp$block=='M',]) +
  geom_line(aes(x=by15, y=watermark_MP_2_kPa, color='2')) +
  geom_line(aes(x=by15, y=watermark_MP_1_kPa, color='1')) +
  ggtitle('block M')

### OMIT THIS DATA, IT IS CRAP, GARRET CONFIRMED
swp[swp$block=='M', 'watermark_MP_1_kPa'] <- NA

# is data from block W, sensor 2 crap? (I don't think this was even connected to a sensor...)
ggplot(swp[swp$block=='W',]) +
  geom_line(aes(x=by15, y=watermark_MP_2_kPa, color='2')) +
  geom_line(aes(x=by15, y=watermark_MP_1_kPa, color='1')) +
  ggtitle('block W')

### OMIT THIS DATA, IT IS CRAP, GARRET CONFIRMED
swp[swp$block=='W', 'watermark_MP_2_kPa'] <- NA


ggplot(swp[swp$block=='D',]) +
  geom_line(aes(x=by15, y=watermark_MP_2_kPa, color='2')) +
  geom_line(aes(x=by15, y=watermark_MP_1_kPa, color='1')) +
  ggtitle('block W')


# Data at end of experiment are bad, anything at 11:30 or later on 12/12, remove.
swp <- subset(swp, by15 < "2019-12-12 11:30")

### Take one last look at data...
summary(swp)
all(is.na(swp$teros_MP_2_kPa)) # this is all NA, let's delete this column
swp$teros_MP_2_kPa <- NULL



# Save the combined data
write.csv(swp, '/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/soil_water_potential/soil_water_potential_compiled.csv',
          row.names = F)


### Can we simplify this at all before analysis?

# Note: There is only 1 teros matric potential sensor with good data...rename this column (drop number)
colnames(swp)[colnames(swp) == 'teros_MP_1_kPa'] <- 'teros_MP_kPa'

# Watermarks: Can we condense to a single column of non-overlapping data?
ggplot(swp) + 
  # geom_line(aes(x=by15, y=watermark_MP_1_kPa, color=block)) +
  geom_line(aes(x=by15, y=watermark_MP_2_kPa, color=block)) 
  
# ...yes, since watermark 2 is only in "M" block
any(!is.na(swp$watermark_MP_1_kPa) & !is.na(swp$watermark_MP_2_kPa))
ind1 <- !is.na(swp$watermark_MP_1_kPa)
ind2 <- !is.na(swp$watermark_MP_2_kPa)
# create 'concensus' column
swp$watermark_MP_kPa <- ifelse(is.na(swp$watermark_MP_1_kPa), swp$watermark_MP_2_kPa, swp$watermark_MP_1_kPa)
# remove old columns
swp$watermark_MP_1_kPa <- swp$watermark_MP_2_kPa <- NULL

## note: soil temperature is generally only from sensor 1, except for a week or so in block M.
##Let's add a "mean soil temp column" that averages these special cases (but keep the sensor 1 column).
ggplot(swp) +
  # geom_line(aes(x=by15, y=teros_soil_temp_1_C, color=block)) +
  geom_line(aes(x=by15, y=teros_soil_temp_2_C, color=block))

# here is the "mean" column....probably better to just use data from sensor #1 for consistency...
swp$teros_soil_temp_mean_C <- rowMeans(swp[,c('teros_soil_temp_1_C','teros_soil_temp_2_C')], na.rm = T)


### Save compiled and condensed file as .RDS and .csv
saveRDS(swp, '/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/soil_water_potential/soil_water_potential_compiled_condensed.rds')
write.csv(swp, '/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/data/soil_water_potential/soil_water_potential_compiled_condensed.csv',
        row.names = F)
