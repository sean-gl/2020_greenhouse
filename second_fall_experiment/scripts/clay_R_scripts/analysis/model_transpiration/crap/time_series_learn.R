Sys.setenv(tz='GMT')
packages <- c('lubridate','plyr','ggplot2','nnfor','imputeTS','zoo','xts','vars','astsa')
lapply(packages, require, character.only = TRUE)

str(AirPassengers)
head(AirPassengers)
tail(AirPassengers)

fit1 <- mlp(AirPassengers)
print(fit1)

frc <- forecast(fit1,h=12*5)
print(frc)

plot(AirPassengers)
plot(frc)

?taylor
plot(taylor)
str(taylor)
head(taylor)
x <- msts(taylor, seasonal.periods=c(48,336), start=2000+22/52)
plot(x)
y <- msts(USAccDeaths, seasonal.periods=12, start=1949)

dat2 <- subset(dat, block == 'W', select = c('by15','leaftemp_mean','mean_T_mL_hr'))
dat2 <- subset(dat2, by15 >= '2019-12-03 17:00:00' &
                   by15 <= '2019-12-06 17:00:00') # why is timezone wrong here?
dat2$mean_T_mL_hr <- na_interpolation(dat2$mean_T_mL_hr)

summary(dat2)

x <- read.zoo(dat2); str(x)
# x <- xts(dat2$leaftemp_mean, order.by=dat2$by15)
x <- as.ts(x)
summary(dat2)
head(dat2);tail(dat2)
# dat2$by15 <- as.numeric(dat2$by15)
plot(x)

x = ts(data = subset(dat2, select = -by15),
     frequency = 24*60/15,
     start = c(2019, 10))
head(x)
plot(x)

y <- as.ts(x); plot(y); str(y)
ti = time(y)
ti[2]-ti[1]
fit1 <- mlp(x, reps=3)
fit1 <- mlp(x, lags = 0, difforder = 0, reps = 1, hd=10)
print(fit1)

frc <- forecast(fit1)
plot(frc)

z <- 1:(length(AirPassengers)+24) # I add 24 extra observations for the forecasts
z <- cbind(z) # Convert it into a column-array
fit4 <- mlp(AirPassengers,xreg=z,xreg.lags=list(0),xreg.keep=list(TRUE),
            # Add a lag0 regressor and force it to stay in the model
            difforder=0) # Do not let mlp() to remove the stochastic tren
print(fit4)
frc <- forecast(fit4, xreg = z)
plot(frc)

require(tsutils)
fit1 <- mlp(AirPassengers)
loc <- residout(AirPassengers - fit1$fitted, outplot=FALSE)$location
zz <- cbind(z, 0)
zz[loc,2] <- 1
fit5 <- mlp(AirPassengers,xreg=zz, xreg.lags=list(c(0:6),0),xreg.keep=list(rep(FALSE,7),TRUE))
print(fit5)

#### ---------- Vector Autoregressive (VAR) models ------------

dat3 <- subset(dat, block == 'W' & by15 >= '2019-12-01 17:00:00' &
                  by15 <= '2019-12-11 05:00:00', 
               select = c('by15','mean_T_mL_hr','line_PAR_west_umol_m2_s','leaftemp_mean'))
dat3$mean_T_mL_hr <- na_interpolation(dat3$mean_T_mL_hr)
dat3$line_PAR_west_umol_m2_s <- na_interpolation(dat3$line_PAR_west_umol_m2_s)
dat3$leaftemp_mean <- na_interpolation(dat3$leaftemp_mean)
plot(dat3$mean_T_mL_hr, type='l')
summary(dat3)
dat3 <- read.zoo(dat3) 


m1 <- VARselect(dat3, lag.max = 20, type='both')
m1$selection
m1$criteria

m2 <- VAR(dat3, p=2, type='both')
summary(m2)

pred <- predict(m2, n.ahead = 500, ci = 0.95)

fanchart(pred)
