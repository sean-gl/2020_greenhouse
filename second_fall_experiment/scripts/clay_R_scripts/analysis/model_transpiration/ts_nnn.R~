Sys.setenv(tz='GMT')
packages <- c('lubridate','nnfor','imputeTS')
lapply(packages, require, character.only = TRUE)

### Univariate Example
str(AirPassengers); head(AirPassengers)

# fit univariate NN
fit1 <- mlp(AirPassengers)
print(fit1)

# forecast using fit NN
frc <- forecast(fit1,h=12*5)
plot(frc)

# read 15-minute experimental data
dat  <- readRDS('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/analysis/clay/output_data/combined_data_predict_psi_leaf_transpiration.rds')
names(dat)

# get a subset for testing
dat2 <- subset(dat, block == 'W', select = c('by15','leaftemp_mean','mean_T_mL_hr','line_PAR_east_umol_m2_s'))

# impute missing values 
nas <- which(apply(dat2, 2, function(x) any(is.na(x))))
for(i in nas) dat2[,i] <- na_interpolation(dat2[,i])
summary(dat2)

# Subset just to training date range (endogenous variable)
dat3 <- subset(dat2, by15 >= '2019-12-03 17:00:00' & by15 <= '2019-12-06 16:45:00') 

# convert to time-series object (note: the indices are wrong)
# x <- ts(data = subset(dat3, select = -by15), frequency = 24*60/15, start = c(2019, 1))
x <- ts(data = subset(dat3, select = -by15), frequency = 24*60/15)

# univariate fit
fit1 <- mlp(x[,'mean_T_mL_hr'], reps=1)
# fit1 <- mlp(x, lags = 0, difforder = 0, reps = 1, hd=10)
print(fit1)

str(fit1)
seq(0, 288, 96)
y <- as.numeric(fit1$y)
y.hat <- as.numeric(fit1$fitted)
summary(y)
summary(y.hat)
plot(y[1:length(y.hat)], y.hat, type='p'); abline(c(0,1))
plot(y[96:190], y.hat, type='p'); abline(c(0,1))

sqrt()
x[,'mean_T_mL_hr']

frc <- forecast(fit1)
plot(frc)

## --- Multivariate fit using exogenous regressors ('xreg')

# Subset just to test date range (exogenous variable)
dat3 <- subset(dat2, by15 >= '2019-12-03 17:00:00' & by15 <= '2019-12-08 16:45:00') 
xreg <- as.matrix(dat3[,c('line_PAR_east_umol_m2_s','leaftemp_mean')])
length(x[,'mean_T_mL_hr']); nrow(xreg)
fit4 <- mlp(x[,'mean_T_mL_hr'], xreg=xreg, 
            lags = 1,
            xreg.lags=as.list(rep(0, ncol(xreg))), # add lag 2 regressor
            xreg.keep=as.list(rep(T), ncol(xreg)), # force it to stay in the model
            difforder=0, # Do not let mlp() to remove the stochastic trend
            reps=5) 

print(fit4)
frc <- forecast(fit4, xreg = xreg)
plot(frc)
# lines(frc$mean, col='red')
# nrow(xreg) - length(x[,'mean_T_mL_hr'])

# plot predictions vs. actual
y <- dat3$mean_T_mL_hr[(length(x[,'mean_T_mL_hr'])+1):nrow(dat3)]
y.hat <- as.numeric(frc$mean)
plot(y, type = 'l'); lines(y.hat, col = 'red')

summary(y.hat)
summary(y)
