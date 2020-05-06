

wind <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/wind_sensor_data/wind_15.csv')
s_temp <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/RH_temp_PAR_logger_data/soil_temp_15.csv')
pb <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/pressure_bomb/pressure_bomb_15.csv')
lq <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/line_PAR_sensors/line_PAR_15.csv')
rh <- read.csv('/home/sean/github/2020_greenhouse/second_fall_experiment/data/RH_temp_PAR_logger_data/rh_15.csv')


# combine by 15-min date and time
# first, merge data that include water treatments (e.g., not rh, air_temp, etc)
comb <- merge(wind, s_temp, by=c('by15', 'treatment'), all=TRUE); nrow(comb)
comb <- merge(comb, pb, by=c('by15', 'treatment'), all=TRUE); nrow(comb)
comb <- merge(comb, lq, by='by15', all=TRUE); nrow(comb)
comb <- merge(comb, rh, by='by15', all=TRUE); nrow(comb)

# save to file
# write.csv(comb, paste("/home/sean/sean_stuff/r_stuff/2020/greenhouse_2019/second_fall_experiment/",
#                      "data/rh_par_wind_psiLeaf_soilTemp_airTemp_15.csv", sep=""), row.names=FALSE)

df <- comb

# include only data when there are pressure bomb measurements 
df <- subset(df, complete.cases(comb$psi_MPa)); nrow(df)

# add machine learning and lasso libraries
library(caret)
library(randomForest)
library(glmnet)

# first, run simple linear mr with a few key variables...
m1 <- lm(df$psi_MPa ~ df$wind_speed_m_s + df$soil_temp_C + df$line_PAR_east_umol_m2_s + df$am2320_high_temp +
           df$am2320_high_rh + df$sht1_high_temp + df$sht1_high_rh + df$bmp_box_atm_p)
summary(m1)


###___________ lasso regression ______________####
###___________ lasso regression ______________####

# first subset to all possible independent variables and dependent variable
df2 <- subset(df, select=c("by15", "treatment", "position", "psi_MPa", 
                           "wind_speed_m_s", "soil_temp_C", "psi_MPa", 
                           "line_PAR_east_umol_m2_s", "line_PAR_west_umol_m2_s",
                           "par1_n", "par2_s", "pyr1_n", "pyr2_s", "am2320_high_temp", 
                           "am2320_high_rh", "sht1_high_temp", "sht1_high_rh", 
                           "sht2_low_temp", "sht2_low_rh", "bmp_box_temp", "bmp_box_atm_p")); nrow(df2)

# subset to... whatever...
df2 <- subset(df2, df$treatment == "well_watered")  ## CHANGE TREATMENT HERE...   

# there can't be any missing values
df2 <- subset(df2, complete.cases(df2)); nrow(df2)

# matrix "testvar" should include all possible predictor variables
testvar <- subset(df2, select=c("wind_speed_m_s", "soil_temp_C",  
                                "line_PAR_east_umol_m2_s", "line_PAR_west_umol_m2_s",
                                "par1_n", "par2_s", "pyr1_n", "pyr2_s", "am2320_high_temp", 
                                "am2320_high_rh", "sht1_high_temp", "sht1_high_rh", 
                                "sht2_low_temp", "sht2_low_rh", "bmp_box_temp", "bmp_box_atm_p")); nrow(testvar)
testvar <- as.matrix(scale(testvar)); nrow(testvar) # standardize matrix "testvar"

# give a dependent variable as a matrix
dep <- as.matrix(scale(df2$psi_MPa)); length(dep)
is.matrix(testvar); is.matrix(dep)

# "alpha" = lasso.  
fitY = glmnet(testvar, dep, family='gaussian', alpha=1, standardize=F) #already standardized  
plot(fitY, xvar="lambda") # plot coefficients against lambda value
plot(fitY, xvar="dev")    # plot coefficients against fraction of deviance explained
coef(fitY)  # list all coefficients with increasing values of lambda
# now... to choose an appropriate value of lambda run k-fold cross-validation
# note that this will give you slightly different distributions each time it is run
# the number of variables incldued in the model are across the top of the plot... and lambda across the bottom
# we want a model that minimizes MSE (y axis), but is also regulated to avoid over-fitting
# the dotted vertical line to the right is the value of lambda (and numer of fitted variables)
# that give you MSE that is within one SE of the mean... as a way to apply regularization
# as such the methods uses "k-fold" cross validation to select a lambda vallue that is within
# 1 SE of the minimum SE... which is somewhat arbitrary... but a different value could 
# be chosen to aid interpretation, i.e., for a biological reason...
# you can see that the 4-parameter model is nearly always within 1SE of the mean MSE
CV = cv.glmnet(testvar, dep, family='gaussian', alpha=1, standardize=F); plot(CV)

# refit "fitY" using lambda value that is 1 SE of model giving minimum MSE
fitY = glmnet(testvar, dep, family='gaussian', alpha=1, standardize=F, lambda=(CV$lambda.1se))  
coef(fitY)
summary(fitY)