

# cannot have NAs in model matrix for lasso
# which variables have most NAs?
m = apply(dat, 2, function(x) length(x[is.na(x)]))
round(m[order(m, decreasing = T)] / nrow(dat), 2)

# Option1: remove variables with MANY missing values
df <- subset(dat, select = -c(mean_psi_MPa, leaftemp_bottom, leaftemp_middle, leaftemp_top))

# Option 2: remove variables with MANY missing values, PLUS 
# remove block, treatment (and any other experiment-specific variables.)
df <- subset(dat, select = -c(mean_psi_MPa, leaftemp_bottom, leaftemp_middle, leaftemp_top,
                              sd_T_mL_hr, n_T_mL_hr, block, treatment, predicted_psi_leaf,
                              hour))


## Now, remove by15 and date
df2 <- subset(df, select = -c(by15, date))



# create model matrix for predictor variables
# note, as written (with ".") this doesn't allow interaction terms

# Need to omit any NAs to use Lasso (or impute them...)
df <- df[complete.cases(df),]
df2 <- df2[complete.cases(df2),]

x <- model.matrix(mean_T_mL_hr ~ ., df2)[,-1]
y <- df2$mean_T_mL_hr

### REPEAT THE CROSS-VALIDATION N TIMES, TO SEE WHICH VARIABLES ARE CONSISTENTLY IMPORTNAT
# list to store variables
nreps <- 20
nzcList <- list()
require(glmnet)
for(i in 1:nreps) {
  
  # CV using full dataset
  lasso.cv <- cv.glmnet(x, y, family='gaussian', alpha=1, nfolds=10, standardize=T)
  # plot(lasso.cv)
  lasso.cv$lambda.1se
  
  # Now that we know lambda, fit on *full* data set
  full_fit_lasso <- glmnet(x, y, alpha = 1, lambda = lasso.cv$lambda.1se)
  
  # summary(full_fit_lasso)
  lasso_coeffs <- predict(full_fit_lasso,
                          type = "coefficients", # return betas; not predictions
                          s = lasso.cv$lambda.1se)
  nzCoef <- lasso_coeffs@Dimnames[[1]][which(lasso_coeffs != 0)]
  nzCoef <- nzCoef[nzCoef != '(Intercept)']
  nzcList[[i]] <- nzCoef
}
z=unlist(nzcList)
b=sort(unique(unlist(nzcList)))
k=sapply(b, function(a) length(z[z == a]))
impVars = names(k[k==nreps]) # these variables are always important
impVars

# CV using full dataset
lasso.cv <- cv.glmnet(x, y, family='gaussian', alpha=1, nfolds=10, standardize=T); plot(lasso.cv)
lasso.cv$lambda.1se

# Now that we know lambda, fit on *full* data set
full_fit_lasso <- glmnet(x, y, alpha = 1, lambda = lasso.cv$lambda.1se)

# summary(full_fit_lasso)
lasso_coeffs <- predict(full_fit_lasso,
                        type = "coefficients", # return betas; not predictions
                        s = lasso.cv$lambda.1se)
nzCoef <- lasso_coeffs@Dimnames[[1]][which(lasso_coeffs != 0)]
nzCoef <- nzCoef[nzCoef != '(Intercept)']
nzCoef

length(nzCoef)
setdiff(impVars, nzCoef)
setdiff(nzCoef, impVars)

# predictions on all data
lasso_pred_full <- predict(full_fit_lasso, s = lasso.cv$lambda.1se, newx = x)
sqrt(mean((lasso_pred_full - y)^2)) # RMSE
mean(abs(lasso_pred_full - y)) # MAE
plot(lasso_pred_full, y); abline(0, 1, col='red')

# Use all variables (full model)
fullmod_lasso_phys <- lm(y ~ x[,nzCoef])
summary(fullmod_lasso_phys)
sqrt(mean(fullmod_lasso_phys$residuals^2)); mean(abs(fullmod_lasso_phys$residuals))

# plot lasso predictions
df$pred_T <- lasso_pred_full
df$error_T <- df$mean_T_mL_hr - df$pred_T
sub <- subset(df, date <= '2019-11-04')

ggplot(sub, aes(x=by15, y=error_T, color=block)) + geom_line()

ggplot(sub) +
  geom_line(aes(x=by15, y=pred_T, color=block)) + 
  geom_point(aes(x=by15, y=mean_T_mL_hr, color=block))


### ----------- RANDOM FORESTS --------------------

### Split data into test/train sets
# set.seed(1)
propTrain <- 0.5

# split the data
# stratified sampling based on treatment
stratvar <- 'treatment'

### stratified sample
# train <- as.numeric(createDataPartition(df2[[stratvar]], p = propTrain, list = F))

# non-stratified sampling
train <- sample(1:nrow(df2), nrow(df2)*propTrain, replace = F)

# create split datasests
df2_train <- df2[train,]; nrow(df2_train)
df2_test <- df2[-train,]; nrow(df2_test)
# parallel versions for plotting predictions
df_train <- df[train,]
df_test <- df[-train,]

# fit a one-off random forest model (no tuning)
require(ranger)
rf_quick <- ranger(mean_T_mL_hr~ .,
                   data=df2,
                   num.trees=5000,
                   mtry=NULL,
                   importance = 'impurity')

# treeInfo(Rf1)
print(rf_quick)
importance(rf_quick)[order(importance(rf_quick), decreasing = T)]


### --- Tune the Random Forest Model --

# Set the tuning/training hyperparameters]
require(caret)
fitControl_randomForest <- trainControl(method='repeatedcv', 
                                        number=10, # either # folds (repeatedcv) or # of resampling iterations (not sure what method this is for?)
                                        repeats=1, # complete sets of folds to compute (repeatedcv method only)
                                        classProbs=F, # only for classification models
                                        verboseIter=T, # print training log
                                        search='grid', # grid or random
                                        savePredictions = 'final') # all, final or none

# (optional) create grid of tuning parameters for train()
sqrt(ncol(df2_train)-1)
tuneGrid = expand.grid(mtry = 1:6, 
                       splitrule = c('variance', 'extratrees', 'maxstat'),
                       min.node.size = seq(1, 10, 2))


# train the model
randomForest_pl <- train(mean_T_mL_hr ~ .,
                         data=df2_train,
                         metric='RMSE',
                         trControl=fitControl_randomForest,
                         # tuneLength=5, # use instead of tuneGrid (? I think)
                         method='ranger',
                         tuneGrid = tuneGrid)
print(randomForest_pl)
best <- randomForest_pl$bestTune
res <- randomForest_pl$results
res[res$RMSE == min(res$RMSE),]
res[res$Rsquared == max(res$Rsquared),]




# using refit forest "best"
train_pred <- predict(rf_quick, df2_train)$predictions 
test_pred <- predict(rf_quick, df2_test)$predictions
summary(df2_train$mean_T_mL_hr); summary(train_pred)
summary(df2_test$mean_T_mL_hr); summary(test_pred)

## calculate RMSE 
sqrt(mean((test_pred - df2_test$mean_T_mL_hr)^2)) # on test set
sqrt(mean((train_pred - df2_train$mean_T_mL_hr)^2)) # on train set

## calculate MAE (mean absolute error)
mean(abs(test_pred - df2_test$mean_T_mL_hr)) # testing
mean(abs(train_pred - df2_train$mean_T_mL_hr)) # training

## Plot y vs. yhat (training data)
plot(df2_train$mean_T_mL_hr, train_pred, xlab='actual', ylab='predicted'); abline(0,1, col='red') # train points in black


## Finally, predict on full dataset and plot some predictions
all_pred <- predict(rf_quick, df2)$predictions 

df$rf_pred_T <- all_pred

sub <- subset(df, date <='2019-11-04')
ggplot(sub) +
  geom_line(aes(x=by15, y=rf_pred_T, color=treatment)) +
  geom_point(aes(x=by15, y=mean_T_mL_hr, color=treatment))
