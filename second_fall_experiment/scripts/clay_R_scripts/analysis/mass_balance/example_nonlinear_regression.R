# logistic model fitting example code
# http://strata.uga.edu/8370/lecturenotes/nonlinearRegression.html

time <- c(1,2,3,5,10,15,20,25,30,35)
population <- c(NA,4.2,3.5,6.3,15.7,21.3,23.7,25.1,25.8,25.9)
plot(time, population, las=1, pch=16)

logisticModel <- nls(population ~ K / (1 + exp(Po + r * time)), start=list(Po=5, r=2, K=5))

logisticModel <- nls(population~K/(1+exp(Po+r*time)), start=list(Po=0, r=-0.211, K=30))


# to find r, use a values of p, t from middle of data set and solve for r:
t=15
p.mid=population[which(time==t)]; p.mid
Po = 0; K =30

r = (1/t) * (log((K-p.mid)/p.mid) - Po); r
logisticModel <- nls(population~K/(1+exp(Po+r*time)), start=list(Po=0, r=r, K=30))

summary(logisticModel)
coef(logisticModel)
confint(logisticModel)

x <- seq(min(time), max(time), length=100)
y <- predict(logisticModel, list(time=x))
points(x, y, type='l', col='blue')

# initial guesses
Po = 0; K = end.plant.wt

# to find r, use a values of p, t from middle of data set and solve for r:
x[46,]
p.mid = 0.1; t = 48 
r = (1/t) * (log((K-p.mid)/p.mid) - Po)
K; r

dat.backup <- dat
# dat <- dat[-1,]
mass <- dat$saturated_mass_kg
time <- dat$date
logmod <- nls(mass ~ K / (1 + exp(Po + r * time)), 
              start=list(Po=Po, r=r, K=K))


# self-starting logistic
ssl <- nls(mass ~ SSlogis(time, a, b, c))
ssl <- nls(mass ~ SSfpl(time, a, b, c, d))

summary(ssl)
xv <- seq(0, time[length(time)], 1)
yv <- predict(ssl, newdata = list(time = xv))
yv <- as.numeric(yv)
plot(time, mass)
lines(xv, yv)
