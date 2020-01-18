# TIME SERIES
rm(list=ls())
#########################################################################
# Exercise 1
dd <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/rainDay.txt")
dd$DATE <- as.Date(dd$DATE, format="%d.%m.%Y")
ts.dd <- ts(dd[,2], start=2000, freq=365)
str(ts.dd)
month_data <- months(dd$DATE)
weeks_data <- weekdays(dd$DATE)
quaters_data <- quarters(dd$DATE)
data <- as.data.frame(cbind(dd,weeks_data ,month_data, quaters_data))
#########################################################################
# Exercise 2
# A
hstart <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/hstart.dat")
hstart <- ts(hstart[,1], start=1966, frequency=12)
window(hstart,start=1970)
plot(hstart)
H.stl <- stl(hstart, s.window="periodic")
str(H.stl)
plot(H.stl)
H.stl$time.series[,"trend"]
H.stl$time.series[,"seasonal"]
H.stl$time.series[,"remainder"]
plot(H.stl$time.series[,"trend"])
plot(H.stl$time.series[,"seasonal"])
plot(H.stl$time.series[,"remainder"])
# B
dt <- co2
plot(dt)
res_decompose <- decompose(dt, type="additive")
str(res_decompose)
plot(res_decompose)
library(mgcv)
tr <- as.numeric(time(dt))
mm <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
            "Jul","Aug", "Sep", "Oct", "Nov","Dec")
factor_months <- factor(rep(mm,40),levels=mm)[1:length(dt)]
fit <- gam(dt ~ s(tr) + factor_months)
summary(fit)
plot(co2, ylab="co2",type="l")
lines(tr, fitted(fit), col="red")
plot(tr, co2 - fitted(fit), ylab="residuals", type="l")
fit <- gam(co2 ~ s(tr) + sin(2 *pi*tr)+cos(2*pi*tr))
plot(co2, ylab="co2",type="l")
lines(tr, fitted(fit), col="red")
plot(tr, co2 - fitted(fit), ylab="residuals", type="l")
# C
d.creatine <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/kreatin.dat")
t.creatine <- ts(d.creatine[,2], start = 1)
plot(t.creatine)
acf(t.creatine, plot = TRUE)
acf(t.creatine, type = "partial", plot = TRUE)
pacf(t.creatine)
#########################################################################
# Exercise 3
# A
d.force <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/kraft.dat",  header = FALSE)
ts.force <- ts(d.force[, 1])
ts.forceA <- window(ts.force, start = 1, end=280)
plot(ts.forceA)
acf(ts.forceA, plot = TRUE)
acf(ts.forceA, type = "partial", plot = TRUE)
ar.force <- ar(ts.forceA, method = "burg")
ar.force$aic
ar.force <- arima(ts.forceA, order = c(9,0,0), method = "ML")
residuals <- resid(ar.force)
plot(residuals)
force.pred <- predict(ar.force, n.ahead = 40)
true_force.pred <- window(ts.force, start = 281)
plot(force.pred$pred)
points(true_force.pred)
lines(true_force.pred,col=2)
# Corrected
par(mfrow = c(2, 2), mar = c(3, 3, 2, 0.1))
plot(ar.force$residuals, ylab = "residuals")
acf(ar.force$residuals, type = "partial", plot = TRUE, main = "")
plot(ts.forceA - ar.force$residuals, ar.force$residuals, xlab = "fitted values",
       ylab = "residuals", main = "Tukey-Anscombe plot")
qqnorm(ar.force$residuals)
qqline(ar.force$residuals)
force.pred <- predict(ar.force, n.ahead = 40)
par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))
plot(true_force.pred)
lines(force.pred$pred, lty = 2)
lines(force.pred$pred + 1.96*force.pred$se, lty = 3)
lines(force.pred$pred - 1.96*force.pred$se, lty = 3)
# B
d.yields <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/yields.dat", header = FALSE)
t.yields <- ts(d.yields[, 1])
mean_ts <- mean(t.yields)
plot(t.yields)
abline(h=mean_ts)
acf(t.yields, plot = TRUE, main = "ACF")
acf(t.yields, type = "partial", plot = TRUE, main = "Partial acf")
lag.plot(t.yields, lag = 6, layout = c(2, 3), do.lines = FALSE)
var(t.yields) * (length(t.yields) - 1) / length(t.yields)
acf(t.yields, type = "covariance", plot = F)$acf[1]
# C
yields <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/yields.dat",
                     header = FALSE)
t.yields <- ts(yields[, 1])
acf(t.yields, plot = TRUE, main = "ACF")
acf(t.yields, type = "partial", plot = TRUE, main = "Partial acf")
r.yw <- ar(t.yields, method="yw", order.max=1)
str(r.yw)
r.yw <- ar(t.yields, method="yw", order.max=1)
r.burg <- ar(t.yields, method="burg", order.max=1)
str(r.burg)
r.mle <- ar(t.yields, method="mle", order.max=2)
str(r.mle)
#########################################################################
# Exercise 4
# A
plot(0:30, ARMAacf(ar=c(0.9,-0.5), lag.max=30), type="h", ylab="ACF")
plot(1:30, ARMAacf(ar=c(0.9,-0.5), lag.max=30, pacf=T), type="h", ylab="PACF")
plot(0:30, ARMAacf(ma=c(0.8,-0.5,-0.4), lag.max=30), type="h", ylab="ACF")
plot(1:30, ARMAacf(ma=c(0.8,-0.5,-0.4), lag.max=30, pacf=T), type="h", ylab="PACF")
plot(0:30, ARMAacf(ma=c(0.8,-0.5,-0.4), lag.max=30), type="h", ylab="ACF")
plot(1:30, ARMAacf(ma=c(0.75,-1.0,0.25), lag.max=30, pacf=T), type="h", ylab="PACF")
plot(0:30, ARMAacf(ar=c(-0.75), ma=c(-1,-0.25), lag.max=30), type="h", ylab="ACF")
plot(1:30, ARMAacf(ar=c(-0.75), ma=c(-1,-0.25), lag.max=30, pacf=T), type="h", ylab="PACF")

r.sim1 <- arima.sim(n=2000, model=list(ar=c(0.9,-0.5)))
plot(r.sim1)
acf(r.sim1, plot = TRUE, main = "ACF")
acf(r.sim1, type = "partial", plot = TRUE, main = "Partial acf")
r.sim2 <- arima.sim(n=2000, model=list(ma=c(0.8,-0.5,-0.4)))
plot(r.sim2)
acf(r.sim2, plot = TRUE, main = "ACF")
acf(r.sim2, type = "partial", plot = TRUE, main = "Partial acf")
r.sim3 <- arima.sim(n=2000, model=list(ar=c(-0.75), ma=c(-1,-0.25)))
plot(r.sim3)
acf(r.sim3, plot = TRUE, main = "ACF")
acf(r.sim3, type = "partial", plot = TRUE, main = "Partial acf")
# B
dat <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/mcARMA.dat",
                  header = TRUE)
ts <- ts(dat$x)
plot(ts, main ="Time series")
library(forecast)
tsdisplay(ts)
ar.ts <- arima(ts, order = c(2,0,1))
ar.ts
confint(ar.ts )
str(ar.ts)
plot(ts - ar.ts$residuals,ar.ts$residuals)
acf(ar.ts$residuals, plot = TRUE, main = "ACF")
acf(ar.ts$residuals, type = "partial", plot = TRUE, main = "Partial acf")
confint(ar.ts)
# C
d.beluga <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/beluga.dat", header=T)
d.beluga <- ts(d.beluga)
summary(d.beluga)
fit <- lm(NURSING ~., data = d.beluga)
plot(fit,which=1)
plot(fit,which=2)
tsdisplay(fit$residuals)
fit.res <- arima(fit$residuals, order = c(2,0,1))
fit.res$residuals
tsdisplay(fit.res$residuals)
# Correct
r.burg <- ar(fit$residuals, method="burg", order.max=2, aic=F)
str(r.burg)
r.aic <- ar(fit$residuals, method="burg")$aic
plot(0:(length(r.aic)-1), r.aic, xlab="Order", type="b")
library(nlme)
#########################################################################
# Exercise 5
# A
library(fpp)
plot(sunspotarea)
hist(sunspotarea)
ts.series <- window(log(sunspotarea), start = 1875, end = 1974)
tsdisplay(ts.series)
ar.series <- ar(ts.series, method="burg", order.max=3, aic=F)
tsdisplay(ar.series$resid)
ar.series <- ar(ts.series, method="burg", order.max=20, aic=T)
tsdisplay(ar.series$resid)
ts.forecast <- window(log(sunspotarea), start = 1875, end = 1974)
arima.series <- arima(ts.series, order = c(10,0,0))
pred <- predict(arima.series,n.ahead=37)
conf_pred_high <- pred$pred + 1.96 *  pred$se
conf_pred_low <- pred$pred - 1.96 *  pred$se
plot(log(sunspotarea))
abline(h = arima.series$coef["intercept"], lty = 2)
lines(pred$pred, col = "green")
lines(conf_pred_high, col = "red")
lines(conf_pred_low, col = "blue")
pred.error <- window(log(sunspotarea), start = 1975, end = 2011) - pred$pred
(pred.mse <- mean(pred.error^2) )
# B
t.url <- "http://stat.ethz.ch/Teaching/Datasets/WBL/varve.dat"
d.varve <- ts(scan(t.url)[201:550])
hist(d.varve)
tsdisplay(d.varve)
tsdisplay(diff(log(d.varve)))
d.series <- log(d.varve)
mean(diff(log(d.varve)))
arima.series <- arima(d.series, order = c(1,1,1))
tsdisplay(arima.series$residuals)
predictions <- predict(arima.series,n.ahead=30)
conf_predictions_high <- predictions$pred + 1.96 *  predictions$se
conf_predictions_low <- predictions$pred - 1.96 *  predictions$se
plot(d.series)
lines(predictions$pred,col=2, ylim = c(1.5, 4.5))
lines(conf_predictions_high, col = 4)
lines(conf_predictions_low, col = 4)
y = diff(log(d.varve))
arima.series <- arima(y, order = c(1,1,1))
tsdisplay(arima.series$residuals)
predictions <- predict(arima.series,n.ahead=30)
conf_predictions_high <- predictions$pred + 1.96 *  predictions$se
conf_predictions_low <- predictions$pred - 1.96 *  predictions$se
plot(y)
lines(predictions$pred,col=2, ylim = c(1.5, 4.5))
lines(conf_predictions_high, col = 4)
lines(conf_predictions_low, col = 4)
time.varve <- time(log(d.varve))
fit.loess <- loess(log(d.varve) ~ time.varve)
ts.resid <- log(d.varve) - fit.loess$fitted
tsdisplay(ts.resid)
arima.series <- arima(ts.resid, order = c(1,0,0))
tsdisplay(arima.series$residuals)
rem.pred<- predict(arima.series,n.ahead=30)
plot(arima.series$residuals)
lines(predictions$pred, col=2)
starting.point <- tail(predict(fit.loess),n=1)
yy <- tail(fitted(fit.loess),n=30)##fit the last 30 values of the trend
xx <- tail(time(log(d.varve)),n=30)
fit.regr <- lm(yy~xx)
trend.forecast <- predict(fit.regr,newdata=data.frame(xx=(max(time(log(d.varve)))+1:30)))
#Or alternatively
trend.forecast <- starting.point + (1:30) * coef(fit.regr)[2]
plot(log(d.varve), xlim=c(0,400))
lines(as.numeric(time(log(d.varve))), fitted(fit.loess), col = "red")
lines((max(time(log(d.varve)))+1:30), trend.forecast + rem.pred$pred,  col="blue")
expsmooth.fit <- HoltWinters(log(d.varve), gamma=FALSE)
t.pr <- predict(expsmooth.fit, 30, prediction.interval=T)
plot(expsmooth.fit, t.pr)
# C
set.seed(5)
ts.sim <- arima.sim(model = list(ar = c(0.3)), n = 100)
tsdisplay(ts.sim)
model1 <- arima(ts.sim,order=c(1,0,0))
model2 <- arima(ts.sim,order=c(2,0,0))
model3 <- arima(ts.sim,order=c(5,0,0))
model4 <- arima(ts.sim,order=c(10,0,0))
tsdisplay(model1$residuals)
tsdisplay(model2$residuals)
tsdisplay(model3$residuals)
tsdisplay(model4$residuals)
model1$sigma2
model2$sigma2
model3$sigma2
model4$sigma2
pred1 <- predict(model1,n.ahead=1)
pred2 <- predict(model2,n.ahead=1)
pred3 <- predict(model3,n.ahead=1)
pred4 <- predict(model4,n.ahead=1)
conf_pred <- rbind(pred1$pred[1] + c(1,-1)*1.96*pred1$se[1],pred2$pred[1] + c(1,-1)*1.96*pred2$se[1],
                   pred3$pred[1] + c(1,-1)*1.96*pred3$se[1],pred3$pred[1])
                   
summary_pred <- rbind(pred1[1], pred2[1], pred3[1], pred4[1])
summary_pred <- cbind(summary_pred, rbind(model1$sigma2,model2$sigma2,model3$sigma2,model4$sigma2))
# Simulation
set.seed(1)
# Initialization
coverage.1 <- numeric(length = 100)
coverage.5 <- numeric(length = 100)
coverage.10 <- numeric(length = 100)
ms.pred.error.1 <- numeric(100)
ms.pred.error.5 <- numeric(100)
ms.pred.error.10 <- numeric(100)
for(i in 1:100){
  # simulate AR(1) of length 101
  sim <- arima.sim(model = list(ar = c(0.3)), n = 101)
  # fit AR(1), AR(5) and AR(10)
  fit1 <- arima(window(sim, start = 1, end = 100), order = c(1,0,0))
  fit5 <- arima(window(sim, start = 1, end = 100), order = c(5,0,0))
  fit10 <- arima(window(sim, start = 1, end = 100), order = c(10,0,0))
  # 1-step prediction:
  pred1 <- predict(fit1, n.ahead=1)
  pred5 <- predict(fit5,n.ahead=1)
  pred10 <- predict(fit10, n.ahead=1)
  # 1-step prediction interval:
  PI1 <- pred1$pred[1] + c(-1,1)* 1.96 * pred1$se[1]
  PI5 <- pred5$pred[1] + c(-1,1)* 1.96 * pred5$se[1]
  PI10 <- pred10$pred[1] + c(-1,1)* 1.96 * pred10$se[1]
  # MSE
  ms.pred.error.1[i] <- (sim[101] - pred1$pred[1])^2
  ms.pred.error.5[i] <- (sim[101] - pred5$pred[1])^2
  ms.pred.error.10[i] <- (sim[101] - pred10$pred[1])^2
  # Is true 101th observation contained in prediction interval?
  coverage.1[i] <- ifelse(sim[101] >= PI1[1] & sim[101] <= PI1[2], 1, 0)
  coverage.5[i] <- ifelse(sim[101] >= PI5[1] & sim[101] <= PI5[2], 1, 0)
  coverage.10[i] <- ifelse(sim[101] >= PI10[1] & sim[101] <= PI10[2], 1, 0)
}
# How often in 100 times is true value contained in prediction interval?
sum(coverage.1)
sum(coverage.5)
sum(coverage.10)
# Mean squared error averaged over 100 simulations
mean(ms.pred.error.1)
mean(ms.pred.error.5)
mean(ms.pred.error.10)
#########################################################################
# Exercise 6
# A
d.advert <- read.table("ftp://stat.ethz.ch/Teaching/Datasets/WBL/advert.dat",  header = TRUE)
ts.advert <- ts(log(d.advert[, "ADVERT"]), start = 1907)
ts.sales <- ts(log(d.advert[, "SALES"]), start = 1907)
plot(ts.union(ts.sales, ts.advert), plot.type = "single")
ts.plot(ts.sales, ts.advert)
par(mfrow = c(1, 1))
ts.sal.d1 <- diff(ts.sales[!is.na(ts.sales)])
ts.adv.d1 <- diff(ts.advert[!is.na(ts.advert)])
tsdisplay(ts.sal.d1)
tsdisplay(ts.adv.d1)
r.fit.adv <- arima(ts.adv.d1, order = c(2,0,0))
ts.D <- resid(r.fit.adv)
alpha <- r.fit.adv$coef[1:2]
ts.Z <- filter(ts.sal.d1, c(1, -alpha), sides = 1)
ts.trans <- ts.intersect(ts.Z, ts.D)
acf(ts.trans, na.action = na.pass)
acf(..., type = "covariance", na.action = na.pass)$acf[, 1, 2]
gamma21 <- acf(ts.trans, plot = FALSE, type = "covariance",  na.action = na.pass)$acf[, 1, 2]
round(gamma21/r.fit.adv$sigma2, 2)[1:6]
# B
ts.ocwave <- ts(scan("http://stat.ethz.ch/Teaching/Datasets/WBL/ocwave.dat"),
                start = 1, frequency = 4)
plot(ts.ocwave)
spec.pgram(ts.ocwave, taper = 0.1, detrend = FALSE, demean = TRUE, log = "dB")
spec.ar(ocwave.burg, log = "dB")
spec.ar(ocwave.yw, log = "dB", add = TRUE, lty = 2)
legend("bottomleft", legend = c("Burg", "Yule-Walker"), lty = 1:2, bty = "n")
ocwave.yw <- ar.yw(ts.ocwave, aic = FALSE, order = 6)
ocwave.burg <- ar.burg(ts.ocwave, aic = FALSE, order = 6)
