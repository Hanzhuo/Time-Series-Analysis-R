library(fpp)
library(TSA)

data <- read.csv("[Your file location]")

head(data)
head(data$Count)

length(data$Count)

train<-data$Count[0:105]
test<-data$Count[105:length(data$Count)]

#Convert to time.series data type
week_count <- ts(train, start=c(2014,1),frequency=52)
week_count_test <- ts(test, start=c(2016,2),frequency=52)

start(week_count)
end(week_count)

#This will plot the time series
plot(week_count)
# This will fit in a line
abline(reg=lm(week_count~time(week_count)))

# Augmented Dickey-Fuller (ADF) test, null is non-stationary
adf.test(week_count, alternative = "stationary")

#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test, null is stationary
kpss.test(week_count)

#Conduct additive decomposition
fit <- stl(week_count, s.window="periodic")
plot(week_count, col="gray",
     main="Weekly Human Traffic",
     ylab="Human Traffic", xlab="")
lines(fit$time.series[,2],col="red",ylab="Trend")

#an additive decomposition of these data
plot(fit)

horizon <- length(test)

fc <- stlf(week_count, 
           h=horizon, 
           s.window="periodic", 
           method='ets',
           ic='bic', 
           opt.crit='mae')
length(fc$mean)
plot(forecast(fc))
lines(week_count_test)

res_fc<-residuals(fc)
tsdisplay(res_fc)
Box.test(res_fc,fitdf=5,lag=10,type="Ljung")

####
qqnorm(train); qqline(train)
qqnorm(log(train)); qqline(log(train))

par(mfrow=c(1,2))
Acf(week_count,lag.max = 52)
Pacf(week_count,lag.max = 52)

diff_train<-diff(week_count,52)
plot(diff_train)

#test differencing of train data
adf.test(diff_train, alternative = "stationary")
kpss.test(diff_train)
#so d=1 is fine


par(mfrow=c(1,2))
Acf(diff_train,lag.max = 52)
Pacf(diff_train,lag.max = 52)

eacf(diff_train)
#p=0,q=0, but we should try p=1,q=1; p=1,q=1

#Corrected Akaikes's Information Critetion(AIC) is index for deteming the order of an ARIMA model
#Good models are obtained by minizing AICc here
Arima(week_count,order = c(0,1,0), seasonal = c(1,0,0))
#AICc=1948.89
Arima(week_count,order = c(0,1,0), seasonal = c(1,1,0))
#wrong
Arima(week_count,order = c(0,1,0), seasonal = c(0,1,1))
#AICc=993.7 
Arima(week_count,order = c(0,1,0), seasonal = c(1,1,1))
#wrong
Arima(week_count,order = c(0,1,0), seasonal = c(2,1,1))
#wrong
Arima(week_count,order = c(0,1,0), seasonal = c(0,1,2))
#AICc=995.96

#####
Arima(week_count,order = c(0,1,1), seasonal = c(1,0,0))
#AICc=1921.5 
Arima(week_count,order = c(0,1,1), seasonal = c(1,1,0))
#wrong
Arima(week_count,order = c(0,1,1), seasonal = c(0,1,1))
#AICc=977.89
Arima(week_count,order = c(0,1,1), seasonal = c(1,1,1))
#wrong
Arima(week_count,order = c(0,1,1), seasonal = c(2,1,1))
#wrong
Arima(week_count,order = c(0,1,1), seasonal = c(0,1,2))
#AICc=980.24

####
Arima(week_count,order = c(1,1,0), seasonal = c(1,0,0))
#AICc=1936.2
Arima(week_count,order = c(1,1,0), seasonal = c(1,1,0))
#wrong
Arima(week_count,order = c(1,1,0), seasonal = c(0,1,1))
#AICc=993.9
Arima(week_count,order = c(1,1,0), seasonal = c(1,1,1))
#wrong
Arima(week_count,order = c(1,1,0), seasonal = c(2,1,1))
#wrong
Arima(week_count,order = c(1,1,0), seasonal = c(0,1,2))
#AICc=996.25

####
Arima(week_count,order = c(1,1,1), seasonal = c(1,0,0))
#wrong
Arima(week_count,order = c(1,1,1), seasonal = c(1,1,0))
#wrong
Arima(week_count,order = c(1,1,1), seasonal = c(0,1,1))
#AICc=977.2
Arima(week_count,order = c(1,1,1), seasonal = c(1,1,1))
#wrong
Arima(week_count,order = c(1,1,1), seasonal = c(2,1,1))
#wrong
Arima(week_count,order = c(1,1,1), seasonal = c(0,1,2))
#AICc=979.65 

arima_fit1 <- Arima(week_count,order = c(0,1,1), seasonal = c(0,1,1))
arima_fit2 <- Arima(week_count,order = c(1,1,1), seasonal = c(0,1,1))

Acf(residuals(arima_fit1))
Box.test(residuals(arima_fit1), lag=36, fitdf=8, type="Ljung")

Acf(residuals(arima_fit2))
Box.test(residuals(arima_fit2), lag=36, fitdf=8, type="Ljung")
###arima_fit2 is better

#par(mar = rep(2, 4))
#tsdiag(arima_fit, gof=15, omit.initial = F)

horizon <- length(test)
plot(forecast(arima_fit2, h=horizon))
lines(week_count_test)

#
holiday_train <- data$Holiday[0:105]
holiday_test <- data$Holiday[105:length(data$Count)]

arima_reg_fit <- Arima(week_count,
                       order = c(1,1,1), 
                       seasonal = c(0,1,1),
                       xreg=holiday_train)


residuals(arima_reg_fit)
Box.test(residuals(arima_reg_fit), lag=36, fitdf=8, type="Ljung")

horizon <- length(test)
arima_reg_fc <- forecast(arima_reg_fit,xreg=holiday_test,h=horizon)

plot(week_count)
plot(arima_reg_fc)
lines(week_count_test)


