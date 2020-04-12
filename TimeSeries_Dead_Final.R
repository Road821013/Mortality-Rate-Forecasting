library(data.table)
library(TSA)
library(timeSeries)
library(forecast)
library(locfit)
library(tseries)
D<-read.csv("C:/Users/User/Desktop/NCCU/TimeSeries/死亡.csv",header = T)
colnames(D)<-c("Time","CDR")
Dtrain<-ts(D$CDR,start=c(2000,1),end = c(2018,4),frequency = 12)
win.graph(width=2.5,height=2.5,pointsize=8)
plot(Dtrain,ylab='粗死亡率',type='o')
acf(Dtrain,lag.max = 60)

#Building linear model for CDR-time series####
model_l=lm(Dtrain~time(Dtrain))
win.graph(width=2.5,height=2.5,pointsize=8)
plot(Dtrain,type='o')
abline(model_l)
res_l=residuals(model_l)
Acf(rstudent(model_l),lag.max = 60)
Acf(res_l,lag.max = 60)

#Building seasonal mean model for CDR-time series( With intercept )####
res_l<-ts(res_l,start=c(2000,1),frequency = 12)
month.=season(res_l) # period added to improve table display
modelSeason=lm(res_l~month.) # January is dropped automatically

res_ls=residuals(modelSeason)
Acf(res_ls,lag.max = 60)
pacf(res_ls,lag.max = 60)

adf.test(res_ls, alternative = c("stationary"),k = 1)

modelSeason_fit = ts(fitted(modelSeason),start = c(2000,1),freq = 12)

ts.plot(modelSeason_fit, main = 'Seasonal Means Model',
        ylim = c(min(res_l),max(res_l)));points(res_l, col = 'black')

#Building SARIMA Model####
par(mfrow=c(1,2))
model1 <-arima(Dtrain,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=12))
res1<-residuals(model1)
Acf(res1,lag.max = 60,main="ACF of Residual of SARIMA(0,1,0)X(0,1,0)")
pacf(res1,lag.max = 60,main="PACF of Residual of SARIMA(0,1,0)X(0,1,0)")

de_trend = diff(diff(Dtrain),differences = 12)
par(mfrow=c(1,2))
acf(de_trend,lag.max=50,xlab="Lag",ylab="ACF",
    main="ACF after first&seasonal difference")
acf(de_trend,lag.max=50,xlab="Lag",ylab="PACF",
    type="partial",
    main="PACF after first&seasonal difference")

model2 <-arima(Dtrain,order=c(0,1,3),seasonal=list(order=c(0,1,0),period=12))
res2<-residuals(model2)
Acf(res2,lag.max = 60,main="ACF of Residual of SARIMA(0,1,3)X(0,1,0)")
pacf(res2,lag.max = 60,main="PACF of Residual of SARIMA(0,1,3)X(0,1,0)")

model3 <-arima(Dtrain,order=c(0,1,3),seasonal=list(order=c(0,1,1),period=12))
res3<-residuals(model3)
Acf(res3,lag.max = 120,main="ACF of Residual of SARIMA(0,1,3)X(0,1,1)")
pacf(res3,lag.max = 120,main="PACF of Residual of SARIMA(0,1,3)X(0,1,1)")

model4 <-arima(Dtrain,order=c(0,1,3),seasonal=list(order=c(5,1,1),period=12))
res4<-residuals(model4)
Acf(res4,lag.max = 120,main="ACF of Residual of SARIMA(0,1,3)X(5,1,1)")
pacf(res4,lag.max = 120,main="PACF of Residual of SARIMA(0,1,3)X(5,1,1)")

hist(res4,xlab='Standardized Residuals')
qqnorm(res4);
qqline(res4)
plot(res4,ylab='Residual of SARIMA(0,1,3)X(5,1,1) ',type='o')

#Forecast####
D_forecast = as.data.frame(forecast(Dtrain,model = model4,12))

