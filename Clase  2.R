##############################################################################################
###### Econometr?a de series de tiempo #######################################################
###### Marcelo Villena, PhD Universidad Ib??ez ###############################################
###### Clase II.- Univariate Time Series Models ##############################################
###### Invierno 2018 Pe?alol?n ###############################################################
##############################################################################################
# Set the working directory
setwd("/Users/simon/Desktop/Econometrics/Clase 2.- TSE")

##############################################################################################
# Ejemplo 1 Sacando la tendencia de la serie temperatura global

rm(list=ls())
mydata<-read.csv ("gtemp.csv")
gtemp<-mydata$"gtem"
plot(gtemp, type="o", ylab="Global Temperature Deviations")
t<-1:142
summary(reg <- lm(gtemp ~ t))
plot(gtemp, type="o", ylab="Global Temperature Deviations")
abline(reg)
##############################################################################################
reg1= lm(gtemp~time(gtemp), na.action=NULL) # regress gtemp on time
par(mfrow=c(2,1))
plot(resid(reg1), type="o", main="detrended")
plot(diff(gtemp), type="o", main="first difference")
##############################################################################################
ts.plot(resid(reg1), main="detrended y first difference", xlim=c(0, 130), ylim=c(-0.5, 0.5))
lines(diff(gtemp), lwd=2, col=2)

# sobre la descomposición de una serie - media y error estandar -----------
mean(diff(gtemp)) #media
sd (diff (gtemp)) / sqrt (length(diff (gtemp))) #error estandar


##############################################################################################
par(mfrow=c(3,1)) # plot ACFs
acf(gtemp, 48, main="gtemp")
acf(resid(reg), 48, main="detrended")
acf(diff(gtemp), 48, main="first difference")
##############################################################################################
# Ejemplo Raiz Unitaria
# como ionstalar una libreria
install.packages('forecast', dependencies = TRUE)
install.packages("tseries", dependencies = TRUE)
install.packages("quadprog", dependencies = TRUE)
library(tseries)
library(quadprog)

adf.test(gtemp)
adf.test(resid(reg1))
adf.test(diff(gtemp))

summary(resid(reg1))
summary(diff(gtemp))
qqnorm(resid(reg1))
qqnorm(diff(gtemp))
shapiro.test(resid(reg1))
shapiro.test(diff(gtemp))

##############################################################################################
# simulacion ARIMA
par(mar=c(1,1,1,1))
par(mfrow=c(2,1))
plot(arima.sim(list(order=c(1,0,0), ar=.9), n=100), ylab="x",
     main=(expression(AR(1)~~~phi==+.9)))
plot(arima.sim(list(order=c(1,0,0), ar=-.9), n=100), ylab="x",
     main=(expression(AR(1)~~~phi==-.9)))

par(mfrow = c(2,1))
plot(arima.sim(list(order=c(0,0,1), ma=.5), n=100), ylab="x",
     main=(expression(MA(1)~~~theta==+.5)))
plot(arima.sim(list(order=c(0,0,1), ma=-.5), n=100), ylab="x",
     main=(expression(MA(1)~~~theta==-.5)))


# Ejemplo 2 - IPC Chile ---------------------------------------------------
rm(list=ls())
data<-read.csv ("ipc.csv")
ipc <- ts(data[,2],start = c(2013,1), end=c(2018, 6), frequency = 12)
plot.ts(ipc, xlab='Years', ylab = 'Indice de Precios al Comsumidor (IPC)')
##############################################################################################
#Descomposición
fit <- stl(ipc, s.window="period")
plot(fit)
#TestRaízUnitaria
library("tseries")
adf.test(ipc)
adf.test(diff(ipc))
#FunciónAutocorrelación(AFC)_AutocorrelaciónParcial(PAFC)
par(mfrow = c(1,2))
acf(diff(ipc),lag=36,lwd=3)
pacf(diff(ipc),lag=36,lwd=3)


# Descomposici??n exponencial simple  
fit <- HoltWinters(ipc, beta=FALSE, gamma=FALSE)
plot(fit)

##############################################################################################
# Encontrando el orden del modelo
adf.test(ipc)
adf.test(diff(ipc))

acf(diff(ipc),lag=36,lwd=3)
pacf(diff(ipc),lag=36,lwd=3)

##############################################################################################
# Calculo pronostico y error extramuestral

train_series=ipc[1:44]
test_series=ipc[45:62]

arimaModel_1=arima(train_series, order=c(1,1,1))
arimaModel_2=arima(train_series, order=c(1,1,0))
arimaModel_3=arima(train_series, order=c(0,1,1))
print(arimaModel_1);print(arimaModel_2);print(arimaModel_3)

forecast1=predict(arimaModel_1, length(test_series))
forecast2=predict(arimaModel_2, length(test_series))
forecast3=predict(arimaModel_3, length(test_series))
print(forecast1)

mse3 <- sum((forecast3$pred-test_series)^2)/length(test_series)
mad3 <- sum(abs(forecast3$pred-test_series))/length(test_series)
mape3 <- 100*sum(abs(1-test_series/forecast3$pred))/length(test_series)

forecast(arimaModel_3, length(test_series))
plot(forecast(fit, length(test_series)))
accuracy(fit)

##############################################################################################
# ARIMA autom??tico

fit <- auto.arima(train_series)
summary(fit)
plot(fit)

##############################################################################################
# Forecasting

f<-forecast(fit, length(test_series))
plot(forecast(f))
accuracy(fit)
mape <- 100*sum(abs(1-test_series/f[["mean"]]))/length(test_series)

##############################################################################################
# SARIMA
fit <- arima(ipc,order=c(1,1,1), seasonal=c(0,1,1))
print(fit)
res <- residuals(fit)
plot(res)
plot(forecast(fit, h=12))

##############################################################################################
# FORECAST

install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)

install.packages("forecast", dependencies=TRUE)
library(forecast)

seasonplot(ipc)
nsdiffs(ipc)
auto.arima(ipc)

##############################################################################################


