##############################################################################################
###### Econometr??a de series de tiempo ######################################################
###### Marcelo Villena, PhD Universidad Ib??ez ###############################################
###### Clase V.- Cointegration Analysis #####################################################
###### Invierno 2018 Pe?alol?n ###############################################################
##############################################################################################
# Set the working directory
setwd("/Users/simon/Desktop/Econometrics/Clase 5.- TSE")

##############################################################################################
rm(list=ls())

install.packages("xts", repos="http://cloud.r-project.org")
install.packages("data.table", dependencies=TRUE)
install.packages("TTR")
install.packages("tseries")

library(TTR)
library(tseries) # df, adf
install.packages("dynlm")
library(dynlm) # time series regression
install.packages("quantmod")
library(quantmod) # getSymbols

##############################################################################################
# Ejemplo 1 S&P

spy <- getSymbols("SPY", from="2000-01-01", to="2010-12-01", auto.assign = FALSE)$SPY.Adjusted
plot(spy,main="Precios SP500")
Rspy<-log(spy)-lag(log(spy))
plot(Rspy,main="Retornos SP500")

##############################################################################################
# Ejemplo 2 APPLE

aapl <- getSymbols("AAPL", from="1980-12-12", to="2014-08-24", auto.assign = FALSE)$AAPL.Adjusted
plot(aapl)
Raapl<-log(aapl)-lag(log(aapl))
plot(Raapl)

aapl <- as.numeric(aapl)
aapl <- aapl[!is.na(aapl)]

Raapl <- as.numeric(Raapl)
Raapl <- Raapl[!is.na(Raapl)]

##############################################################################################

adf.test(aapl)
adf.test(Raapl)

##############################################################################################

mRaapl=mean(Raapl)
x<-Raapl-mRaapl
x2<-x*x
plot.ts(x2)

##############################################################################################
require(lmtest)
model1<-arima(Raapl,order=c(0,0,0))
print(model1)
coeftest(model1)
res <- residuals(model1)
plot(res)
AAPL.var<-var(res)
res1 <- as.numeric(res)
res1 <- res1[!is.na(res1)]
acf(res1)
adf.test(res1)

##############################################################################################
# ARCH EFFECTS
reg1 <- dynlm(res1^2 ~ lag(res1,1)^2 + lag(res1,2)^2 + lag(res1,3)^2)
print(reg1)
coeftest(reg1)

##############################################################################################
# ARIMA PRO

Raaplfinal.aic <- Inf
Raaplfinal.order <- c(0,0,0)
for (p in 1:10) for (d in 0:0) for (q in 1:10) {
  Raaplcurrent.aic <- AIC(arima(Raapl, order=c(p, d, q)))
  if (Raaplcurrent.aic < Raaplfinal.aic) {
    Raaplfinal.aic <- Raaplcurrent.aic
    Raaplfinal.order <- c(p, d, q)
    Raaplfinal.arima <- arima(Raapl, order=Raaplfinal.order)
     }
}
Raaplfinal.order

##############################################################################################
# GARCH
require(tseries)
fit.garch <- garch(Raapl, trace=FALSE)
print(fit.garch)
confint(fit.garch)

##############################################################################################

# diapo36 GARCH vs Volatilidad promedio -----------------------------------
library(rugarch)
library(lmtest)
getSymbols("ECH", from="2008-01-01")
Returns = diff(log(Ad(ECH)))
adf.test(Ad(ECH))
adf.test(Returns[-1])
Returns[as.character(head(index(Ad(ECH)),1))] = 0
fit.garch <- garch(Returns, trace=FALSE)
print(fit.garch)
coeftest(fit.garch)
sigmaGarch<-fit.garch[["coef"]][["a0"]]/(1-fit.garch[["coef"]][["a1"]]-fit.garch[["coef"]][["b1"]])
sigmaAvg<-var(Returns)
sigmas<-data.frame(sAVG=sigmaAvg,sGARCH=sigmaGarch)
names(sigmas)<-c("sigmaAVG","sigmaGARCH")
sigmas


# ejemplo GARCH D39-40 ----------------------------------------------------

# Ejemplo Clase -----------------------------------------------------------

getSymbols('ECH', from='2008-01-01')
Returns = diff(log(Ad(ECH)))
adf.test(Ad(ECH))
adf.test(Returns[-1])
Returns[as.character(head(index(Ad(ECH)),1))] = 0
fit.garch = garch(Returns, trace=FALSE)
print(fit.garch)
coeftest(fit.garch)
sigmaGarch<- fit.garch[['coef']][['a0']]/(1-fit.garch[['coef']][['a1']]-
                                            fit.garch[['coef']][['b1']])
sigmaAvg=var(Returns)

windowLength = 40
foreLength = length(Returns) - windowLength
sigmaAvgV = vector(mode='character', length=foreLength)
sigmaGarchV <- vector(mode='character', length=foreLength)

for (d in 1:foreLength){
  ReturnsOffset = Returns[(d):(windowLength+d)]
  fit.garch <-garch(ReturnsOffset, trace=FALSE)
  sigmaGarch <-fit.garch[['coef']][['a0']]/(1-fit.garch[['coef']][['a1']]-
                                              fit.garch[['coef']][['b1']])
  sigmaAvg <- var(ReturnsOffset)
  print(sigmaGarch);print(sigmaAvg)
  sigmaGarchV[d]<-sigmaGarch
  sigmaAvgV[d]<-sigmaAvg
}

plot.ts(sigmaAvgV, col = 'red', main='Volatility Average versus GARCH(1,1)')
lines.default(sigmaGarchV, col = 'black')

