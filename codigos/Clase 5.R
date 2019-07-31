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
plot(spy)
Rspy<-log(spy)-lag(log(spy))
plot(Rspy)

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
