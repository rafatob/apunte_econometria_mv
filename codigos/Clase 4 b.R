##############################################################################################
###### Econometr??a de series de tiempo ######################################################
###### Marcelo Villena, PhD Universidad Ib??ez ###############################################
###### Clase IV.- Cointegration Analysis #####################################################
###### Invierno 2018 Pe?alol?n ###############################################################
##############################################################################################
# Set the working directory
# setwd("/Users/simon/Desktop/Econometrics/Clase 4.- TSE")
setwd("~/projects/apunte_econometria_mv/data")

##############################################################################################

rm(list=ls())

#install.packages("tseries")
library(tseries) # df, adf
#install.packages("dynlm")
library(dynlm) # time series regression
#install.packages("quantmod")
library(quantmod)
#install.packages("urca")
library(urca)
#install.packages("devtools")
library(devtools)
#install_github("fcbarbi/ardl")
#install.packages("tsDyn")
library(tsDyn)
#install.packages("reshape2")
library("reshape2")
#install.packages("ggplot2")
library("ggplot2")

##############################################################################################

mydata<-read.csv ("cointegration.csv")
ipsa<-ts(mydata$IPSA,frequency=12, start = c(2010,1))
cu<-ts(mydata$CU,frequency=12, start = c(2010,1))
sp<-ts(mydata$s.p,frequency=12, start = c(2010,1))
summary(mydata)

getSymbols("SPY", from="2010-01-01", to="2018-01-01")
spy <- SPY$SPY.Adjusted

df <- data.frame(ipsa, sp, cu)

##############################################################################################

acf(ipsa)
adf.test(ipsa)
adf.test(diff(ipsa))

acf(sp)
adf.test(sp)
adf.test(diff(sp))

acf(cu)
adf.test(cu)
adf.test(diff(cu))

##############################################################################################
# cointegratio

ipsa_reg1 <- dynlm(ipsa ~ cu + sp)
summary(ipsa_reg1)
ipsa_reg2 <- dynlm(ipsa ~ L(cu, 1:3))
summary(ipsa_reg2)
ipsa_reg3 <- dynlm(ipsa ~ cu)
summary(ipsa_reg3)
residuos <- ipsa_reg3[["residuals"]]
plot(residuos)
adf.test(residuos)
qqnorm(residuos)
qqline(residuos)

##############################################################################################
# Error correction model
ipsa_reg4 <- dynlm(diff(ipsa) ~ diff(cu) + lag(residuos))
summary(ipsa_reg4)

##############################################################################################
# Johansen-Procedure 
library(urca)

a<-data.frame(ipsa,cu)
cointegration <- ca.jo(a, type="trace",ecdet="trend",spec="transitory")
summary(cointegration)

library(tsDyn)
#Fit a VECM with Engle-Granger 2OLS estimator:
vecm.eg<-VECM(a, lag=2)

#Fit a VECM with Johansen MLE estimator:
vecm.jo<-VECM(a, lag=2, estim="ML")


##############################################################################################
# ARDL

ipsa_reg4 <- auto.ardl(ipsa ~ cu)
ipsa_reg4 <- ardl(ipsa ~ cu)
##############################################################################################
