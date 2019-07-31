##############################################################################################
###### Econometr??a de series de tiempo #######################################################
###### Marcelo Villena, PhD Universidad Ib??ez ###############################################
###### Clase III.- Vector Autoregressive Models ##############################################
###### Invierno 2018 Pe?alol?n ###############################################################
##############################################################################################
# Set the working directory
setwd("/Users/simon/Desktop/Econometrics/Clase 3.- TSE")

##############################################################################################
# Ejemplo 1 VAR economia canadiense

rm(list=ls())
#install.packages("vars")
library("vars")
mydata<-read.csv ("Canada.csv")
prod<-ts(mydata$prod,frequency=4, start = c(1980,1))
e<-ts(mydata$e,frequency=4, start = c(1980,1))
U<-ts(mydata$U,frequency=4, start = c(1980,1))
rw<-ts(mydata$rw,frequency=4, start = c(1980,1))
summary(mydata)

par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
plot(prod, main="Labour productivity")
plot(e, main="Emplyment")
plot(U, main="Unemployment rate")
plot(rw, main="real wages")

##############################################################################################
# ra??z unitaria

adf1 <- summary(ur.df(prod, type = "trend", lags = 2))
adf1
adf2 <- summary(ur.df(prod, type = "drift", lags = 1))
adf2

##############################################################################################
# VAR estimation

VARselect(mydata, lag.max = 8, type = "both")
p1ct <- VAR(mydata, p = 1, type = "both")
p1ct

summary(p1ct, equation = "e")
plot(p1ct, names = "e")

##############################################################################################
# Chequeo Errores

ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
ser11$serial

norm1 <- normality.test(p1ct)
norm1$jb.mul

##############################################################################################
# irf 
var.irf <- irf(p1ct, impulse = "e", response = c("U", "prod", "rw"), n.ahead = 10, boot = TRUE)
plot(var.irf)

var.irf1 <- irf(p1ct, impulse = "e", response = "U", n.ahead = 10, boot = TRUE)
plot(var.irf1)

prd <- predict(p1ct, n.ahead = 10, ci = 0.95, dumvar = NULL)
print(prd)
plot(prd, "single")

##############################################################################################

fevd.U <- fevd(p1ct, n.ahead = 48)$U
summary(fevd.U)

##############################################################################################
# Test for Granger Causality

var.cg <- VAR(mydata, p = 2, type = "const") 
causality(var.cg, cause = "e")
grangertest(prod ~ e, order=4)

##############################################################################################
for (i in 1:4)
{
  cat("LAG =", i)
  print(causality(VAR(mydata, p = i, type = "const"), cause = "e")$Granger)
}
##############################################################################################


