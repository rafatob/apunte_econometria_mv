##############################################################################################
###### Econometr?a de series de tiempo #######################################################
###### Marcelo Villena, PhD Universidad Ib??ez ###############################################
###### Clase I.- Introducci?n a la Econometr?a de Series de Tiempo ##########################
###### Invierno 2018 Pe?alol?n ###############################################################
##############################################################################################
# Set the working directory
setwd("/Users/simon/Desktop/Econometrics/Clase 1.- TSE")
setwd("/Users/rafa_/projects/apunte_econometria_mv")
##############################################################################################

# Ejemplo 1

rm(list=ls())

install.packages("tidyverse")
install.packages("dplyr")
install.packages("tseries")

mydata<-read.csv ("gtemp.csv")
plot(mydata, type="o", ylab="Global Temperature Deviations")

##############################################################################################

# Ejemplo 2
mydata2<-read.csv("sp.csv")
precio<-mydata2$"Adj.Close"
plot.ts(precio, type="o", ylab="Precio Bolsa de Nueva York")
lnprecio <- log10(precio)
Dlnprecio <- diff(lnprecio,1)
plot.ts(Dlnprecio, type="o", ylab="Retorno Bolsa de Nueva York")
summary (lnprecio)
summary (Dlnprecio)

##############################################################################################
# Distribución
h <- hist(Dlnprecio,breaks=15)
xhist <- c(min(h$breaks),h$breaks)
yhist <- c(0,h$density,0)
xfit <- seq(min(Dlnprecio),max(Dlnprecio),length=40)
yfit <- dnorm(xfit,mean=mean(Dlnprecio),sd=sd(Dlnprecio))
plot(xhist,yhist,type="s",ylim=c(0,max(yhist,yfit)), main="Normal pdf
     and histogram")
lines(xfit,yfit, col="red")
shapiro.test(Dlnprecio)

##############################################################################################
# Ruido blanco - White Noise
set.seed(154)
w = rnorm(200,0,1)
par(mar=c(1,1,1,1))
plot.ts(w, ylim=c(-3,3), main="White Noise")

##############################################################################################
# Caminata Aleatoria - Random Walk
set.seed(154)
w = rnorm(200,0,1)
x = cumsum(w)
wd = w + 0.2
xd = cumsum(wd)
plot.ts(xd, ylim=c(-5,55), main="random walk")
lines(x)
lines(0.2*(1:200), lty="dashed")

##############################################################################################
# Promedios m?viles - Moving Averages
w = rnorm(500,0,1) ; v = filter(w, sides=2, rep(1/3,3))
par(mfrow=c(2,1))
plot.ts(w, main="white noise"); plot.ts(v, main="moving average")

##############################################################################################
# Autorregresiones - Autoregressions
w = rnorm(550,0,1) ; x = filter(w, filter=c(1,-.9), method="recursive")[-(1:50)]
plot.ts(x, main= "autoregression")

##############################################################################################
# Descomposici?n de las series de tiempo
t = 2 + 0.1 * 1:500
s = 6.5 * cos(pi * 1:500/90)
set.seed(154)
i = rnorm(500, 0, 5)
plot.ts(s + t + i)

##############################################################################################
# Ejemplo 3
rm(list=ls())
mydata1 <- read.csv ("sp.csv", header =TRUE, stringsAsFactors = FALSE)
precio_sp <- mydata1$"Adj.Close" ; lnprecio_sp <- log10(precio_sp)
Dlnprecio_sp <- diff(lnprecio_sp ,1)
mydata2 <- read.csv ("rut.csv", header= TRUE, stringsAsFactors = FALSE)
precio_rut <- mydata2$"Adj.Close" ; lnprecio_rut <- log10(precio_rut)
Dlnprecio_rut <- diff(lnprecio_rut ,1)
reg1 <- lm ( Dlnprecio_sp ~ Dlnprecio_rut)
summary(reg1)

residuos <- rstandard(reg1)
valores.ajustados <- fitted(reg1)
plot(valores.ajustados, residuos)
qqnorm(residuos)
qqline(residuos)
##############################################################################################
reg2 <- lm ( Dlnprecio_sp ~ 0 + Dlnprecio_rut)
summary(reg2)

residuos2 <- rstandard(reg2)
valores.ajustados2 <- fitted(reg2)
plot(valores.ajustados2, residuos2)
qqnorm(residuos2)
qqline(residuos2)

##############################################################################################
# graficando en R
library(astsa) # para instalar la librer??a - install.packages("nombre")
par(mfrow = c(2,1)) # set up the graphics
tsplot(precio_sp, ylab="S&P Index - daily data")
tsplot(precio_rut, ylab="Russel Index - daily data")

ts.plot(precio_sp, main="S&P and Russel Index - daily data", xlim=c(0, 5000), ylim=c(0, 3000))
lines(precio_rut, lwd=2, col=2)

##############################################################################################