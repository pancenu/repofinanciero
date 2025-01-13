###################Tutorial Anal?tica Financiera. Modelos de Ajuste y Pron?stico Datos Financieros
install.packages("fpp2")
library(fpp3)
library(PerformanceAnalytics)
library(xts)
library(quantmod)
library(fUnitRoots)
library(forecast)
library(ggplot2)
library(tseries)
library(lmtest)
library(TSA)
install.packages("Metrics")
library(Metrics)
library(FitAR)
install.packages('forecast', dependencies = TRUE)
options(digits = 3)
options(warn = - 1)  

##Obtenemos precios de AMAZON
AMZN<-getSymbols("AMZN", from="2020-08-01",to="2021-03-31", src = "yahoo", auto.assign = FALSE) #
# Eliminando valores faltantes
AMZN <- na.omit(AMZN)
# Mantenemos columnas con Precios de Cierre  columna 4:
AMZN <- AMZN[,4]
##Podemos graficar:
plot(AMZN, ylab="Precios")
length(AMZN)
##Partimos serie, tomemos el 7% para la prueba
h <- round(length(AMZN)*0.07, digits = 0 )
h
train <- AMZN[1:(nrow(AMZN) - h), ]
test<- AMZN[(nrow(AMZN) - h + 1):nrow(AMZN), ]

#######################################################
############ Modelos ARIMA ############################
###Veamos si la serie es estacionaria:
adfTest(train)

##Como no es estacionaria, la diferenciamos y vemos si ya es estacionaria:
dtrain<-diff(train)[-1,]
adfTest(dtrain)  #con libreria fUnitRoorts
adf.test(dtrain)  #con librer?a tseries

##Gr?ficas de ambos
par(mfrow=c(2,1))
plot(train, col="red")
plot(dtrain, col="blue")

##Ya estacionaria, definimos candidatos de modelos ARMA
library(TSA)
m<-eacf(dtrain, 15,10)  #Seria un ARMA(7,3), pero si deseamos expresarla como ARIMA, ser?a: ARIMA (7,1,3), pues la diferenciamos una vez para hacerla estacionaria.

#Definamos otros modelos mediante la funci?n auto.arima()
m2<-auto.arima(train, seasonal = TRUE)
summary(m2)   #Ser?a arima(1,0,0)

###Modelaci?n:
mod1<-Arima(train, order=c(7,1,3), method = "ML")
summary(mod1)
coeftest(mod1)
tsdiag(mod1)  ##residuos sw ven ok.

mod2<-Arima(train, order=c(1,0,0), method = "ML")
mod2
coeftest(mod2)
tsdiag(mod1)   #Residuos se ven ok
##Pronosticos
library(FitAR)
#install.packages('forecast', dependencies = TRUE)
library(tseries)
library(forecast)
### Modelos pron?stico para m1 y m2:
Pron_m1<-forecast(mod1, h)
Pron_m2<- forecast(mod2, h)

summary(Pron_m1)
summary(Pron_m2)

##Gr?ficos
par(mfrow=c(2,1))
plot(Pron_m1, include=50)
plot(Pron_m2, include=50)

##Otro Gr?fico integral:
##pasamos a ts los datos, son 154 datos en la parte train:
traints<-ts(train, start=c(2020,08,01), frequency = 154)
fitted1<-ts(mod1$fitted,start=c(2020,08,01), frequency = 154 )
fitted2<-ts(mod2$fitted,start=c(2020,08,01), frequency = 154 )
pron1<-ts(Pron_m1$mean, start = c(2021,08), frequency = 154)
pron2<-ts(Pron_m2$mean, start = c(2021,08), frequency = 154)

autoplot(traints)+
 autolayer(fitted1, series="ARIMA (7,1,3)")+
 autolayer(fitted2, series="ARIMA (1,0,0)")+
  autolayer(pron1, series="Pron Arima (7,1,3)")+
  autolayer(pron2, series="Pron Arima (1,0,0)")

#### Midamos el error de pron?stico, RMSE y MAPE:
library(Metrics)
RMSE_arima<-rmse(test, Pron_m1$mean)
RMSEar1<-rmse(test, Pron_m2$mean)

MAPE_arima<-mape(test, Pron_m1$mean)
MAPEar1<-mape(test, Pron_m2$mean)

##imprimir los resultados al momento:
###Imprimamos los resultados en una tabla:

Modelo<-c("ARIMA(7,1,3)", "AR(3)")

RMSE<-c(RMSE_arima, RMSEar1)

MAPE<-c(MAPE_arima, MAPEar1)

res<-data.frame(Modelo,RMSE, MAPE)

print((res))



#######################################################
############ Modelos Suavizamiento Exponencial ########
##Obtenemos precios de AMAZON
AMZN<-getSymbols("AMZN", from="2020-08-01",to="2021-03-31", src = "yahoo", auto.assign = FALSE) #
# Eliminando valores faltantes
AMZN <- na.omit(AMZN)
# Mantenemos columnas con Precios de Cierre  columna 4:
AMZN <- AMZN[,4]
##Podemos graficar:
plot(AMZN, ylab="Precios")
length(AMZN)
##Partimos serie, tomemos el 7% para la prueba
h <- round(length(AMZN)*0.07, digits = 0 )
h
train <- AMZN[1:(nrow(AMZN) - h), ]
test<- AMZN[(nrow(AMZN) - h + 1):nrow(AMZN), ]
####
## A partir de los mismos datos, grafiquemos la serie:
plot(train, col="red")

traints<-ts(train, start=c(2020,08,01), frequency = 154)
plot(traints)

##No hay una estacionalidad evidente, por lo que probamos modelos de suavizamiento simples
###Posibles enfoques de suavizamiento: 
##
###Primer Modelo:
fit1<-ses(traints, h=12 )
summary(fit1)
ffit1<-forecast(fit1, h=12)
autoplot(fit1) +  autolayer(fitted(fit1))

#############
#Segundo Modelo: Tendencia Lineal con Holt, podr?amos probar a?n este, auque no hay tendencia evidente:
fit2 <- holt(traints,h=12)
summary(fit2)
ffit2<-forecast(fit2, h=12)
autoplot(fit2) +  autolayer(fitted(fit2))

##############
###Tercer Modelo: Holt con correcci?n a la deriva (sin estacionalidad):
fit3<-HoltWinters(traints, alpha = NULL, beta=NULL, gamma = FALSE)
fit3
ffit3<-forecast(fit3, h=12)
autoplot(traints)+autolayer(ffit3)

###Cuarto Modelo: Probemos la aplicaci?n de ets(), que deje determine el modelo
fit4<-ets(train, model="ZZZ", damped=FALSE, alpha=NULL, beta=NULL,
   gamma=NULL, phi=NULL, lambda=FALSE, biasadj=FALSE,
   additive.only=FALSE, restrict=TRUE,
   allow.multiplicative.trend=FALSE)
summary(fit4)
ffit4<-forecast(fit4, h=12 )
autoplot(forecast(fit4,h=12), include=50)
##############
##M?trica Desempe?o pron?stico:
library(Metrics)
RMSEses<-rmse(test, ffit1$mean)
RMSEholt<-rmse(test, ffit2$mean)
RMSE_HW<-rmse(test, ffit3$mean)
RMSEets<-rmse(test, ffit4$mean)

MAPEses<-mape(test, ffit1$mean)
MAPEholt<-mape(test, ffit2$mean)
MAPE_HW<-mape(test, ffit3$mean)
MAPEets<-mape(test, ffit4$mean)


###Imprimamos los resultados en una tabla:

Modelo<-c("ARIMA(7,1,3)", "AR(3)", "ses", "holt", "HW", "ets")

RMSE<-c(RMSE_arima, RMSEar1, RMSEses, RMSEholt, RMSE_HW, RMSEets)

MAPE<-c(MAPE_arima, MAPEar1, MAPEses, MAPEholt, MAPE_HW, MAPEets)

res<-data.frame(Modelo,RMSE, MAPE)

print((res))


#######################################################################
#################  Red Neuronal tipo Feed Forward Neural ##############
##Neural Network Time Serie Regression ##https://pkg.robjhyndman.com/forecast/reference/nnetar.html
##Funci?n:nnetar: "Feed-forward neural networks with a single hidden layer and lagged inputs for forecasting univariate time series."
library(forecast)
library(ggplot2)
#####
##Obtenemos precios de AMAZON
AMZN<-getSymbols("AMZN", from="2020-08-01",to="2021-03-31", src = "yahoo", auto.assign = FALSE) #
# Eliminando valores faltantes
AMZN <- na.omit(AMZN)
# Mantenemos columnas con Precios de Cierre  columna 4:
AMZN <- AMZN[,4]
##Podemos graficar:
plot(AMZN, ylab="Precios")
length(AMZN)
##Partimos serie, tomemos el 7% para la prueba
h <- round(length(AMZN)*0.07, digits = 0 )
h
train <- AMZN[1:(nrow(AMZN) - h), ]
test<- AMZN[(nrow(AMZN) - h + 1):nrow(AMZN), ]
##################################################
## A partir de los mismos datos, grafiquemos la serie:
plot(train, col="red")
autoplot(train)

## Generamos la funci?n de pron?stico. En datos de precios, se deben transformar 
#los datos lambda para tratar que los residuos sean cercanos a homoced?sticos.  
nn1 <- nnetar(train, lambda = TRUE)
nn1

autoplot(forecast(nn1,PI=TRUE, h=12), include=50) #50 últimos, se puede omitir (todos)
fnn1<-forecast(nn1,h=12)

## AR Nivel, recordemos que en la primera parte, ten?amos un modelo ARMA con la parte AR(7) 
#que podemos incluir:

nn2=nnetar(train, p=7, lambda=TRUE)
nn2
autoplot(forecast(nn2,PI=TRUE, h=12))
fnn2<-forecast(nn2,h=12)

##C?lculo de las m?ricas de error de pron?stico:
library(Metrics)
RMSE_nnetar<-rmse(test, fnn1$mean)
MAPE_nnetar<-mape(test, fnn1$mean)
RMSE_nnetar2<-rmse(test, fnn2$mean)
MAPE_nnetar2<-mape(test, fnn2$mean)

###Imprimamos los resultados en una tabla:

Modelo<-c("ARIMA(7,1,3)", "AR(3)", "ses", "holt", "HW", "ets", "nnetar_z", "nnetar_ar7")

RMSE<-c(RMSE_arima, RMSEar1, RMSEses, RMSEholt, RMSE_HW, RMSEets, RMSE_nnetar, RMSE_nnetar2)

MAPE<-c(MAPE_arima, MAPEar1, MAPEses, MAPEholt, MAPE_HW, MAPEets,MAPE_nnetar, MAPE_nnetar2)

res<-data.frame(Modelo,RMSE, MAPE)

print((res))










install.packages("tsfeatures")
library(tsfeatures)
nonlinearity(train)
entropy(train)









