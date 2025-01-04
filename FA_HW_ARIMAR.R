### Tutorial Analítica Financiera. Modelos de Ajuste y Pronóstico Datos Financieros
#install.packages("fpp2")
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
#install.packages("Metrics")
library(Metrics)
library(FitAr)
#remotes::install_github("cran/FitAR")
#install.packages('forecast',dependencies=TRUE)
options(digits = 3)
options(warn = -1)

###Se obtienen los precios de AMAZON
AMZN<-getSymbols("AMZN", from="2020-08-01", to="2021-03-31",src="yahoo", auto.assign = FALSE) #
# Se eliminan los valores faltantes
AMZN<-na.omit(AMZN)
# Se nabtuebeb los volumenes con precios de cierre: columna 4:
AMZN<-AMZN[,4]
# Se puede graficar:
plot(AMZN, ylab="Precios")
length(ANZN)
# Se parten las series, se toma el 7% para la prueba
h<-round(length(AMZN)*0.07, digits=0)
h
train<-AMZN[1:(nrow(AMZN)-h),]
test<-AMZN[(nrow(AMZN)-h+1):nrow(AMZN), ]

####################################################
### Modelos ARIMA
### Veamos si la serie es estacionaria:
adfTest(train)
### Como no es estacionaria se diferencia y vemos si ya es estacionaria
dtrain<-diff(train)[-1,]
adfTest(dtrain) #con fUnitRoots
adf.test(dtrain) #con tseries

##OJO2   ## Gráficas de ambos
par(mfrow=c(2,1))
plot(train, col="red")
plot(dtrain, col="blue")

## Ya estacionaria, definimos candidatos de modelos ARMA
library(TSA)
m<-eacf(dtrain,15,10) #Seria un ARMA (7,3) pero si deseamos expresarla como ARIMA, #Sería ATIMA(7,1,3), pues

### Se definen otros modelos mediante la función auto.arima()
m2<-auto.arima(train, seasonal=TRUE)
summary(m2) # Seria arima(1,0,0)
 
#Modelación:
mod1<-Arima(train,order = c(7,1,3), method = "ML")
summary(mod1)
coeftest(mod1)
tsdiag(mod1) ##residuos sw ven ok

mod2<-Arima(train,order = c(1,0,0), method = "ML")
mod2
coeftest(mod2)
tsdiag(mod2) ##residuos sw ven ok

### Pronósticos
library(FitAR)
library(tseries)
library(forecast)
### modelos pronóstico para m1 y m2:
Pron_m1<-forecast(mod1,h)
Pron_m2<-forecast(mod2,h)

summary(Pron_m1)
summary(Pron_m2)

### Gráficas
par(mfrow=c(2,1))
plot(Pron_m1, include=50)
plot(Pron_m2, include=50)

## Otro gráfico integral:
## Se pasan a ts todos los datos, son 154 datos en la parte de train:
traints<-ts(train, start=c(2020,08,01),frequency = 154)
fitted1<-ts(mod1$fitted, start=c(2020,08,01),frequency = 154)
fitted2<-ts(mod2$fitted, start=c(2020,08,01),frequency = 154)
pron1<-ts(Pron_m1$mean, start=c(2021,08),frequency = 154)
pron2<-ts(Pron_m2$mean, start=c(2021,08),frequency = 154)

autoplot(traints)+
  autolayer(fitted1, series="ARIMA (7,1,3,)")+
  autolayer(fitted2, series="ARIMA (1,0,0,)")+
  autolayer(pron1, series="Pron Arima(7,1,3)")+
  autolayer(pron2, series="Pron Arima(1,0,0)")

### Se mide el error de pronóstico RMSE y MAPE
library(Metrics)
RMSE_arima<-rmse(test, Pron_m1$mean)
RMSEar1<-rmse(test, Pron_m2$mean)

MAPE_arima<-mape(test, Pron_m1$mean)
MAPEar1<-mape(test, Pron_m2$mean)

### Imprimir resultados al momento
### Imprimir los resultados en una tabla:

Modelo<-c("ARIMA(7,1,3)", "AR(3)" )

RMSE<-c(RMSE_arima, RMSEar1)

MAPE<-c(MAPE_arima, RMSEar1)

res<-data.frame(Modelo, RMSE, MAPE)

print(res)



################################################################################
##################### Modelos de suavizamiento exponencial #####################
###Se obtienen los precios de AMAZON
AMZN<-getSymbols("AMZN", from="2020-08-01", to="2021-03-31",src="yahoo", auto.assign = FALSE) #
# Se eliminan los valores faltantes
AMZN<-na.omit(AMZN)
# Se nabtuebeb los volumenes con precios de cierre: columna 4:
AMZN<-AMZN[,4]
# Se puede graficar:
plot(AMZN, ylab="Precios")
length(ANZN)
# Se parten las series, se toma el 7% para la prueba
h<-round(length(AMZN)*0.07, digits=0)
h
train<-AMZN[1:(nrow(AMZN)-h),]
test<-AMZN[(nrow(AMZN)-h+1):nrow(AMZN), ]
################################################################################
### A partir de los mismos datos, se vuelve a graficar la serie
plot(train, col="red")

traints<-ts(train, start=c(2020,08,01), frequency=154)
plot(traints)

## No hay una estacionalidad evidente,  se prueban modelos de suavizamiento simples
## Posibles enfoques de suavizamiento:
##
## Primer modelo:
fit1<-ses(traints, h=12)
summary(fit1)
ffit1<-forecast(fit1, h=12)
autoplot(fit1) + autolayer(fitted(fit1))

################################################################################
# Segundo Modelo: Tendencia lineal con Holt, podríamos probar aún este, aunque no hay tendencia evidente:
fit2<-holt(traints, h=12)
summary(fit2)
ffit2<-forecast(fit2, h=12)
autoplot(fit2) + autolayer(fitted(fit2))

################################################################################
# Tercer Modelo: 
fit3<-HoltWinters(traints, alpha= NULL, beta= NULL, gamma= FALSE)
fit3
ffit3<-forecast(fit3, h= 12)
autoplot(traints)+ autolayer(ffit3)

# Cuarto Modelo: Se prueba la aplicación de ets(), se deja que el modelo determine
fit4<-ets(train, model="ZZZ", damped= FALSE, alpha= NULL, beta= NULL, 
          gamma= NULL, phi= NULL, lambda = FALSE, biasadj = FALSE, 
          additive.only = FALSE, restrict = TRUE,
          allow.multiplicative.trend = FALSE)
summary(fit4)
ffit4=forecast(fit4, h = 12)
autoplot(ffit4, include = 50) ##original include all ffit4

library(Metrics)
RMSEses<-rmse(test, ffit1$mean)
RMSEholt<-rmse(test,ffit2$mean)
RMSE_HW<-rmse(test,ffit3$mean)
RMSEets<-rmse(test,ffit4$mean)

MAPEses<-mape(test, ffit1$mean)
MAPEholt<-mape(test,ffit2$mean)
MAPE_HW<-mape(test,ffit3$mean)
MAPEets<-mape(test,ffit4$mean)

### Impresión de resultados en una tabla:

Modelo<-c("ARIMA(7,1,3)", "AR(3)", "ses", "holt", "Hw", "ets")

RMSE<-c(RMSE_arima, RMSEar1, RMSEses, RMSEholt, RMSE_HW, RMSEets)

MAPE<-c(MAPE_arima, MAPEar1, MAPEses, MAPEholt, MAPE_HW, MAPEets)

res<-data.frame(Modelo,RMSE, MAPE)

print(res)

