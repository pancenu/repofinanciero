#### Ejercicio Anal?tica Financiera: Tipos de Datos
library(dygraphs)
library(xts)
library(quantmod)
library(dplyr)
options(warn = - 1)  
######################################################
###Primero, generemos una funci?n que ayude a simplificar los tipos de datos que deseamos de la fuente de 
#informaci?n financiera.
#En este ejemplo, los datos de Cierre y Vol?menes, que depender?n del simbolo o ticker 
#del activo y a partir de qu? a?o se consultan:
##Datos:
start<-format(as.Date("2015-01-01"),"%Y-%m-%d")
end<-format(as.Date("2024-07-01"),"%Y-%m-%d")

precios_volumenes <- function(simbolo)
{
  # Obtener precios stocks de Yahoo Finance
  datos <- getSymbols(simbolo, auto.assign = FALSE, from=start, to=end)
  # Eliminando valores faltantes
  datos <- na.omit(datos)
  # Mantenemos columnas con Precios de Cierre y Vol?menes, columnas 4 y 5 de cada stock:
  datos <- datos[, 5:6]
  # Para hacer los datos accesibles, asignamos a Global Environment:
  assign(simbolo, datos, envir = .GlobalEnv)
}

# Llamamos la funci?n para cada stock desde el 2014:
precios_volumenes("IBM")
precios_volumenes("ORCL")
precios_volumenes("INTC")
precios_volumenes("MSFT")

# Juntamos los datos y renombramos las columnas:
PyV <- merge.xts(IBM, ORCL, INTC, MSFT)
colnames(PyV) <- c("IBM Transados","IBM P.Ajustados", "Oracle Transados","Oracle P.Ajustados", 
                   "INTEL Transados", "INTEL P.Ajustados", "Microsoft Transados", "Microsoft P.Ajustados")

##Serie De Tiempo:
# Podemos generar una gr?fica interactiva las variables, en este caso de los precios:
Precios<-  dygraph(PyV[,c(2,4,6,8)], main = "Precios de IBM, Oracle, INTEL y Microsoft") %>%
  dyAxis("y", label = "Precios") %>%
  dyRangeSelector(dateWindow = c("2015-01-01", "2020-12-31")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1")) 
Precios

# Podemos ver los 5 ultimos datos redondeando hasta 3 decimales:
round(tail(PyV, n = 5), 3)

#########################################################################################################
# Ejemplo de Panel Data, generemos una list de objetos dygraphs, y para imprimirlos usamos htmltools:
library(dygraphs)
library(htmltools)
dy_graficos <- list(
  dygraphs::dygraph(PyV[,c(1,3,5,7)], main = "Volumen de IBM, Oracle, Intel, y Microsoft"), 
  dygraphs::dygraph(PyV[,c(2,4,6,8)], main = "Precio de IBM, Oracle, Intel, y Microsoft"))


# Representemos los objetos dygraphs usando htmltools
htmltools::browsable(htmltools::tagList(dy_graficos))


#-------------------------------------------------------------------
#-------Datos tipo Transversales o Cross Sectional
# Seleccionaremos los datos de AMZN del 2014 y del 2020. 
# Empecemos seleccionando los aqos 2014 de AMZN que es la 1ra columna.

MSFT_2015<-subset(PyV[,8], index(PyV)>="2015-01-01"& index(PyV)<="2015-12-31")
MSFT_2015[c(7:8, nrow(MSFT_2015))]
#Para el aqo 2020:
MSFT_2020<-subset(PyV[,8], index(PyV)>="2020-01-01"& index(PyV)<="2020-12-31")
MSFT_2020[c(7:8, nrow(MSFT_2020))]

# Ahora, podemos tambien visualizarlo, elegimos un histograma  
par(mfrow=c(2,1))

hist(MSFT_2015, freq = FALSE, col="yellow", border="blue",main= "Densidades de los Precios MSFT en 2015", xlab = "Precios Cierre")
lines(density(MSFT_2015), lwd = 2, col = 'red')
hist(MSFT_2020, freq = FALSE, col="blue", border="blue",main= "Densidades de los Precios MSFT en 2020", xlab = "Precios Cierre")
lines(density(MSFT_2020), lwd = 2, col = 'red')

