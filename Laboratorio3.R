

########################################
## Universidad del Valle de Guatemala ##
## Laboratorio 2: Data Science        ##
## Autores:                           ##
##    Mayra Silva                     ##
##    Odalis Reyes                    ##           
##    Ivan Maldonado                  ##
########################################



# Instalación de paquetes
#install.packages("rela")
#install.packages("psych")
#install.packages("FactoMineR")
#install.packages("corrplot")
#install.packages("cluster")
#install.packages("fpc")
#install.packages("NbClust")
#install.packages("factoextra")
#install.packages("REdaS")
#install.packages("arules")
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("ggmap")
#install.packages("arulesViz")
#install.packages("dplyr")
#install.packages("caret")
#install.packages("class")
#install.packages("e1071")
#install.packages("fitdistrplus")

require(ggpubr) # Para mejorar la visualización gráfica
require(tidyverse) # Para explotar, manipular y visualizar datos que comparten info
require(corrplot) # Para visualizar la matriz de correlación
require(cluster) #Para calcular la silueta
library(fpc) # Para hacer el plotcluster
library(NbClust) # Para determinar el numero de clusters optimo
library(factoextra) # Para hacer graficos bonitos de clustering
library(rela) # Para poder utilizar paf()
library(psych) # Para poder utilizar KMO()
library(FactoMineR)
library(corrplot)
library(REdaS)
library(ggplot2) # Graficas bonitas
library(ggpubr) # Graficas bonitas x2
#library(ggmap)
library(arules) # Reglas de asociacion
library(factoextra) 
library(arulesViz)
library(dplyr)
library(caret) # Muestreo estratificado
library(class) # Para KNN
library(e1071) # Requisito para la matriz de confusión
library(fitdistrplus) #Para pruebas de normalidad
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)

#setwd("C:/Users/smayr/Documents/Tercer a?o/Semestre 6/Data Science/Laboratorio 3/Laboratorio3")


# Leyendo el dataset de csv importacion
datos <- read.csv("datosImp.csv", TRUE, ",")
# Volviendo el csv en un data frame
class(datos)


#Quitando las columnas llenas de NA
data <- datos[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,16,17,18,19,20,21,22,24,25)]

#quitando ceros
data[is.na(data)] <- 0

#Exploracion rapida

summary(data)


#-------------------------- Analisis exploratorio ------------------------

corr <- cor(data)
# Se visualiza la matriz de correlación de forma gráfica
corrplot(corr)


#Ajuste de normalidad, para la variable diesel. 
descdist(data$Diesel, discrete= FALSE)
dieselfit <- fitdist(data$Diesel,"norm")
plot(dieselfit)


#Ajuste de normalidad para variable de Regular
descdist(data$GasRegular, discrete= FALSE)

#Prueba de normalidad para super
descdist(data$GasSuperior, discrete= FALSE)

superfit <- fitdist(data$GasSuperior,"norm") ## tomar en cuenta qq plot. 
plot(superfit)


# Meses de importacion
dataMonth  <- group_by(data, Mes)
summaryMonth  <- summarise(dataMonth, sumMonth=sum(Total))
plot(summaryMonth, main= 'Meses vs Total de Importaciones', type='l')

#Picos en importaciones de año
ggplot(data, aes(x=as.factor(Anio), y=Total)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Año de importacion")

#Picos en importaciones Diesel 
ggplot(data)+ geom_point(aes(x=Anio, y=Diesel))

#Picos en importaciones super
ggplot(data)+ geom_point(aes(x=Anio, y=GasSuperior), color='blue')
#Picos en importaciones regular
  ggplot(data)+ geom_point(aes(x=Anio, y=GasRegular, color='red'))






# ----------------------------------------------------
# Series temporales de gasolina
# ----------------------------------------------------



# DIESEL
#Ver el gráfico de la serie
diesel.ts<-ts(data$Diesel,start = c(2001,1), end=c(2016,12), frequency = 12)
plot(diesel.ts)
# Descomponiendo la serie de diesel
diesel.ts.desc <- decompose(diesel.ts)
plot(diesel.ts.desc)
# RESUMEN: No muestra tendencia, es estancionaria con la media pero no con la varianza

#Prueba de autocorrelación
acf(diesel.ts) # No es estacionaria
# Prueba de Dickey-Fuller
adf.test(diesel.ts,k=12) # Se rechaza Ho --> no es estacionario

# Volviendo Diesel estacionaria
# Se harán 12 diferenciaciones 
l.diesel <- BoxCox.lambda(diesel.ts) 
l.diesel #fue de 1.80
x.diesel <- BoxCox(diesel.ts, lambda=l.diesel)
plot.ts(x.diesel)
# Eliminación de tendencia
ndiffs(diesel.ts)
# d = 1
diff.diesel1 = diff(x.diesel)
plot(diff.diesel1)
ndiffs(diff.diesel1)
# Eliminación de estacionalidad con diferencias estacionales de orden 12
#diff.diesel2 = diff(diff.diesel1, lag=12)
#plot(diff.diesel2)
#plot(decompose(diff.diesel2))

# Verificación de estacionalidad de media
# Prueba de autocorrelacion
acf(x.diesel,150) # No se puede distinguir is es AR o ARMA
pacf(x.diesel,150)
# p = 7
# q = 4

# Prueba de Dickey-Fuller
adf.test(x.diesel, k = 12) # Ya es estacionaria



# ----------------------------------------------------
# SUPER
# ----------------------------------------------------
#Ver el gráfico de la serie
super.ts<-ts(data$GasSuperior,start = c(2001,1), end=c(2019,6), frequency = 12)
plot(super.ts)
# Descomponiendo la serie de diesel
super.ts.desc <- decompose(super.ts)
plot(super.ts.desc)
# RESUMEN: Sí hay tendencia, no es estacionara con la media pero lo es con la varianza un poco

# Prueba de autocorrelación
acf(super.ts) # No es estacionaria en la media
# Prueba de Dickey-Fuller
adf.test(super.ts, k = 12) # Se rechaza Ho --> no es estacionario

# Volviendo SUPER estacionaria
# Se harán 12 diferenciaciones 
l.super <- BoxCox.lambda(super.ts) 
l.super #fue de 0.55
x.super <- BoxCox(super.ts, lambda=l.super)
plot.ts(x.super)
# Eliminación de tendencia
ndiffs(super.ts)
# d = 1
diff.super1 = diff(x.super)
plot(diff.super1)
# Eliminación de estacionalidad con diferencias estacionales de orden 12
#diff.super2 = diff(diff.super1, lag=12)
#plot(diff.super2)
#plot(decompose(diff.super2))

# Verificación de estacionalidad de media
# Prueba de autocorrelacion
acf(x.super) # No hay tendencia
# Prueba de Dickey-Fuller
adf.test(x.super, k = 12) # No es estacionaria

# Verificación de estacionalidad de media con diferenciación
# Autocorrelación
acf(diff.super1, 150)
pacf(diff.super1, 95)

# Gasolina Super: Esto ya está listo

# ----------------------------------------------------
# REGULAR
# ----------------------------------------------------
#Ver el gráfico de la serie
regular.ts<-ts(data$GasRegular,start = c(2001,1), end=c(2019,6), frequency = 12)
plot(regular.ts)
# Descomponiendo la serie de diesel
regular.ts.desc <- decompose(regular.ts)
plot(regular.ts.desc)
# RESUMEN: Muestra tendencia,no es estancionaria con la media y tampoco con la varianza

#Prueba de autocorrelación
acf(regular.ts) # No es estacionaria
# Prueba de Dickey-Fuller
adf.test(regular.ts) # Se rechaza Ho --> no es estacionario

# Volviendo Regular estacionaria
# Se harán 12 diferenciaciones 
l.regular <- BoxCox.lambda(regular.ts) 
l.regular #fue de 0.45
x.regular <- BoxCox(regular.ts, lambda=l.regular)
plot.ts(x.regular)
# Eliminación de tendencia
diff.regular1 = diff(x.regular)
plot(diff.regular1)
# Eliminación de estacionalidad con diferencias estacionales de orden 12
diff.regular2 = diff(diff.regular1, lag=12)
plot(diff.regular2)
plot(decompose(diff.regular2))

# Verificación de estacionalidad de media
# Prueba de autocorrelacion
acf(diff.regular2) # No hay tendencia
# Prueba de Dickey-Fuller
adf.test(diff.regular2) # Ya es estacionaria


# ----------------------------------------------------
# Modelo ARIMA para diesel
# ----------------------------------------------------

modeloDiesel <- auto.arima(diff.diesel2, stationary=TRUE)



pronosticoDiesel <- forecast(modeloDiesel, level = c(95), h = 120)

autoplot(pronosticoDiesel)

# ----------------------------------------------------
# Modelo ARIMA para super
# ----------------------------------------------------


modeloSuper <- auto.arima(diff.super2, stationary = TRUE)



pronosticoSuper <- forecast(modeloSuper, level = c(95), h = 120)

autoplot(pronosticoSuper)


# ----------------------------------------------------
# Modelo ARIMA para Regular
# ----------------------------------------------------

modeloRegular <- auto.arima(diff.regular2, stationary = TRUE)



pronosticoRegular <- forecast(modeloRegular, level = c(95), h = 120)

autoplot(pronosticoRegular)
