

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

setwd("C:/Users/smayr/Documents/Tercer a�o/Semestre 6/Data Science/Laboratorio 3/Laboratorio3")


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

#Picos en importaciones de año

ggplot(data, aes(x=as.factor(Anio), y=Total)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Año de importacion")






# ----------------------------------------------------
# Series temporales de gasolina
# ----------------------------------------------------



# DIESEL


#Ver el gráfico de la serie
diesel.ts<-ts(data$Diesel,start = c(2001,1), end=c(2019,6), frequency = 12)
plot(diesel.ts)
# Descomponiendo la serie de diesel
diesel.ts.desc <- decompose(diesel.ts)
plot(diesel.ts.desc)

# RESUMEN: No muestra tendencia, es estancionaria con la media pero no con la varianza
#Transformacion de la serie

dieselTrans<- log(diesel.ts)
tm <- cbind(dieselTrans, diesel.ts)
plot.ts(tm)
#↓ot(decompose(dieselTrans))

#no es eficiente
lambda<- BoxCox.lambda(diesel.ts) 
lambda #fue de  1.8
plot.ts(BoxCox(diesel.ts, lambda=0.7))

#elimnando la tendencia

m <- acf(diff(diesel.ts), plot=TRUE)
m




# SUPER
#Ver el gráfico de la serie
super.ts<-ts(data$GasSuperior,start = c(2001,1), end=c(2019,6), frequency = 12)
plot(super.ts)
# Descomponiendo la serie de diesel
super.ts.desc <- decompose(super.ts)
plot(super.ts.desc)
# RESUMEN: Sí hay tendencia, no es estacionara con la media pero lo es con la varianza un poco


# REGULAR
regular.ts<-ts(data$GasRegular,start = c(2001,1),frequency = 12)
plot(regular.ts)
# Descomponiendo la serie de diesel
start(data$GasRegular)
regular.ts.desc <- decompose(regular.ts)
plot(regular.ts.desc)




# ----------------------------------------------------
# Modelo ARIMA para diesel
# ----------------------------------------------------

auto.arima(data$Diesel)

# Iniciando el modelo de ARIMA
fit <- arima(log(data$Diesel), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))




