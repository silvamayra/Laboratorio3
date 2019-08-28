
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


setwd("C:/Users/smayr/Documents/Tercer año/Semestre 6/Data Science/Laboratorio 3/Laboratorio3")


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


#meses de mayor venta
plot(data$Mes, data$Total)
