###########################
######   Problema 2  ######
### ratones  comensales ###
###########################
# En el marco de un programa de control de roedores comensales (roedores asociados a ambientes antropizados) en localidades de la provincia de Córdoba, se desea saber si la industrialización favorece o no el desarrollo de estos roedores.


rm(list=ls())
library(ggplot2)     # La vamos a utilizar para graficas
library(dplyr)       # La vamos a utilizar para armar tablas resumen
library(car)         # La vamos a utilizar para analisis de algunos supuestos 
library(glmmTMB)     # La vamos a utilizar para ajustar el modelo 
library(DHARMa)      # La vamos a utilizar para analizar supuestos
library(performance) # La vamos a utilizar para analizar supuestos y más ...
library(emmeans)     # La vamos a utilizar para realizar comparaciones 
library(ggeffects)   # La vamos a utilizar para obtener (y graficar) los valores predichos por el modelo
library(sjPlot)      # Alternativa para ggeffcets y grafico de efectos aleatorios

setwd("~/Carrera/Biometria II/tp8")
datos <- read.delim("Rcomensales.txt", stringsAsFactors = T)
str(datos)
summary(datos)
head(datos, 10)

# a) Identifique la variable respuesta y las variables explicativas (tipo y condición de fijas y aleatorias). ¿Cuál es el diseño muestral aplicado? ¿Cuál es la potencial distribución de probabilidad de la variable respuesta? Interprete los parámetros de la misma en términos del problema.

# Variable respuesta: cantidad de presas ratones comensales por localidad 
# posible distribucion de probabilidad: Binomial (pi_i, n)
# siendo pi_i la probabilidad de que la presa sea raton y n la cantidad de egagropilas evaluada por localidad 

# Variables explicativas:
# sup nativa, fija, cuanti, cruzada en la interaccion nivel inducstrial y localidad
# nivel industrial, fija, cuali
# localidad, aleatroia, cuali, anidada en nivel industrial

# diseño anidado


#b) Realice un análisis exploratorio apropiado para los datos.

#c) Indique cuáles son los tres componentes del modelo lineal generalizado. Escriba el modelo en parámetros y en términos del problema. ¿Cuáles son los supuestos?   ¿Cuántos (y cuáles) son los parámetros estimados por el modelo?

#d) Ajuste el modelo en R. Analice los supuestos del modelo.

#e) ¿Cómo se relacionan la proporción de roedores comensales en la dieta con la superficie de pastizal nativo? Concluya de manera general para las variables} involucradas. Informe cómo se modifica el odd de la proporción de roedores comensales en la dieta con el aumento de la superficie de pastizal (IC95%).

#f) Grafique el modelo predicho para cada nivel de industrialización y se indique si existen o no diferencias estadísticamente significativas entre ellos.

#g) Prediga cuál será la proporción de roedores comensales en la dieta para una localidad con nivel de industrialización grado 2 y con una superficie de pastizal de 0.5km2 , sabiendo que la superficie promedio de pastizal fue de 0.35km2

#h) ¿Cuáles son las conclusiones generales del estudio y sobre quiénes se aplican?