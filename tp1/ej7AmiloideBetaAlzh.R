# Problema 7 - Terapia con anticuerpos para enfermedad de Alzheimer
# Se efectuó un ensayo para evaluar la eficacia del tratamiento con anticuerpos anti-Aβ. Para ello, ratones transgénicos PDAPPAJ20 con EA fueron sometidos a tratamiento con anticuerpos anti-Aβ. Al inicio, 1 mes, 2 meses y 3 meses de tratamiento los ratones fueron sacrificados y por ELISA se determinó la masa total de amiloide beta en cerebro (en ng)
#Se aplicó un análisis de regresión simple


rm(list=ls())
library(car)
library(dplyr)
library(ggplot2)

setwd("C:/Users/snpre/Documents/Carrera/Biometria II/tp1 ") 
data<-read.csv("terapiaEA.csv", dec=",")
head(data)
str(data)
#1. Indique la cantidad de réplicas y el tipo de variables involucradas.

# Se evaluaron 20 ratones a los que se les aplico "tratamientos" distintos. Esos tratamientos fueron el tiempo que dejaron actuar a los anticuerpos
# Se asignaron 5 ratones por tratamiento, por lo que hay 5 replicas

# Tiempo será tratada como una variable cuantitativa
# Amiloide (masa total en cerbero en ng) es la variable respuesta y es cuantitativa continua, proponemos una distribucion normal

#2. Describa gráfica y estadísticamente los datos.

summary(data)

ggplot(data=data, aes(x=tiempo, y=amiloide))+ geom_point(color="navy")+ geom_smooth(method = "lm", color="darkcyan")

# Tablas resumen
resumen <- data[2:3] %>% 
  group_by(tiempo) %>%
  summarise_all(.funs = c(
    media = mean, 
    DE = sd
  )
  )
resumen
# Vemos que la media del deposito amiloide decae con el tiempo


#3. Sabiendo que se aplicó un análisis de regresión simple, analice cómo se modifica la masa total de amiloide beta en el cerebro con el tiempo luego de aplicado el tratamiento con anticuerpos. Informe la magnitud del efecto en el contexto del ensayo.
m1<- lm(amiloide~tiempo, data = data)

summary(m1)

# h0: la variacion de carga amiloide en el cerebro no se explica linealmente por el cambio en el tiempo
# Rechazo, si se explica (VEO SIGNIFICANCIA ESTADISTICA EN LA PENDIENTE)

# Ademas, puedo mirar el R2(0.44) que me dice la capacidad predictiva del modelo. Esto implica que el 44% de la variabilidad del modelo esta explicada por la variacion en el tiempo

# La estimacion de la pendiente de nuestro modelo es -30.94. Es decir, por cada aumento unitario del tiempo la masa amiloide beta promedio en el cerebro disminuye significativamente en 80.94 ng.
confint(m1)
# El IC para la pendiente es de -48.26 a -13.69

# En conclusión, el tratamiento disminuye la carga amiloide beta en el cerebro en promedio entre -48 y -14 con 95% de confianza.
