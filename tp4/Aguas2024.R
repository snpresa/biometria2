#### AGUAS 2024

#### Problema Caracterización de aguas residuales 


# Antes que nada veamos si hay algun comando u objeto almacenado en la memoria
ls()
# Si te aparece character(0) es que la memoria esta limpia, si no ejecuta
# la siguiente sentencia para borrar la memoria.
rm(list=ls())
# revisamos...
ls()
# No deberia haber nada en el entorno de trabajo ahora!


#### setear el directorio de trabajo .... (cada uno tendra una ruta diferente)

# setwd("~/")

#### Cargar el data.frame (si esta en el mismo directorio seteado no es necesario indicar la ruta)

Datos  <- read.csv("AGUAS.csv", stringsAsFactors = T) 
str(Datos) 
names(Datos)
head(Datos)

# librerias necesarias:
library(ggplot2)
library(emmeans)
library(car)
library(dplyr)

#### Exploramos la estructura del data.frame (dimensiones, tipo de variables, datos faltantes, etc.)

str(Datos)
head(Datos)
names(Datos)
summary(Datos)

## Tablas resumen 
Datos %>% 
  select(fuente, DBO) %>%
  group_by(fuente) %>%
  summarise_all(.funs = c(
    n = length, 
    media_DBO = mean, 
    sd = sd, 
    min = min, 
    max = max
  )
  )

Datos %>% 
  select(fuente, pH) %>%
  group_by(fuente) %>%
  summarise_all(.funs = c(
    n = length, 
    media_pH = mean, 
    sd = sd, 
    min = min, 
    max = max
  )
  )

## Grafico
DBO_vs_pH <-ggplot(Datos, aes(x =pH , y = DBO)) +  geom_point(aes(), colour ="deepskyblue", size=4)
DBO_vs_pH <-DBO_vs_pH+ xlab("pH") +  ylab("DBO") +  ggtitle("DBO vs pH") 
DBO_vs_pH

DBO_vs_pHyFue <-ggplot(Datos, aes(x =pH , y = DBO, colour =fuente)) + geom_point(size=2)
DBO_vs_pHyFue <- DBO_vs_pHyFue + xlab("pH") +  ylab("DBO") +  ggtitle("DBO vs pH y fuente")
DBO_vs_pHyFue 

## con rectas de regresion lineal (lm)
p1 <- ggplot(Datos, aes(x =pH , y = DBO, colour =fuente)) + geom_point(size=2)
q1 <- p1 + xlab("pH") +  ylab("DBO") +  ggtitle("DBO en función del pH y la fuente")
r1 <- q1 + geom_smooth(method = "lm", se = FALSE)
r1



#Modelo con interaccion

modelo1<-lm(DBO ~ pH*fuente, Datos)
anova(modelo1)
summary(modelo1)

# antes de analizar los resultados:

#Supuestos
e<-resid(modelo1) # residuos
re<-rstandard(modelo1) #residuos estandarizados
pre<-predict(modelo1) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo 1" )
abline(0,0)

qqPlot(e, main = "QQplot -Modelo 1")

shapiro.test(e)


# analizar resultados

summary(modelo1)

# existe relacion lineal entre DBO y pH? 
# difiere entre las fuentes?

# plot modelo 1
library(ggeffects)
(a<-ggpredict(modelo1, 
                      terms = c("pH", "fuente"),
                      interval = "confidence")  ) 

p<-plot(a, add.data = TRUE)

p + ggtitle("Relación predicha entre la DBO y el pH") +
  labs(x="pH") + labs(y="DBO (mg/l) ")


### Escribir las Ecuaciones estimadas para el modelo con interaccion



# Comparaciones

options(emmeans= list(emmeans = list(infer = c(TRUE, F)),
                      contrast = list(infer = c(TRUE, TRUE)))) # acá estamos seteando la salida de las tablas del emmeans , el primer True de cada paréntesis es para pedir los intervalos de confianza y el segundo es para pedir los valores P. 


# Comparacion de pendientes de las 3 fuentes (relacion entre DBO y pH para cada fuente)

comp_pendientes <- emtrends(modelo1,  pairwise ~ fuente, var="pH") #, contr="cld"
comp_pendientes
plot(comp_pendientes$emtrends, comparisons = TRUE)


# Comparaciones de los valores de DBO medios de cada fuente

comp_fuentes <- emmeans(modelo1, pairwise ~ fuente|pH)
comp_fuentes #dif entre fuentes para pH promedio

plot(comp_fuentes$emmeans, comparisons = TRUE)

# A que valor de pH se efectuaron esas comparaciones? (mirar salida de emmeans)

## algo interesante: 
## Agregando un argumento podemos optar por comparar las fuentes en el min y max  pH: 

Fuentes_en_PH_PH_min_max <- emmeans(modelo1, pairwise~ fuente:pH, cov.reduce = range)
Fuentes_en_PH_PH_min_max
plot(Fuentes_en_PH_PH_min_max$emmeans, comparisons = TRUE)


# Mismo análisis pero centrando en el valor de pH promedio
#de pH para que la ordenada sea interpretable 

# valor de pH promedio:
mean(Datos$pH) # 7.816667
# centrado
Datos$pH_c <- Datos$pH - mean(Datos$pH)

modelo3<-lm(DBO ~ pH_c*fuente, Datos)

#Supuestos
e<-resid(modelo3) # residuos
re<-rstandard(modelo3) #residuos estandarizados
pre<-predict(modelo3) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo 4" )
abline(0,0)
qqPlot(e, main="QQplot - Modelo 4")
shapiro.test(e)

# comparar la salida del summary del modelo 3 con la del modelo 1

summary(modelo3)
summary(modelo1)


# comparacion de DBO media del agua de las 3 fuentes para un valor de pH promedio
options(emmeans= list(emmeans = list(infer = c(TRUE, F)),contrast = list(infer = c(TRUE, TRUE))))

Fuentes_en_PH_PH_c <- emmeans(modelo3, pairwise ~ fuente|pH_c)
Fuentes_en_PH_PH_c

# Comparacion de pendientes
comp_pendientes <- emtrends(modelo3, pairwise ~ fuente, var="pH_c")
comp_pendientes



######  ValidaciOn modelo 1 ###################

# Predichos vs observados
p<-ggplot(Datos, aes(x =DBO , y = predict(modelo1), colour =fuente)) + geom_point(size=4)
p + geom_abline(intercept = 0, slope =1) +  ggtitle("Predichos vs observados")

# correlacion entre predichos y observados
cor <- cor(predict(modelo1), Datos$DBO)
cor
cor^2


#predicciones

library(ggeffects)
(a<-ggpredict(modelo1, 
              terms = c("pH", "fuente"),
              interval = "confidence")  ) 

p<-plot(a, add.data = TRUE)

p + ggtitle("Relación predicha entre la DBO y el pH") +
  labs(x="pH") + labs(y="DBO (mg/l) ")


####  prediccion de Y para nuevos valores de x  ######
nuevo = data.frame(fuente= "A", pH=7)
nuevo # exploren que es el objeto "nuevo"
predict(modelo1, nuevo, interval="prediction") 
#observar que el intervalo de prediccion para una observacion individual es mas amplio que la banda de confianza del modelo para la media poblacional 



