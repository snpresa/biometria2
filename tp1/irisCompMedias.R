#  TP N° 1 - # Problema 6. Características morfológicas de plantas del género Iris

# Modelo lineal (comparacion de medias)
# PARTE B, prgeuntas 4 a 6

# Antes que nada veamos si hay algun comando u objeto almacenado en la memoria
rm(list=ls())


# Librerias que vamos a utilizar en este TP
library(car)
library(dplyr)
library(ggplot2)

# Setear
setwd("C:/Users/snpre/Documents/Carrera/Biometria II/tp1 ") 


########### El dataset ############

datos <- iris # el dataset iris se encuentra disponible en R
head(datos)
# por comodidad modificamos los nombres de las columnas al castellano:

colnames(datos) <- c("LongSepalo", "AnchoSepalo", "LongPetalo", "AnchoPetalo", "especie")


# Exploramos caracteristicas / estructura del data.frame

head(datos)
str(datos)
names(datos)
nrow(datos)
ncol(datos)
dim(datos)

class(datos$especie) 
class(datos$LongSepalo)

View(datos)

# Estadistica descriptiva / medidas resumen 

summary(datos) 

datos %>% 
  group_by(especie) %>%
    summarise_all(.funs = c(
    media = mean
    )
  )

# datos resumen
resumen <- datos %>% 
  group_by(especie) %>%
  summarise_all(.funs = c(
    media = mean, 
    DE = sd
  )
  )


resumen
# tabla de frecuencias INCLUYE NAs

tabla_frecuencias <- table(datos$especie, useNA = "always")
tabla_frecuencias

# frecuencias relativas
prop.table(tabla_frecuencias)


# Mas adelante exploraremos "prop.table" para tablas con dos variables 

# Para graficos exploratorios ver archivo *irisGraficos.R*


## Modelo lineal para comparacion de medias ("ANOVA DE 1 FACTOR")
#  4. ¿Hay evidencia para decir que las especies difieren en relación a la longitud del sépalo? Difieren las especies en relacion a la longitud de sepalo?

# Escribimos la sintaxis para un modelo lineal 

m1 <- lm(LongSepalo~especie, data=datos)  

## Con la funcion lm (linear model) se ajusta un modelo lineal, 
# en este caso, la longitud del sepalo variable respuesta/dependiente vs 
# la variable predictora/explicatoria/independiente especie -que es una variable categorica con 3 niveles-)


# para repasar:
# cuales son los supuestos de este modelo?
# Homocedasticidad y Normalidad

# Descomp de la SC ("tabla de ANOVA") del modelo
anova(m1)


# antes de interpretar: supuestos
# Supuestos
e<-residuals(m1) # residuos
re<-rstandard(m1) # residuos estandarizados
pre<-predict(m1) # predichos
#Gráficos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Graf de dispersion de RE vs PRED" )
abline(0,0, col="red")
qqPlot(e, main = "QQ Plot residuos")
par(mfrow = c(1, 1))
#Pruebas analíticas de los supuestos
shapiro.test(e) # pv = 0.22
# Homogeneidad de varianza. Prueba de Levene
leveneTest(datos$LongSepalo, datos$especie) 
# Rechazo!!! wtf

anova(m1)
# Hay evidencias para decir que las especies difieren en la longitud del sepalo. Pero no sabemos cual con cual, por eso hacemos comparaciones

# comparaciones
# como analizarian la magnitud del efecto?
library(emmeans)

# seteo de opciones de salida (opcional, modificando los argumentos T / F puede ver como se modifican las salidas del emmeans)
options(emmeans= list(emmeans = list(infer = c(TRUE, FALSE)),contrast = list(infer = c(TRUE, TRUE))))

# comparaciones
comp <- emmeans(m1, pairwise ~ especie)
comp
# Difieren TODAS CON TODAS!!!

plot(comp, comparisons = TRUE)
# La especie con mayor longitud de sepalo promedio es Virginica, es en promedio 1.582 (unidades) mayor a setosa y 0.652 (u.) mayor que versicolor

# Grafico (una de muchas opciones gr?ficas!)

## una opcion es extraer los valores predichos por el modelo del emmeans y luego construir el grafico

resumen_modelo <-as.data.frame(comp$emmeans)

# exploramos el objeto resumen_modelo
resumen_modelo  # emmeans es la media estimada

# Plot
library(ggplot2)
ggplot(resumen_modelo, aes(x=especie, y=emmean)) +
  labs(x="especie") + labs(y="longitud sepalo") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), color="blue", width=0.2)+
  geom_point(shape=1, size=2, color="blue") +
  ylim(0,8)+
  ggtitle("Comparaci?n de long de sepalo entre especies", "Media ? Error est?ndar") +
  annotate("text", x = c(1,2,3), y=c(6,7,8), label = c("A", "B", "C"))



#6. Presentar un gráfico con los resultados.

## Otra opcion es graficar los valore spredichos con la libreria ggeffects
# Me gusta mas esta

library(ggeffects)
model_plot <-ggpredict(m1, 
                       terms = c("especie"),
                       interval = "confidence")   
model_plot
grafico <- plot(model_plot, add.data = F) # para agregar los puntos observados add.data = T

# AGREGUE LOS DATOS CON GEOM_JITTER

grafico + ggtitle("valores predichos") + labs(y="Long sepalo (mm))") +
  annotate("text", x = c(1,2,3), y=c(5.3,6.3,7), label = c("A", "B", "C")) + geom_jitter(data = datos, aes(x = as.numeric(especie), y = LongSepalo), width = 0.2, height = 0, color = "blue", size = 1.5, alpha = 0.6)


