# Librerias:
library(dplyr) # Para crear tablas resumen
library(ggplot2)
library(gridExtra) # Para poner mas de un grafico de ggplot 
library(car)
library(nlme) # incluye funcion gls().


# Limpio environment y traigo los datos
rm(list=ls())
setwd("C:/Users/snpre/Documents/Carrera/Biometria II/tp3")
data<-read.csv("dataSim.csv", header = TRUE)

# Inspeccion de la BD
names(data)
head(data)
str(data)
summary(data)

# Paso las variables especie y ubicacion a factor
data$especie<-as.factor(data$especie)
data$ubicacion<-as.factor(data$ubicacion)
summary(data)

sum(data$especie == "Sp1" & data$ubicacion == "Norte")
sum(data$especie == "Sp1" & data$ubicacion == "Sur")
sum(data$especie == "Sp2" & data$ubicacion == "Norte")
sum(data$especie == "Sp2" & data$ubicacion == "Sur")
# Vemos que hay una combinacion balanceada de los "tratamientos" ubicacion y especie. Hay 20 de cada, hay 20 replicas :)


# Analisis exploratorio - Estadistica descriptiva
resumen_spp <- data[,-3] %>% 
  group_by(especie) %>%
  summarise_all(.funs = c(
    media = mean, 
    DE = sd, 
    min = min, 
    max = max
  )
  )
resumen_ubi <- data[,-2] %>% 
  group_by(ubicacion) %>%
  summarise_all(.funs = c(
    media = mean, 
    DE = sd, 
    min = min, 
    max = max
  )
  )

resumen_spp # no parece diferir tanto en un principio
resumen_ubi # difiere bastante la media, el desvio, el minimo y el maximo segun si la ubicacion es norte o sur

# todo en un solo grafico:
ggplot(data=data, aes(x=crecimiento, y=ubicacion))+geom_boxplot(aes(color=especie))+ geom_jitter(alpha=0.3, aes(color=especie))

#dos graficos: primero especie
graf_1<-ggplot(data=data, aes(x=crecimiento, y=especie))+geom_boxplot(aes(color=especie))+ geom_jitter(alpha=0.3, aes(color=especie))

# ahora ubicacion
graf_2<-ggplot(data=data, aes(x=crecimiento, y=ubicacion)) + geom_boxplot(color="black", outlier.colour = "black")+ geom_jitter(alpha=0.3, aes(color=ubicacion))

grid.arrange(graf_1, graf_2,ncol=2, nrow=1)

# Si miramos solo cualitativamente el boxplot de ubicacion vs crecimiento, podemos sospechar que la homocedasticidad no se cumplira: el crecimiento del norte, que es mayor, tiene mucha mas varianza que el del sur, que es menor

# Evalucion de supuestos
# Para eso, primero hago el modelo

modelo0<-lm(crecimiento ~ especie*ubicacion, data = data) # Generar modelo.

e <- residuals(modelo0) # Residuos.
re <- rstandard(modelo0) # Residuos estandarizados.
pre <- predict(modelo0) # Predichos.

# Grafico de dispersion residuos v. predichos y QQplot.
par(mfrow = c(1, 2))
# Grafico de dispersion.
plot(x = pre,
     y = re,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED" )

abline(h = 0, lty = 2) # Agregar linea horizontal y = 0.

# QQplot.

# libreria car:
qqPlot(e, main = "QQ Plot residuos")

#par(mfrow = c(1, 1)) # Restaura a un grafico por pantalla.


# Podemos observar un cono en los residuos vs predichos. Es decir, no se cumple la homocedasticidad, a mayor predicho mayor residuo. Ademas, se observan varios outliers que no se veian en el boxplot y no se cumple la normalidad

## Metodos analiticos.

# Prueba analitica para la homogeneidad de varianzas

# duda, se puede hacer un levene test?
leveneTest(crecimiento ~ ubicacion * especie, data = data)
leveneTest(data$crecimiento, data$ubicacion)
# RECHAZO
leveneTest(data$crecimiento, data$especie) # no rechazo

# Cual es la Ho de la prueba de Levene?
# Prueba analitica para normalidad 

shapiro.test(e)
# RECHAZO. Hay evidencias de que no se cumple la homocedasticidad


# Hay que modelar varianza. Como tenemos VE cuali, podemos usar varIdent.

modelo_varIdent <- gls(crecimiento ~especie*ubicacion,
                       weights = varIdent(form = ~1 | especie*ubicacion),
                       data = data)

# Evaluar supuestos.

par(mfrow=c(2,2))

r2 <- residuals(modelo_varIdent, type="pearson")
pred2 <- fitted(modelo_varIdent)

plot(x = pred2,
     y = r2,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion 
     de RE vs PRED")

abline(h = 0, lty = 2)

# Graficar un boxplot de los residuos del modelo

boxplot(r2 ~ data$ubicacion,
        xlab = "Ubicacion",
        ylab = "Residuos estandarizados")
boxplot(r2 ~ data$especie,
        xlab = "especie",
        ylab = "Residuos estandarizados")


# Graficar un qqplot.

qqPlot(r2, main = "QQ Plot residuos estandarizados")

# Prueba de Levene.

leveneTest(r2, data$ubicacion, center = "median") # 0.81
shapiro.test(r2) #0.31
# Evaluar supuestos.

# AHORA QUE MODELE VARIANZA, SE CUMPLEN LOS SUPUESTOS!

anova(modelo_varIdent)
# No hay interaccion. Miro los efectos simples
# Hay efecto de la ubicaicon pero no de la especie sobre el crecimiento de las palmeras
summary(modelo_varIdent)


# cuetsionario
#m2 <- gls(crecimiento ~ ubicacion * especie,
weights = varIdent(form = ~ 1 | especie),
data=data)
summary(m2)
# ultima preg
m2  <- gls(crecimiento ~ ubicacion * especie, weights = varIdent(form = ~ 1 | ubicacion), data=data)
m3  <- gls(crecimiento ~ ubicacion * especie, weights = varIdent(form = ~ 1 | especie), data=data)
par(mfrow=c(1,2))

r3 <- residuals(m3, type="pearson")
pred3 <- fitted(m3)

plot(x = pred3,
     y = r3,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion 
     de RE vs PRED")

abline(h = 0, lty = 2)

# Graficar un qqplot.

qqPlot(r3, main = "QQ Plot residuos estandarizados")

# comparo m2 y modelovarident

AIC(m2, modelo_varIdent)

anova(modelo_varIdent)
summary(m2)
