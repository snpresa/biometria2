###########################
#* Biometria II - 2024   *#
#*      TP N 6           *#
#*   Modelos lineales    *#
#*    Generalizados      *#
########################### 

## Distribucion de Poisson
## Que significa la relacion esperanza-varianza?

##
ls()
rm(list=ls())
ls()

rm(list=ls())


############################################
## Problema 1.                             #
##  # colembolos del suelo en un bosque    #
############################################





#####################################
# Carga e inspeccion del data.frame #
#-----------------------------------#
#####################################

setwd("C:/Users/snpre/Documents/Carrera/Biometria II/tp6")

datos <- read.csv("colembolos2.csv", header = T, sep=";", dec=".", stringsAsFactors = T)
str(datos)

# 'data.frame':	120 obs. of  3 variables:
# $ Id_Muestra: int  1 2 3 4 5 6 7 8 9 10 ...
# $ Especie   : Factor w/ 3 levels "dombeyi","nervosa",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ Riqueza   : int  8 5 10 12 12 17 0 11 7 5 ...

# Exploramos 
summary(datos)
# ID va de 1 a 120
# Esta balanceado: hay 40 de cada especie
head(datos)


#############################
# Analisis exploratorio     #
#---------------------------#
#############################


library(ggplot2)
ggplot(datos, aes(Especie, Riqueza)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.1, alpha=0.3) 

# La media de obliqua es mayor que nervosa y mayor que dombeyi. Y pareceria que la varianza aumenta con la media (esto es bueno en Poisson!)

library(dplyr)

resumen <- datos %>% 
  group_by(Especie) %>%
  summarise(media = mean(Riqueza),
            sd = sd(Riqueza),
            min =min(Riqueza),
            var=var(Riqueza),# Agregue varianza para compararla con la media pues Poisson
            max =max(Riqueza))
resumen

# Se vuelve a ver pero ahora en tabla: mayor media mayor desvio, mayor varianza

################################
# Modelo                       #
#------------------------------#
################################

m1 <- glm(Riqueza ~ Especie, data=datos, family=poisson) # puede dar problemas de convergencia a veces
summary(m1)
#    Null deviance: 213.30  on 119  degrees of freedom
#Residual deviance: 128.41  on 117  degrees of freedom

# con glmmTMB
# install.packages("glmmTMB", dependencies = TRUE)
library(glmmTMB) # es buenisima
m1b <- glmmTMB(Riqueza ~ Especie, data=datos, family=poisson)
summary(m1b) 




#############
# Supuestos #
#-----------#
#############

# Dispersion
# Chequeamos parametros de dispersion
sum(resid(m1, type="pearson")^2)/m1$df.residual # cercano a 1, re bien
library(performance)
check_overdispersion(m1) # No overdispersion detected.
# h0: NO hay sobredispersion (hipotesis del no efecto)
# NO hay eveindecia para afirmar que hay dispersion mayor a 1. Aca asumimos que ambas no se cumplen, aunque solo chequeamos sobre tecnicamente

# continuará ... 

########################################
# Modelo: Resultados e interpretacion  #
#------------------------------------  #
########################################

# significancia 'global'
drop1(m1, test = "Chisq") # mayor devianza peor modelo (medida de falta de ajuste)
# Lo que aparece a la izq es lo que saca. Veo que es significativo sacar a especie.
# Sacar especie tiene mayor devianza! y mayor AIC
# vemos significancia del factor, no dice nada de comparacion de medias

# La devianza nula es la no explicada por el modelo, y la diferencia es significativa

m0<-glmmTMB(Riqueza ~ 1, data=datos, family=poisson)
library(car)
Anova(m1) # deberia dar exactamente lo mismo !
Anova(m0,m1) # NO lo pude correr, ver en casa

# coeficientes del modelo
summary(m1)# en que escala estan los coeficientes?
# logaritmica!

# Devianza explicada
((m1$null.deviance-m1$deviance)/m1$null.deviance)*100 # 39


##################
# Comparaciones  #
# Mag del efecto #
# -------------- #
##################

library(emmeans)
options(emmeans= list(emmeans = list(infer = c(TRUE, FALSE)),
                      contrast = list(infer = c(TRUE, TRUE)))) # opciones de seteo de las opciones de salida

# puedo tener el emmeans en la escala del pl (lo que ya veniamos haciendo) o de la variable respuesta

# Escala del predictor lineal - Logaritmica
compPL <- emmeans(m1, pairwise ~ Especie) # Tukey por default
compPL
# me da el logaritmo de la riqueza promedio de cada especie y el logaritmo de la diferencia

# Veo diferencias entre todos 
# Escala de la variable respuesta
compVR <- emmeans(m1, pairwise ~ Especie, type="response") # Tukey por default
compVR
# ahora si tenemos las medias de riqueza esperada, spp presentes en cuadrantes por eso dice rate, esta atado al esfuerzo de muestreo. LOS PVALORES SON IGUALES!! 


# ahora si tenemos comparaciones de medias

# En escala de variable respuesta los 3 efectos multiplicativos dan significativos


# Reconstruccion de las medias y Magnitud del efecto a partir del summary()

# calcular las medias estimadas en la escala de la variable respuesta, a partir del summary
# ayuda: 
exp(coef(m1)) # me da e**beta0,  e**beta1, e**beta2

#   (Intercept) Especienervosa Especieobliqua 
#5.675000       1.568282       2.057269 

# Valor esperado "dombeyi" 
5.675000

# Valor esperado "nervosa"
5.675000*1.568282 # 8.9

# Valor esperado "obliqua"
5.675000*2.057269 # 11.6754



###################################
# Valores predichos por el modelo #
# --------------                  #
###################################

library(ggeffects)
estim<-ggpredict(m1, terms = c("Especie"))
estim
plot(estim, add.data = F)
# lOS ic NO SON SIMETRICOS

# a) Identifique la unidad muestral, la variable respuesta (y su potencial distribución de probabilidades) y la/s predictora/s involucrada/s.

# Unidad muestral: cuadrantes de 20 cm donde se toma la muestra de hojarasca
# VR: colembolos en esos 20cm2, Y_i~ Poisson(lambda_i)
# VE: especie de arbol presente en el micrositio. CUALITATIVA, 3 niveles

# b) Plantee el modelo

# Escala del PL
# N_i = mu + alfa_i =beta_o +beta1_i * sp2_i + beta 2*sp3_i
# Siendo mu la media poblacional y alfa_i el efecto de la especie
# i va de i a 3 en comp de medias y i a 120 en reg lineal

# c) Describa los datos mediante gráfico/s y tabla/s
# Hecho arriba
# d) Ajuste el modelo en R y evalúe los supuestos
# Hecho arriba
# e) Compare la riqueza de colémbolos del suelo entre micrositios de las distintas especies de Nothofagus
# Se estima que la riqueza promedio en micrositios de dombeyi es 0.638 veces la riqueza de nervosa. Es decir, nervosa presenta un 37 % más de riqueza. Entre el 22.3 y 47.7% mas de riqueza con una confiaza del 95%
# A la vez, la riqueza promedio en micrositios de dombeyi es 0.486 la riqueza de obliqua y nervosa tiene 0.762 la riqueza de obliqua.

# f) El estudio, ¿aporta evidencia a favor de la hipótesis de investigación bajo estudio?

# Síiii. Es re distinta la riqueza!

############################################
## Problema 2.                             #
## Reservas Urbanas y conservacion de aves #
############################################



#####################################
# Carga e inspeccion del data.frame #
#-----------------------------------#
#####################################

aves <- read.table("aves.txt", header = T, stringsAsFactors = T)

# Exploramos 
str(aves)
#'data.frame':	35 obs. of  3 variables:
#$ Ambiente : Factor w/ 2 levels "Metropol","Reserva": 1 1 1 1 1 1 1 1 1 1 ...
#$ Aves     : int  4 0 1 4 1 5 1 2 2 3 ...
#$ DurObserv: int  4 2 2 3 5 5 5 4 4 5 ...
summary(aves)
table(aves$Ambiente, useNA = "always")

#Damos vuelta los niveles (porque nos será conveniente para interpretar luego)
levels(aves$Ambiente)

aves$Ambiente <- factor(aves$Ambiente, levels = c("Reserva", "Metropol"))

levels(aves$Ambiente)


# calculamos la tasa de avistajes por minuto a partir del N de aves (para descriptiva)
aves$tasa_Aves <- aves$Aves / aves$DurObserv

# chequeamos data.frame
summary(aves) # tasa de aves va de 0 a 3
str(aves)
head(aves)


#############################
# Analisis exploratorio     #
#---------------------------#
#############################
# Poisson requiere que sea entero el numero

# hicimos tasa de aves para graficar esto porque sino no tenia sentido. ARtilugio, sino no eran interpretables los valroes

library(ggplot2)
ggplot(aves, aes(Ambiente, tasa_Aves)) + 
            geom_boxplot() + 
            geom_jitter(width = 0.1, alpha=0.3, colour="blue") 
            
# la reserva tiene un promedio mayor de aves vistas en un minuto que la metropol


################################
# Modelo                       #
#------------------------------#
################################

# Pasamos la variable offset a logaritmo 
aves$Log_DurObserv <- log(aves$DurObserv)


m1a <- glm(Aves~ Ambiente + offset(Log_DurObserv), data=aves, family=poisson)
summary(m1a)
#    Null deviance: 105.843  on 34  degrees of freedom
#Residual deviance:  42.655  on 33  degrees of freedom
# con glmmTMB
# install.packages("glmmTMB", dependencies = TRUE)
library(glmmTMB)
m1b <- glmmTMB(Aves~ Ambiente + offset(Log_DurObserv), data=aves, family=poisson)
summary(m1b) 

#############
# Supuestos #
#-----------#
#############

# Dispersion
sum(resid(m1a, type="pearson")^2)/m1a$df.residual # 1.19 ojotaaa

library(performance)
check_overdispersion(m1a) # NO overdispersion detected todo OK

# Supuestos ####
# install.packages("DHARMa", dependencies = TRUE)
library(DHARMa)

# ver diapos 48, 49 y 50 de la clase 13-14

# Que hace DHARMA
# Para cada observación Yi:
# 1. Suponiendo que el modelo ajustado es correcto, simular datos (i.e. 1000)
# 2. Construir la distribución de probabilidades empírica a partir de los datos simulados
# 3. Calcular el residuo como la probabilidad acumulada en la distribución empírica (rango 0-1)

# Si el modelo especificado es correcto, se espera que:
# los residuales tendrán una distribución uniforme
# No se esperan patrones en el gráfico de residuos escalados vs predichos

sim <- simulateResiduals(m1a, n = 1000)
plot(sim) 

# Puede no hacer el permormance o hacer todo de maniaco
# Primero QQplot, prueba de ajuste a distribucin uniforme
# segundo: regresion de cuantiles. Espero que la linea negra este cerca de la punteada. Si se aparte DHARMA lo pinta de rojo

########################################
# Modelo: Resultados e interpretacion  #
#------------------------------------  #
########################################

# significancia 'global'
drop1(m1a, test = "Chisq") 

# Test de devianz o coeicnte de verosimilitudes
# Hay un cambio significativo en la devianza cuando saco ambiente!!

# coeficientes del modelo
summary(m1a)# en que escala estan los coeficientes?
# Logaritmica, lo devuelve para un minuto de observaicon por haber incluido un offset

# Devianza explicada
((m1a$null.deviance-m1a$deviance)/m1a$null.deviance)*100 #60%


##################
# Comparaciones  #
# Mag del efecto #
# -------------- #
##################

library(emmeans)

options(emmeans= list(emmeans = list(infer = c(TRUE, FALSE)),
                      contrast = list(infer = c(TRUE, TRUE)))) # opciones de seteo de las opciones de salida


# Escala del predictor lineal
# si agrego offset =0  la tasa es de un minuto. Unidades del logaritmo. No se informa, si sirve para significancia
compPL <- emmeans(m1a, pairwise ~ Ambiente, offset = 0) # Tukey por default
compPL

# Escala de la variable respuesta
#  lo mismo, es por minuto. Aves obs por minuto, sin logaritmo
compVR <- emmeans(m1a, pairwise ~ Ambiente, type="response", offset = 0) # Tukey por default
compVR

# En la RECS se espera registrar en una sesion de observacion entre 1.49 y 3.7 veces mas de ves nativas qe en el area metropolitana con una confianza del 95%

# Por que incluimos offset=0? 
# Al indicar offset = 0 para un modelo de regresion de Poisson indicamos que las predicciones de la grilla de referencia (ref_grid, ver mas abajo) se conviertan en tasas relativas al offset que fueron especificadas en el modelo.

# pero por default emmeans no pone offset = 0
# Si no seteamos offset ... 
compVRSin_of <- emmeans(m1a, pairwise ~ Ambiente, type="response") # 
compVRSin_of
# Las medias estimadas dan distintos pero el RATIO NO!!!!!
# La media a tiempo promedio de observacion es de ... para ... ambiente

# Por defecto lo informa en un tiempo promedio!!!

#Observar que la diferencia entre incluir o no incluir offset=0 es:
ref_grid(m1a) # esto devuelve con lo que trabajo

# Toma el valor de Log_DurObserv = 1.4458
mean(aves$Log_DurObserv) # Aca vemos que es el promedio!!

###################################
# Valores predichos por el modelo #
# --------------                  #
###################################

library(ggeffects)
estim<-ggpredict(m1a, terms = c("Ambiente"))
estim
plot(estim, add.data = F)

### FIN ###

# Avistaje promedio de aves en dos ambientes estimados a partir de un modelo GLM con distribucion Poissson. EN la RECS se espera registrar en una sesion de obseracion a tiempo constante entre 1.49 y 3.7 veces mas de aves nativas que en area metropolitana con 95% de confianza
#O
# En la reserva se espera en promedio un aumento en la cantidad  de aves entre 149 y 370% con 95% de confianza

# Se podria hacer el calculo de cuantas aves mas veo en una hora

compVR60 <- emmeans(m1a, pairwise ~ Ambiente, type="response", offset = log(60)) # Tukey por default
compVR60


#a) Identifique la unidad muestral, la variable respuesta (y su potencial distribución de probabilidades) y la/s predictora/s involucrada/s.

# UM: cada punto de muestreo donde se observaron aves
# VR: aves nativas observadas por tiempo de muestreo, Y_i~Poisson(lambda_i)
# VE: Ambiente, cualitativa, 2 niveles

#b) Plantee el modelo

# N_i = beta0 + beta1 * Metropol_i = mu + alfa_i
# En la regresion i va de 1 a 35, en la comparacion de i a 2
# funcion de enlace log

# log (E(Y_i))= N_i
#c) Describa los datos mediante gráfico/s y tabla/s

#d) Ajuste el modelo en R y evalúe los supuestos
# Hecho arriba
#e) En qué medida las reservas favorecen la conservación de aves nativas?. Informe la magnitud del efecto en escala de la variable respuesta.
compVR
# Avistaje promedio de aves en dos ambientes estimados a partir de un modelo GLM con distribucion Poissson. EN la RECS se espera registrar en una sesion de obseracion a tiempo constante entre 1.49 y 3.7 veces mas de aves nativas que en area metropolitana con 95% de confianza
#O
# En la reserva se espera en promedio un aumento en la cantidad  de aves entre 149 y 370% con 95% de confianza
#---> En la RECS se espera registrar en una sesion de observacion entre 1.49 y 3.7 veces mas de ves nativas qe en el area metropolitana con una confianza del 95%
