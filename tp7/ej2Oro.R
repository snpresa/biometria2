rm(list=ls())
setwd("C:/Users/snpre/Documents/Carrera/Biometria II/tp7")

######### Paquetes
library(dplyr)             # manipulacion de data.frames (descriptiva)  
library(ggplot2)           # graficos 
library(gridExtra)         # para plotear multiples graficos
library(ResourceSelection) # para evaluar bondad de ajuste
library(car)               # para funcion Anova (sino drop1)
library(emmeans)           # comparaciones
library(ggeffects)         # predichos del modelo
library(DHARMa)            # analisis supuestos

####################
#####Problema 2#####
##depositos de oro##
####################

#Se ha propuesto que ciertas características geoquímica y geológica de la superficie y subsuperficie pueden ayudar a predecir la ocurrencia de depósitos metalíferos, en particular de oro. 

#Sahoo y Pandalai (1999) recolectaron datos geoquímicos (concentración Sb en rocas superficiales, en ppm) y estructurales (cercanía a una falla geológica) en distintos sitios de una zona rica en depósitos auríferos en el cinturón de esquistos de Hutti-Maski (India). 

#A partir de estos datos se pretende modelar la presencia/ausencia de depósitos auríferos en los sitios muestreados.

datos<-read.delim("oro.txt")
str(datos)
summary(datos)
datos$falla_geologica # Deberia ser cuali?

datos$falla_geologica<-as.factor(datos$falla_geologica)

#1. Indique el tipo de estudio, la variable respuesta (tipo y potencial distribución de probabilidades). Describa el tipo de variables explicativas. Analice la asociación entre variables.

# Es un estudio observacional.
# La variable repsuesta es cualitativa dicotomica hay deposito de oro/ no hay deposito de oro 
# Potencial distribucion: Bernoulli (pi_i)
# Variables explicativas: Concentracion de Sb, cuantitativa continua, fija
#                         cercania a falla geologica si/no, fija
# La concentracion de Sb esta anidada en la falla geologica


#2. Escriba el modelo lineal generalizado a utilizar en términos del problema (modelo completo), explicitando los tres componentes del GLM. Interprete el odds en el contexto del problema.

# Predictor lineal
# logit(E(Y_i)) = logit(pi_i)=log(pi_i/1-pi_1) = log(ODDS) = 
# = N_i = beta0 + beta1 * Sb_i +beta2 * fallaGeologicaSi

# Escala de ODDS
# Odds = pi_i /(1-pi_1) = e**(beta0 + beta1 * Sb_i +beta2 * fallaGeologicaSi)

# El odds es el cociente entre la probabilidad de encontrar oro sobre la probabilidad de no encontrarlo. Es la probabilidad de exito de encontrar oro

# Escala de probabilidades
# pi_i = e**(beta0 + beta1 * Sb_i +beta2 * fallaGeologicaSi)/(1+e**beta0 + beta1 * Sb_i +beta2 * fallaGeologicaSi)

# 3. Genere un modelo de regresión para la ocurrencia de depósitos auríferos a partir de las variables explicativas seleccionadas, realizando un procedimiento de selección “hacia atrás”.
# AL FINAL AGREGO LA INTERACCION PORQUE ME DIO DUDAS EL ENUNCIADO. A LOS MODELOS DE ARRIBA SE LES AGREGARIA beta3*(sb*fallageologicasi)_i

m1<-glmmTMB(deposito_oro~Sb*falla_geologica, data=datos, family=binomial)
drop1(m1, test="Chisq") # Si saco la interaccion no se modifica significativamente el ajuste
m2<-glmmTMB(deposito_oro~Sb+falla_geologica, data=datos, family=binomial)
drop1(m2, test = "Chisq") # Ambas cambia el modelo significativamente si las saco. El modelo final es con ellas

#a. Analice el modelo obtenido y los supuestos asociados.

# Analisis supuestos:

# Muestra aleatoria -> Por diseño
# Observaciones independientes -> Por diseño
# Linealidad, para VE continuas con respecto al predictor lineal. 
# ¿Evaluamos dispersión? -> No en Bernoulli 
sim <- simulateResiduals(fittedModel = m2)
plot(sim)
hist(sim)
testDispersion(sim)
# No hay evidencias de incumplimiento de ningun supuesto, esta todo bien
summary(m2)
# Tanto la falla geologica como la concentracion de Sb aumentan las probabilidades de encontrar depositos de oro porque tienen estimadores positivos y significativos (puedo mirar el summary porque es una variable cuanti y la otra tiene 2 niveles)

#b. ¿Cómo interpreta la magnitud del efecto para cada una de las variables explicativa? (ayuda: recuerde el concepto de odds ratio).

exp(confint(m2))
# El OR estimado para Sb es 2.04 lo que nos indica que por cada aumento unitario de concentracion de Sb el odds es en promedio 2.04 veces el anterior ajustando por la otra variable
# El OR estimado para falla geologica es de 181. El odds de encontrar oro estando cerca de una falla geologica es 181 veces el odds de encontrar oro no estando cerca de una falla geologica.

#c. Escriba las ecuaciones estimadas en la escala del predictor lineal para el modelo obtenido. Grafique la probabilidad de ocurrencia de depósitos de oro en función de las variables explicativas.

# N_i = 4.21 + 2.04 * Sb_i + 181 * falla geologica_i

#### ********************************************
#### Visualizando las 3 escalas: logit, odd, prob
#### ********************************************


#predichos en escala del predictor lineal
datos$pl<-predict(m2, type="link") 

#predichos en escala de la VR 
datos$pre<-predict(m2, type="response") 

# Escala del predictor lineal
Graf_PL <- ggplot(datos, aes(Sb, pl, group=falla_geologica, colour=falla_geologica)) +
  labs(x="Sb", y="logit(p oro)") +
  geom_smooth(method = lm,  se = FALSE) +
  theme(axis.title=element_text(size=14)) + 
  theme(legend.position = "none")

# escala del odds
odd <- exp(datos$pl)
Graf_ODD <- ggplot(datos, aes(Sb, odd, group=falla_geologica, colour=falla_geologica)) +
  geom_point( size = 2) + labs(x="Sb", y="odds(oro)")+
  theme(axis.title=element_text(size=14)) + 
  geom_smooth(se = FALSE) +
  theme(legend.position = "none")

# escala de probabilidades
Graf_proba <- ggplot(datos, aes(Sb, pre, group=falla_geologica, colour=falla_geologica)) +
  geom_point( size = 2) + labs(x="Sb", y="p(Oro)")+
  theme(axis.title=element_text(size=14)) + 
  geom_smooth(method="glm", method.args=list(family=gaussian(link="logit")), se = FALSE) +
  theme(legend.position = c(0.8, 0.3))

library(gridExtra)
grid.arrange(Graf_PL, Graf_ODD, Graf_proba, ncol=3, nrow=1)


# d. Calcule a mano: ¿Cuál es la probabilidad de encontrar oro en un sitio sin falla y con una concentración de 6 ppm de Sb? ¿Y en uno de 8 y otro de 10 ppm de Sb? ¿Es constante el cambio en la probabilidad?

fixef(m2)$cond[1]
# Los siguientes no me funcionaron
coef(m2)[[1]]
coef(m_aditivo)[[2]]
coef(m_aditivo)[[3]]

# Para un sitio sin falla y Sb de 6
num <- exp(fixef(m2)$cond[1] + fixef(m2)$cond[2]*6+ fixef(m2)$cond[3]*0)  
num
# denominador
den <- 1 + num

num/den # 0.233

# Para un sitio con 8
num2 <- exp(fixef(m2)$cond[1] + fixef(m2)$cond[2]*8+ fixef(m2)$cond[3]*0)  
num2
# denominador
den2 <- 1 + num2
num2/den2 # 0.558

# Para un sitio con 10
num3 <- exp(fixef(m2)$cond[1] + fixef(m2)$cond[2]*10+ fixef(m2)$cond[3]*0)  
num3
# denominador
den3 <- 1 + num3
num3/den3 # 0.841

#4. Concluya en términos del problema e Informe sus resultados. 
# A mayor concentracion de Sb mayor probabilidades de cnontrar oro. Si hay una falla geologica cercana, es mas probable encontrar oro a concentraciones de Sb mas bajas. Ambos favorecen la probabilidad de cnontrar oro