# *** Biometria II 2024 *** #

# Este es el script asociado al problema  
# Controles de la depredacion de plantulas de B. Yatay en el PN El Palmar
# del TP # 7 Regresion Logistica  ##


### Antes que nada veamos si hay algun comando u objeto almacenado en la memoria
ls()
# Si te aparece character(0) es que la memoria esta limpia, si no ejecuta
# la siguiente sentencia para borrar la memoria.
rm(list=ls())
# revisamos...
ls() # No deberia haber nada en el entorno de trabajo ahora

### Setear directorio de trabajo
setwd("C:/Users/snpre/Downloads")

# librerias necesarias:
library(dplyr)             # manipulacion de data.frames (descriptiva)  
library(ggplot2)           # graficos 
library(gridExtra)         # para plotear multiples graficos
install.packages("ResourceSelection")
library(ResourceSelection) # para evaluar bondad de ajuste
library(car)               # para funcion Anova (sino drop1)
library(emmeans)           # comparaciones
library(ggeffects)         # predichos del modelo
library(DHARMa)            # analisis supuestos



#### *********************
#### carga del data.frame
#### *********************


datospalmar <- read.delim("DatosPalmar1.txt", stringsAsFactors = T)



#### **************************
#### Exploracion del data.frame
#### **************************

# Exploracion del data.frame: estructura, clases, niveles, datos faltantes, etc.   
str(datospalmar)
head(datospalmar)
tail(datospalmar)
dim(datospalmar)
View(datospalmar)# para visualizar en otra pestania
summary(datospalmar)

#### **************************
####  Indique -->  
#### el tipo de estudio:
#### la variable respuesta (tipo y potencial distribución de probabilidades):
#### Identifique y clasifique las variables explicativas (fija/aleat; cruzada/anidada):


#### ¿Hay variables de efectos aleatorios?


#### ***********
#### Modelo
#### ***********

# Componente aleatorio:

# Predictor Lineal:

# Funcion de enlace:

##### Modelo en escala del Predictor Lineal =

##### Modelo en escala de ODDs =

##### Modelo en escala de la Variable respuesta =


### ¿cual es el dominio de la VR en cada una de estas escalas?


# Explore los resultados del ensayo con tablas y gráficos, ¿cuál fue la proporción de
# plántulas que sobrevivieron entre las que estaban protegidas por jaulas y cuál la
# proporción entre las que no?

#### ***********
#### Desriptiva
#### ***********

# supervivencia segun condicion de exclusion
datospalmar %>%
  select(exclusion, supervivencia) %>%
  group_by(exclusion) %>%
  summarise_all(.funs = c(
    n = length, 
    media = mean, 
    sd = sd, 
    min = min, 
    max = max
  )
  )

# distancia segun condicion de exclusion
datospalmar %>%
  select(exclusion, distancia) %>%
  group_by(exclusion) %>%
  summarise_all(.funs = c(
    n = length, 
    media = mean, 
    sd = sd, 
    min = min, 
    max = max
  )
  )

# De que tipo es la variable respuesta?
# Que valores puede tomar? 
# Como es potencial distribucion de probabilidades? 
class(datospalmar$supervivencia)
summary(datospalmar$supervivencia)
datospalmar$supervivencia


#### **************************
#### Graficos descriptivos
#### **************************
  
  # supervivencia vs tratat exclusion:
  
  supVsExc <- ggplot(datospalmar, aes(x = exclusion, y = supervivencia)) +
  geom_jitter(height = 0.01, width = 0.1) +
  theme(axis.text.x=element_text(size=15))
supVsExc

#  supervivencia vs distancia:
supVsDist <-  ggplot(datospalmar, aes(x=distancia, y= supervivencia)) +
  geom_jitter(height = 0.01, width = 0.1) +
  xlab("dist a la palmera adulta (cm)") + 
  ylab("") +
  theme(axis.text.x=element_text(size=15))
supVsDist

# supervivencia vs distancia, segun la condicion de exclusion:

supVsDistExc <- ggplot(datospalmar, aes(x=distancia, y= supervivencia,colour = exclusion)) +
  geom_jitter(height = 0.01, width = 0.1) +
  # geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)  +  # para agregar un suavizado glm binomial
  xlab("dist a la palmera adulta (cm)") + 
  ylab("") +
  theme(axis.text.x=element_text(size=15))
supVsDistExc

library(gridExtra)
grid.arrange(supVsExc, supVsDist, supVsDistExc, ncol=3, nrow=1)

#### **************************
#### Descriptiva: Odd, pb ...
#### **************************

# Cuantas plantulas sobrevivieron del total? 
table(datospalmar$supervivencia, useNA = "always")

# Cuantas plantulas protegidas de los jabalies (jaula) sobrevivieron y,cuantas de las que no tenian  proteccion? 
table(datospalmar$exclusion, datospalmar$supervivencia)

# ODDS #
# Recordemos: ODDS es el cociente entre la proporcion de los eventos a favor 
# y la proporción de los eventos en contra.

# Cual fue el odds de supervivencia entre las plantulas protegidas de los jabalies? 

# Estimacion de la pb sobrevivir dado que estan protegidas
35/(15+35) 
# Estimacion de la pb de NO sobrevivir dado que estan protegidas
15/(15+35)


# Estimacion de ODD de sobrevivir 

#### con juala (protegidas)
odd_protegidas <-  0.7 / 0.3
odd_protegidas

#### Por cada 2.33 plantas vivas hay 1 planta muerta

#### Para hacer:
#### sin juala (NO protegidas) 
odd_NO_protegidas <-  
odd_NO_protegidas

#### Conclusion: 

# En alguna de las situaciones fue mas probable que la planta viva a que muera?


#### *****************************************************************
#### Ajuste del modelo para la supervivencia de plantulas de B. Yatay
#### *****************************************************************

# Paso el nivel SIN jaula a la intercept (ayudara a la interpretacion de los resultados)
levels(datospalmar$exclusion)
datospalmar$exclusion <- factor(datospalmar$exclusion,
                            levels = c("sinjaula", "jaula"))
levels(datospalmar$exclusion)


## REGRESION LOGISTICA

# con glm
m_con_int <- glm(supervivencia ~  exclusion*distancia,
                    data =datospalmar, family = binomial)

# otra opcion: glmmTMB 
library(glmmTMB)
m1 <-  glmmTMB(supervivencia ~  exclusion*distancia,
                data =datospalmar, family = binomial)


# Analisis supuestos:

# Muestra aleatoria -> Por diseño
# Observaciones independientes -> Por diseño
# Linealidad, para VE continuas con respecto al predictor lineal. 
# ¿Evaluamos dispersión? -> No en Bernoulli 


library(DHARMa)
sim <- simulateResiduals(fittedModel = m_con_int, plot = T)
plot(sim)

# Tachar lo que no corresponda y completar el p-valor:
# (HAY / No hay)   evidencias de mal ajuste al modelo, ya que (SE RECHAZA / No se rechaza) la distribución uniforme para los residuos escalados de Dharma (p= ______) . (Se detectan / No se detectan) outliers y (No se detecta / se detecta) incumplimiento “significativo” del supuesto de linealidad. 



# Que puede concluir acerca de la interaccion distancia * condicion de exclusion
# Concluya
library(car)
Anova(m_con_int)
# alternativamente drop1
drop1(m_con_int, test="Chisq") 
# vemos el summary
summary(m_con_int)

#### Completar:
# El efecto del tratamiento exclusión sobre la probabilidad de supervivencia (Depende / NO depende) de la distancia a la palmera adulta (p= ____)


# Modelo sin interaccion:
m_aditivo <- glm(supervivencia ~  exclusion + distancia,
                           data =datospalmar, family = binomial)

drop1(m_aditivo , test="Chisq") 

# Analisis supuestos:
sim <- simulateResiduals(fittedModel = m_aditivo, plot = T)
# por variable
plotResiduals(sim, datospalmar$exclusion) #### notar que devuelve un grafico de cajas
plotResiduals(sim, datospalmar$distancia)

#### DHARma-Dudas: https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html


#### ************************************************
#### Modelo ajustado (aditivo) - salidas coef., comp
#### ************************************************


## Coeficientes escala del PL (ln(odds))

# del summary
summary(m_aditivo)

# fc coef 
coef(m_aditivo)
confint(m_aditivo)

#### Escribir la ecuacion estimada del modelo en la escala del PL (ln(ODDS)) e interpretar los coeficientes del modelo:

#### Ecuacion estimada: 

#### Inetrpretacion (completar):
# Por cada aumento en 1 cm en la distancia a la palmera adulta el ln(odds) de que una plántula de B. yatay sobreviva aumenta en promedio ___________, independientemente del tratamiento exclusión (de la depredación).

# Con la presencia de jaula (exclusión de jabalíes) el ln(odds) de que una plántula sobreviva aumenta en promedio __________, manteniendo la distancia a la palmera adulta constante.

# ¿Como son estos coeficientes en relacion al cero? (mayores, menores, etc)

#### Escribir las ecuaciones estimadas (escala de PL) que relacionen el logit de la sup con la distancia, para el tratamiento SIN jaula y CON jaula



###### Escala de odds
### Analizar los coeficientes estimados en la escala de ODDs y concluir

# escala de ODDS
exp(coef(m_aditivo))
exp(confint(m_aditivo))

# Interpretacion y ejercicios: 

###### sobre "(Intercept)":
# En ausencia de jaula, el odds de sobrevivir promedio estimado es _______ a distancia 0 (Sería extrapolar. Para ganar interpretabilidad centrar la distancia)

# El valor de intercept ES el odds de la condición (con jaula / sin jaula) para una distancia de 0 cm a la palmera adulta

###### sobre "exclusionjaula": 
# Si se coloca la jaula (“exclusión de jabalíes”): La plántula tiene un odds de sobrevivir ________ veces mayor que si no tiene jaula, manteniendo constante la distancia

# El valor "exclusiónjaula" ES el OR  de la condición jaula  

#### Tachar lo que no corresponda:
### Como OR (> / < / = ) a 1, la presencia de jaula está asociada (positivamente / negativamente) con la supervivencia

###### sobre "distancia":
# Por cada cm que aumenta la distancia a la plántula adulta el odds de que una plántula viva aumenta ________ %

#### Tachar lo que no corresponda:
### Como OR (> / < / = ) a 1, la distancia a la palmera adulta está asociada (positivamente / negativamente) con la supervivencia


#### Comparaciones 
library(emmeans)
options(emmeans= list(emmeans = list(infer = c(TRUE, FALSE)),contrast = list(infer = c(TRUE, TRUE))))

#en escala del PL
comp <- emmeans(m_aditivo, pairwise ~ exclusion, adjust = "tukey") #Tukey por default  
summary(comp)
plot(comp, comparisons = TRUE)

#en escala de la VR
compVR <- emmeans(m_aditivo, pairwise ~ exclusion, type = "response") #Tukey por default  
summary(compVR)
plot(compVR, comparisons = TRUE)

#### Comparar en ambas comparaciones, cuando el IC contiene al 0, al 1 y relacionarlo con el p-valor obtenido

# explorar el objeto ref_grid (para saber que valor de distancia usa)
ref_grid(m_aditivo)

# distancia que toma
mean(datospalmar$distancia)

#### Comentario: Las medias y OR están calculados a “distancia” promedio. 
# Al no haber interacción (Trat*distancia), el OR vale para cualquier distancia. Pero la Pb de sobrevivir estimada cambia según la distancia (intuición gráfica al final…) 



#### Algunas conclusiones y resultados

# Bajo el supuesto de que la presencia de jaula impide el acceso de los jabalíes a las plántulas, se concluye que la presencia de los jabalíes tiene un impacto negativo sobre la supervivencia de las plántulas de B.yatay (p = ________) 
# 
# Se estima que el odds de que una plántula sobreviva (aumenta/disminuye) en promedio entre  ______ %  y ______ %  en presencia de jabalíes, comparado con la exclusión, controlando por la distancia a la palmera más cercana.


#### Cuentas? #### Salen a partir del los imites del IC para diferencia de medias en escala de la VR (sinjaula / jaula)
# p / L inf
(0.138-1)*100
# p / L sup
(0.771-1)*100

#### Rpta: 22.9 %  y 86.2% 



# Por otra parte, tanto frente a la exclusión o no de jabalíes, se observa que la supervivencia de las plántulas (disminuye/aumenta) con la distancia a la palmera más cercana, sugiriendo un posible mecanismo de competencia intraespecífica. Se estima que por cada cm que aumenta la distancia a la palmera más cercana la supervivencia aumenta en promedio entre ……   y ……  , independientemente de la presencia o no de jabalíes.

#### Cuentas? ####  salen de exp(confint(m_aditivo))
# p / L inf
(1.0007229-1)*100
# p / L sup
(1.0076344-1)*100

#### Rpta: 0.07 %  y 0.76% 


##########################################################
#### modelo final resumen (grafico con valores predichos)
##########################################################

library(ggeffects)
predichos <-ggpredict(m_aditivo, 
                      terms = ~distancia+exclusion,
                      interval = "confidence")   
predichos

plot(ggpredict(m_aditivo, terms = ~distancia+exclusion))+
  labs(y="Supervivencia de la plantula de B. yatay") +
  labs(x="Distancia a la palmera adulta mas cercana (cm)")


#### ********************************************
#### Visualizando las 3 escalas: logit, odd, prob
#### ********************************************

  
#predichos en escala del predictor lineal
datospalmar$pl<-predict(m_aditivo, type="link") 

#predichos en escala de la VR 
datospalmar$pre<-predict(m_aditivo, type="response") 

# Escala del predictor lineal
Graf_PL <- ggplot(datospalmar, aes(distancia, pl, group=exclusion, colour=exclusion)) +
  labs(x="distancia (cm)", y="logit(p supervivencia)") +
  geom_smooth(method = lm,  se = FALSE) +
  theme(axis.title=element_text(size=14)) + 
    theme(legend.position = "none")

# escala del odds
odd <- exp(datospalmar$pl)
Graf_ODD <- ggplot(datospalmar, aes(distancia, odd, group=exclusion, colour=exclusion)) +
  geom_point( size = 2) + labs(x="distancia (cm)", y="odds(supervivencia)")+
  theme(axis.title=element_text(size=14)) + 
  geom_smooth(se = FALSE) +
  theme(legend.position = "none")

# escala de probabilidades
Graf_proba <- ggplot(datospalmar, aes(distancia, pre, group=exclusion, colour=exclusion)) +
  geom_point( size = 2) + labs(x="distancia (cm)", y="p(supervivencia)")+
  theme(axis.title=element_text(size=14)) + 
  geom_smooth(method="glm", method.args=list(family=gaussian(link="logit")), se = FALSE) +
  theme(legend.position = c(0.8, 0.3))

library(gridExtra)
grid.arrange(Graf_PL, Graf_ODD, Graf_proba, ncol=3, nrow=1)



# ***************
# predicciones
# p(jaula;200cm) 
# ***************
### ¿Puede predecir la probabilidad de supervivencia de una plántula sin jaula
# que está a 200 cm de una palmera adulta?

# numerador
# con estos comandos extraigo los 3 coeficientes
# para no tener que redondearlos
coef(m_aditivo)[[1]]
coef(m_aditivo)[[2]]
coef(m_aditivo)[[3]]

num <- exp(coef(m_aditivo)[[1]] + coef(m_aditivo)[[2]] + coef(m_aditivo)[[3]]*200)  

# denominador
den <- 1 + exp(coef(m_aditivo)[[1]] + coef(m_aditivo)[[2]] + coef(m_aditivo)[[3]]*200) 

num/den # 0.6828475

#### señalar dicho valor en el grafico que corresponda (de los 3 anteriores logit, ODDs, proba)




#### *****************************************************
#### EXTRA
#### *****************************************************

#### *****************************************************
#### Validacion modelo (buen ajuste, capacidad predictiva)
#### *****************************************************

# Hoslmen test buen ajuste regresion logistica
library(ResourceSelection)
hl <- hoslem.test(datospalmar$supervivencia, fitted(m_aditivo), g=10)
hl

# para ver graficamente lo que está haciendo el test. 
pred <- m_aditivo$fitted.values # predicted values (100 valores predichos)
var <-as.numeric(datospalmar$supervivencia) # var= variable respuesta
N <-10 # number of intervalos  (g=10)
secu <- c(-0.00001,(1:N)/N) # puntos de corte
grupos <- cut(pred,secu) # agrupamos los pred en intervalos
valores<-unlist(lapply(split(var,grupos),mean)) # las medias de cada intervalo
cexs<-unlist(lapply(split(var,grupos),sum)) 
plot(secu[-1]-1/N/2,valores,pch=16,xlim=c(0,1),ylim=c(0,1),xlab='Pb predicha',ylab='Frec observada')
abline(0,1)

######################################
#### Evaluando capacidad predictiva
######################################
#curva ROC 
library(Epi)
curvaROc <- ROC(form=supervivencia~exclusion+distancia, data=datospalmar)
curvaROc$AUC



