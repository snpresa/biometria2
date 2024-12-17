

#Problema 3. Atropellamiento de anfibios en una carretera en las cercanias de un parque natural (Modificado a partir de base de datos de Zuur 2009) 
#la ruta se divide en 52 segmentos de 500 mts y se presenta el número total de anfibios muertos por segmento (“TOT.N”)
##

ls()
rm(list=ls())
ls()


setwd("C:/Users/snpre/Documents/Carrera/Biometria II/tp6")

datos <- read.delim("roadkills.txt")
names(datos)

#### a) Identifique la unidad muestral, la variable respuesta (y su potencial distribución de probabilidades) y la/s predictora/s involucrada/s. 

# um: segmento de 500m de la ruta
# vr: anfibios muertos por segmento
# ve: variables ambientales OPEN.L,MONT.S, POLIC, SHRUB, WAT.RES, L.WAT.C, L.P.ROAD, D.WAT.COUR y D.PARK

str(datos) # parecen ser todas cuantis
#### b) Plantee el modelo

#PL: N_i= beta0 + beta1*OPEN.L_i + beta2*MONT.S + beta3*POLIC_I + ...

# funcion de enlace -- logaritmo

# modelo log (E(Y_i)) = e**N_i

#### Describa los datos mediante gráfico/s y tabla/s ¿Detecta datos atípicos? ¿Cómo son las relaciones entre las variables?

#### seleccionamos las variables de interes (segun enunciado)
datos <- subset(datos[,c(5,7,9,11,12,14,15,17,19,20)])
names(datos)

#### aplicamos log d ela VR, para descriptiva
datos$ln_TOT.N <- log(datos$TOT.N)

#### grafico de dispersion entre pares de variables y coeficiente de correlacion
library(GGally)
# PARA VER SI SE CUMPLE LA LINEALIDAD EN LA ESCALA DEL PREDICTOR LINEAL
ggpairs(datos,mapping = ggplot2::aes(),lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1), discrete = "blank", combo="blank"), diag = list(discrete="barDiag", continuous = wrap("densityDiag", alpha=0.5 )), upper = list(combo = wrap("box_no_facet", alpha=0.5), continuous = wrap("cor", size=4, alignPercent=0.8))) + theme(panel.grid.major = element_blank())

# aceptamos el supuesto de linealidad pero esta medio raro. 
# (De modo anecdotico) Existen alternativas:
# alternativa 1: Se podria categorizar para quedarme en el mundo glm 
# alternativa 2: modelos gam, para relaciones no lineales

# Correlation matrix
corr_datos <- round(cor(datos[,2:10]), 2)
print(corr_datos)


library(corrplot) 
# Asociaciones entre variables ACA VEMOS COLINEALIDAD
corrplot(corr_datos, 
         type = "upper",
         method = "number")
# las correlaciones entre VE no son tan altos

# vemos en algunas predictoras datos muyyy separados del resto. Por eso se aplica una transformacion para acortar las distancias

# Se decide aplicar raiz cuadrada sobre las variables POLIC, SHRUB, WAT.RES, L.P.ROAD,
# D.WAT.COUR. 
# ¿Por que cree que se realizo este procedimiento? 
# Raiz cuadrada es una transformación que "acorta" las distancias en la X (notar los altos valores)

# ejemplo: 
# var original (x)
range(datos$POLIC) # [1]  0.000 11.263
# aplica raiz (x)
datos$SQ.POLIC <- sqrt(datos$POLIC)
# raiz de la var original (x)
range(datos$SQ.POLIC) # [1] 0.000000 3.356039

# aplica raiz al resto: 
datos$SQ.SHRUB <- sqrt(datos$SHRUB)
datos$SQ.WAT.RES <- sqrt(datos$WAT.RES)
datos$SQ.L.P.ROAD <- sqrt(datos$L.P.ROAD)
datos$SQ.D.WAT.COUR <- sqrt(datos$D.WAT.COUR)

#### Ajuste el modelo (aditivo con todas las predictoras incluidas) en R y evalúe los supuestos
m1 <- glm(TOT.N ~ OPEN.L + MONT.S + SQ.POLIC +D.PARK + SQ.SHRUB + SQ.WAT.RES +
            L.WAT.C +SQ.L.P.ROAD + SQ.D.WAT.COUR, family = poisson,data = datos)

#### analizar colinealidad
library(car)
vif(m1) # No hay problemas de vif
range(vif(m1)) #[1] 1.236836 2.085267

# Parametro de dispersion (del modelo aditivo completo).   Se podria hacer backward o forward
(dispersion<-sum(resid(m1, type="pearson")^2/m1$df.residual))
# La dispersion es casi 6 veces mas grande que la media. Hay que ver si es significativo
# Ahora hay que hacer prueba de hipotesis para ver si es suficiente para afirmar que en la poblacion hay sobredispersion

# podemos usar performance para Prueba hip sobredispersion
library(performance)
check_overdispersion(m1) # Overdispersion detected

# Con paquete Dharma
#Simulaciones Dharma
library(DHARMa)
sim <- simulateResiduals(m1, n = 1000)

# Graficos diagnosticos
plot(sim)
hist(sim)

# prueba de dispersion
testDispersion(sim, plot=F) # tests if the simulated dispersion is equal to the observed dispersion. Rechazo, hay evidencia de sobredispersion

# ENCONTRE EVIDENCIAS DE QUE NO HAY DISTRIBUCION UNIFORME, ENCONTRE MUCHOS OUTLIERS, ENCONTRE SOBREDISPERSION Y TENDENCIAS EN RESIDUOS VS PREDICHOS
# DA DISTINTO EL TEST DE DISPERSION PORQUE LO HACE DE OTRA MANERA

# nos da la informacion de ambas cosas Dharma: sobre y sub

#¿Es correcto modelar suponiendo distribucion de Poisson? ¿Por que? 
# NOO, encontramos sobredispersion! No se cumple el supuesto de la relacion esperanza vs varianza

#¿Como deberia modelar la variable respuesta segun la sobredispersion encontrada?
#### Alternativas 

## Modelos cuasi verosímiles *
## Binomial negativa
## Conway-Maxwell Poisson
## Tweedie

#### Propuesta para este ejercicio: 
# Ajustar el modelo utilizando la distribucion Binomial negativa. 
# Aplicar un metodo de seleccion de stepwise. En este caso "hacia adelante" (también podría ser "hacia atras") (Forward, Backward)
# Utilizar AIC como criterio para decidir la inclusion o no de una variable en el modelo.

####
library(glmmTMB)

# Se comienza ajustando un modelo nulo (sin variables predictoras)
mnulo <- glmmTMB(TOT.N ~ 1, data = datos, family = nbinom2)

summary(mnulo) # sin predictoras

# "add1" hace lo contrario que drop1, evalua la incorporacion
# de variables "de a una".

# para saber mas sobre la funcion add1:
?add1

# evaluar la incorporacion de nuevas variables sobre "mnulo"
add1(mnulo, TOT.N ~ OPEN.L + MONT.S + SQ.POLIC + D.PARK + SQ.SHRUB + SQ.WAT.RES + L.WAT.C +
       SQ.L.P.ROAD + SQ.D.WAT.COUR, test="Chisq")

# Que variable incorporaria en primer lugar?  D.PARK. Su p valor es el mas chico y el modelo que lo incluye tiene el menor AIC por mucho!

# Ajustar m2 incorporando al mnulo la variable seleccionada: 
m2<-update(mnulo,.~.+D.PARK)

# ... Repetir el procedimiento hasta que no haya más variables sig para incorporar al modelo.  

# evaluar la incorporacion de nuevas variables sobre m2
add1(m2, TOT.N ~ OPEN.L + MONT.S + SQ.POLIC + D.PARK + SQ.SHRUB + SQ.WAT.RES + L.WAT.C +
       SQ.L.P.ROAD + SQ.D.WAT.COUR, test="Chisq")

# open L es la siguiente, mismas razones

# Ajustar m3 incorporando al m2 la variable seleccionada: 
m3<-update(m2,.~.+OPEN.L)

# evaluar la incorporacion de nuevas variables sobre m3
add1(m3, TOT.N ~ OPEN.L + MONT.S + SQ.POLIC + D.PARK + SQ.SHRUB + SQ.WAT.RES + L.WAT.C +
       SQ.L.P.ROAD + SQ.D.WAT.COUR, test="Chisq")

# L wat c

# Ajustar m4 incorporando al m3 la variable seleccionada: 
m4<-update(m3,.~.+L.WAT.C)

# evaluar la incorporacion de nuevas variables sobre m4
add1(m4, TOT.N ~ OPEN.L + MONT.S + SQ.POLIC +
       D.PARK + SQ.SHRUB + SQ.WAT.RES + L.WAT.C +
       SQ.L.P.ROAD + SQ.D.WAT.COUR, test="Chisq")

# Agregar variables ya no modifica significativamente al modelo

# Evaluar los supuestos del modelo seleccionado.  
# Primero vemos si hay sobre o subdispersion
# Para m4
sum(resid(m4, type="pearson")^2)/(df.residual(m4)) # da re cercano a 1

# Simulaciones Dharma
library(DHARMa)
simNB <- simulateResiduals(m4, n = 10000)
plot(simNB)
hist(simNB)

# prueba de dispersion DHARMa
testDispersion(simNB, plot=F) # no rechazo

# Dispersion con performance
check_overdispersion(m4) #No overdispersion detected.


####
# Concluya en relacion a las condiciones que favorecen el atropellamiento de anfibios.
####

summary(m4) # Esta en escala logaritmica, Todos influyen, dan significativo

#intervalos de confianza escala PL
round(confint(m4),5)

# escala VR
round(exp(fixef(m4)$cond), 5)
#(Intercept)      D.PARK      OPEN.L     L.WAT.C 
#87.06254     0.99988     0.98934     1.20532 

# Todas las variables tienen estimadores positivos, todas aumentan los anfibios atropellados

#Intervalos de confianza escala de la VR
m<-round(exp(confint(m4)),5)
m
exp(-0.0107) # e**estimador en log de OPEN.L. Veo que da iguak que lo de arriba

# recordar que la magnitud de efecto de una regresion es la pendiente, la pasamos a la escala de la VR

# hacemos (estimado de OpenL - 1) * 100 para saber magnitud de efecto
(0.98934-1)*100 # -1.066

# Por cada aumento unitario de OPenL (ha de superficie sin vegetacion) la cantidad de anfibios atropellados por ramo disminuye en 1.06% controlando por las otras variables

(0.98311-1)*100 # -1.689
(0.99562-1)*100 #0.438
# Por cada ha de superficie sin vegetacion disminuye entre (0.438, 1.689) % el numero de anfibios atropellados medio en un segmento de 500 m manteniendo ctes las otras variables con una confianza del 95%

# DPARK
# Escala VR- agarro ambos intervalos 2.5 y 95 y le resto uno
m<-as.matrix(m[,1:2])
m
(m-1)*100 # Aca tengo todas las magnitudes de efecto en porcentaje con una confianza del 95%

library(ggeffects)

# cuantos graficos realiza? como se interpretan? 
# Parece ser uno para cada predictora dejando a las otras constantes
ggpredict(m4)

names(ggpredict(m4)) # D.PARK"  "OPEN.L"  "L.WAT.C"

plot(ggpredict(m4), add.data = TRUE)[1] # Distance to natural park. A mayor distancia, menor muertos
plot(ggpredict(m4), add.data = TRUE)[2] # Open lands, mayor area descubierta menor muertos
plot(ggpredict(m4), add.data = TRUE)[3] # Lenght of water courses, mas largos mas muertos

#i) Concluya en relación a las condiciones que favorecen el atropellamiento de anfibios.
# La distancia a un parque natural disminuye la cantidad de anfibios atropellados. Podria tener sentido pensando en que en un parque natural hay mayor riqueza y por eso son mas atropellados
# Mayor area descubierta disminuye la cantidad de anfibios atropellados. Puede ser por lo mismo, no hay tantos anfibios si el area es descubierta, habitan otro tipo de ambientes
# Lo contrario pasa para longitud de cursos de agua. Si hay cursos de agua mas largos hay mas muertos, nos indica que las especies prefieren habitats mas humedos y que entonces donde hay curso de agua hay mayor abundancia.

# Para saber mas: Zuur (2009), plantea este caso y realiza diversos analisis con distinto nivel de complejidad, debido a la naturaleza de los datos y las relaciones entre las variables; ademas compara distintas formas de modelar.
# (si leen el analisis completo veràn que ajusta modelos GAM)


#############################
#### Para practicar
#############################

#### Modelo poisson (ya vimos que no era correcto)
# Ajuste del modelo reducido
# m5: con family Poisson
m5 <- glmmTMB(TOT.N ~ OPEN.L +D.PARK + L.WAT.C, family = poisson, data = datos)

#Vemos los supuestos
#Simulaciones Dharma
sim <- simulateResiduals(m5, n = 1000)
plot(sim) # Da horrible ya sabiamos


#### Comparacion m4 ajustados con tweedie y compos
# m6: con family Tweedie (family=tweedie)
m6 <- glmmTMB(TOT.N ~ OPEN.L +D.PARK + L.WAT.C, family = tweedie, data = datos)
#Vemos los supuestos
#Simulaciones Dharma
sim6 <- simulateResiduals(m6, n = 1000)
plot(sim6) # Se desvia un solo cuantil

ggpredict(m6)
plot(ggpredict(m6), add.data = TRUE)[1] # Distance to natural park. A mayor distancia, menor muertos
plot(ggpredict(m6), add.data = TRUE)[2] # Open lands, mayor area descubierta menor muertos
plot(ggpredict(m6), add.data = TRUE)[3] # Lenght of water courses, mas largos mas muertos



# m7: con family compois (family=compois)

m7 <- glmmTMB(TOT.N ~ OPEN.L +D.PARK + L.WAT.C, family = compois, data = datos)
summary(m7)
#Vemos los supuestos
#Simulaciones Dharma
sim7 <- simulateResiduals(m7, n = 1000)
plot(sim7) # No se si convergio (aparecen warnings de exceeded iterations) pero en los supuestos de desvia un solo cuantil

ggpredict(m7)
plot(ggpredict(m7), add.data = TRUE)[1] # Distance to natural park. A mayor distancia, menor muertos
plot(ggpredict(m7), add.data = TRUE)[2] # Open lands, mayor area descubierta menor muertos
plot(ggpredict(m7), add.data = TRUE)[3] # Lenght of water courses, mas largos mas muertos

AIC(m4,m6,m7)
#   df      AIC
#m4  5 384.2536 binom
#m6  6 384.9314 tweedie
#m7  5 393.5983 Conway-Maxwell-Poisson

#### Alternativa modelo cuasi verosimil
# m8: modelo cuasi verosimil (funcion glm, family = "quasipoisson")

m8 <- glm(TOT.N ~ OPEN.L +D.PARK + L.WAT.C, family = "quasipoisson", data = datos)

#No se pueden chequear los supuestos para quasipoisson con dharma
#No se puede comparar con otros modelos porque usa quasiverosimilitud
