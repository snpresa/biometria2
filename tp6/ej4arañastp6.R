setwd("C:/Users/snpre/Documents/Carrera/Biometria II/tp6")
library(dplyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(ggeffects)
library(sjPlot)

################
## Problema 4 ##
################
#El propósito de este trabajo fue estudiar los efectos del glifosato sobre algunos atributos reproductivos de A. veniliae, en laboratorio. 

#Para ello, hembras fecundadas fueron criadas en frascos de vidrio individuales y alimentadas con moscas, que previamente habían sido tratadas con dosis distintas de glifosato (2) o con solvente. 
#Se utilizaron 10 hembras por tratamiento. Para cada hembra se registró la fecundidad (número de huevos) y la fertilidad (número de crías). No se observó un efecto letal del glifosato


#a) Identifique la unidad experimental, la variable respuesta (y su potencial distribución de probabilidades) y la/s predictora/s involucrada/s.

# Unidad experimental: arañas hembras fecundadas
# Variable respuesta: numero de huevos por hembra ~ Poisson(lambda_i)
# Variable explicativa/predictora: Tratamiento (dosis de glifosato aplicada en la mosca de alimento), cuantitativa aunque se podria modelar como cualitativa. VIENDO LA BD VEO QUE ESTA CUALITATIVA!!

# b) Plantee el modelo

# N_i = beta0 + beta1*DosisAlta_i + beta2*DosisBaja_i # siendo N la letra designada para el predictor lineal

# log(E(nro. de huevos por hembra_i)) = beta0 + beta1*Dosis_i+ beta2*DosisBaja_i 
# Yi~Poisson(lambda_i)
# E(nro. de huevos por hembra_i)= Var(nro. de huevos por hembra_i)

# c) Describa los datos mediante gráfico/s y tabla/s

datos<-read.csv("AraniaV2 (1).csv",header=TRUE ,sep = ";")
head(datos)
str(datos)

#'data.frame':	30 obs. of  3 variables:
#$ frasco  : int  1 2 3 4 5 6 7 8 9 10 ...
#$ trat    : chr  "control" "control" "control" "control" ...
#$ n_huevos: int  267 250 231 253 258 246 243 266 248 234 ...

datos$trat<-as.factor(datos$trat)
str(datos)
# $ trat    : Factor w/ 3 levels "control","glifo_alta",..: 1 1 1 1 1 1 1 1 1 1 ...

resumen <- datos[2:3] %>% 
  group_by(trat) %>%
  summarise_all(.funs = c(
    media = mean, 
    VAR = var, 
    min = min, 
    max = max
  )
  )

resumen
# A simple vista puedo ver que la varianza y la media difieren: la varianza siempre da menor que la media, sospecho subdispersion. Lo voy a probar mas adelante con el modelo

box2 <- ggplot(datos, aes(x=trat, y=n_huevos)) +
  geom_boxplot(color="black")+
  theme_bw() +  geom_jitter(alpha=0.3, size=2,aes(color=trat), position = position_jitter(width = .2))+ ylab("Numero de huevos por hembra")+xlab("Dosis de glifosato")+guides(color = "none")+ labs(title = "Numero de huevos medios por hembra segun dosis de glifosato \nen alimento", size=0.5)
box2

# d) Ajuste el modelo en R y evalúe los supuestos
m1 <- glmmTMB(n_huevos ~ trat, data=datos, family=poisson)
summary(m1) 

# SUPUESTOS: NO HAY NI SOBRE NI SUBDISPERSION
# Parametro de dispersion

# NO ME FUNCIONA DF$RESIDUAL??
sum(resid(m1, type="pearson")^2)/m1$df.residual # da 0
dispersion<- sum(resid(m1, type="pearson")^2)/27
dispersion

# Chequeo SOLO sobredispersion
library(performance)
check_overdispersion(m1) # No overdispersion detected

# Con paquete Dharma chequeo SOBRE Y SUB
#Simulaciones Dharma
sim <- simulateResiduals(m1, n = 1000)

# Graficos diagnosticos
plot(sim) # Me da signifcactivo el dispersion test! Despues no tengo evidencia para rechazar distribucion uniforme ni que haya muchos outliers
hist(sim) # No veo outliers

# prueba de dispersion
testDispersion(sim, plot=F) 
#dispersion = 0.43885, p-value = 0.006
testDispersion(sim, type="Pearson") # ESTO ME DEBERIA HABER DADO EL ANTERIOR!
# La prueba de dispersion dio significativa. Viendo que el parametro de dispersion es menor a 1 veo que hay subdispersion.
# No sería correcto modelar suponiendo una distribucion Poisson.
# Mis alternativas son Tweedie o Convey-Maxwell-Poisson

#### Modelos ajustados con tweedie y compois

# m2: con family Tweedie (family=tweedie)
m2 <- glmmTMB(n_huevos ~ trat, family = tweedie, data = datos)
sim2 <- simulateResiduals(m2, n = 1000)
# Graficos diagnosticos
plot(sim2) 
hist(sim2)
testDispersion(sim2, plot=F) 
# No hay evidencia de incumplimiento de los supuestos, nada dio significativo
#dispersion = 1.0338, p-value = 0.838

# m3: con family CoMPois (family=compois)
m3 <- glmmTMB(n_huevos ~ trat, family = compois, data = datos)
sim3 <- simulateResiduals(m3, n = 1000)
# Graficos diagnosticos
plot(sim3) 
hist(sim3) 
testDispersion(sim3, plot=F) 
# No hay evidencia de incumplimiento de los supuestos, nada dio significativo
#dispersion = 1.0513, p-value = 0.802

AIC(m2,m3)
anova(m2,m3)
#   df      AIC
#m2  5 227.4556
#m3  4 225.8649
# El modelo con menor AIC es el modelo 3 (2 puntos menos de AIC).
# Me quedo con m3, Conway-Maxwell-Poisson

# escala VR
round(exp(fixef(m3)$cond), 5)

#Intervalos de confianza escala de la VR
m<-exp(confint(m3))
m

m<-as.matrix(m[,1:2])
m
(m-1)*100


#e) Compare los tres tratamientos y concluya en relación al efecto del glifosato sobre la fecundidad de A. veniliae.

library(emmeans)
options(emmeans= list(emmeans = list(infer = c(TRUE, FALSE)),
                      contrast = list(infer = c(TRUE, TRUE)))) # opciones de seteo de las opciones de salida

# Escala del predictor lineal - Logaritmica
compPL <- emmeans(m3, pairwise ~ trat) # Tukey por default
compPL
# me da el logaritmo del numero medio de huevos por hembra para cada tratamiento y el logaritmo de la diferencia
# Veo diferencias entre todos 

# Escala de la variable respuesta
compVR <- emmeans(m3, pairwise ~ trat, type="response") # Tukey por default
compVR
# ahora si tenemos las medias de numero de huevos esperada
# El numero de huevos por hembra medio con tratamiento control es 1.48 veces el numero de huevos medio por hembra con tratamiento glifosato alta. Es entre 41.2 y 55.2 % mayor

# El numero de huevos por hembra medio con tratamiento control es 1.248 veces el numero de huevos con glifosato bajo. Es entre 20 y30 % mayor

# El numero de huevos con glifosato alta es 0.843 el numero de huevos con glifosato alta. Es entre 20 y 12 % menor

plot(compVR, comparisons = TRUE)
############################################
############# GRAFICOS FINALES #############
############################################

# Con la libreria SJPlot
plot_model(m3, type = "pred", terms = c("trat"))+ geom_jitter(data=datos, aes(x=as.numeric(trat),y=n_huevos), width=0.2)

# Con ggeffects
model_plot <- ggpredict(m3, 
                        terms = c("trat[control, glifo_alta, glifo_baja]"),
                        interval = "confidence")   
model_plot
plot(model_plot, add.data = T, jitter = 0.3, color="blue")   +
  xlab("Dosis de glifosato") +
  ylab("Numero de huevos por hembra")
