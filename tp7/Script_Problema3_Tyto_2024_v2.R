# *** Biometria II 2024 *** #
# Este es el script asociado al  Problema 3 - Dieta de Tyto alba
# del TP # 7  ##


# Antes que nada veamos si hay algun comando u objeto almacenado en la memoria
ls()
# Si aparece character(0) es que la memoria esta limpia, si no ejecuta
# la siguiente sentencia para borrar la memoria.
rm(list=ls())
# revisamos...
ls()
# No deberia haber nada en el entorno de trabajo ahora!


# setee el directorio de trabajo


setwd("C:/Users/snpre/Documents/Carrera/Biometria II/tp7")

## Carga del data.frame
Datos<- read.delim("tyto.txt", stringsAsFactors = T)
str(Datos)
#'data.frame':	41 obs. of  5 variables:
#$ ID         : int  1 2 3 4 5 6 7 8 9 10 ...
#$ Region     : Factor w/ 3 levels "Delta","Espinal",..: 2 2 2 2 2 2 2 2 2 2 ...
#$ N_presas   : int  153 214 199 134 308 167 308 586 231 308 ...
#$ N_aves     : int  0 1 6 5 5 1 0 10 5 5 ...
#$ porc_urbano: num  0.14 0 0 0.12 0.1 0 0 0.37 0.11 0.15 ...

###### 

# 1) ¿Cuál es la variable respuesta? ¿Qué valores toma y cómo es su distribución? ¿Cuáles son los parámetros de la misma? Describa el tipo de variables explicativas involucradas.

# Variable respuesta: el número de aves consumidas por egaropila. La distirbucion es Binomial y tiene dos parametros (pi, n)

# VE: Region. CUalitativa con 3 niveles
# VE: Porcentaje de urbanizacion. Cuantititativa
######

# 2) Explore si la proporción de aves consumidas se asocia con ambiente muestreado.

#### **************************
#### Exploracion del data.frame
#### **************************
str(Datos)
summary(Datos)
head(Datos)

# proporcion de aves en el total de presas

Datos$prop_aves <- Datos$N_aves/Datos$N_presas

# Chequear que se haya creado correctamente la nueva variable
  
class(Datos$prop_aves)   # numeric
Datos$prop_aves
summary(Datos$prop_aves)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.01623 0.02961 0.04080 0.05908 0.14019 

# cantidad de NO aves en el total de presas
Datos$no_aves <- Datos$N_presas-Datos$N_aves
summary(Datos$no_aves)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#98.0   203.0   303.0   319.3   396.0   747.0 

#### ***********
#### Desriptiva
#### ***********

library(dplyr)
library(ggplot2)

#### **************************
#### medidas resumen
#### **************************

#Medidas resumen de prop_aves por region
Datos %>%
  select(Region, prop_aves) %>%
  group_by(Region) %>%
  summarise_all(.funs = c(
    n = length, 
    media = mean, 
    sd = sd, 
    min = min, 
    max = max
  )) %>%
    mutate_if(is.numeric, round, 3) #Este argumento es para setear cuantos dígitos luego de la coma
  
#### Las medias de prop_aves por region son los estimadores de pi_i

#Medidas resumen: "n" presas totales, presas aves y presas no_aves
Datos %>%
  select(Region, N_presas, N_aves, no_aves) %>%
  group_by(Region) %>%
  summarise_all(.funs = c(
    total = sum
  )) %>%
  mutate_if(is.numeric, round, 3) #Este argumento es para setear cuantos dígitos luego de la coma


#### Calcular las estimaciones de ODDS de cada region 
# (Probabilidad de consumo de aves sobre probabilidad de no consumo de aves)


#ODDS Delta
(294/4137)/(3843/4137)
# ODDS Espinal
#etc

# Medidas resumen porcentaje de urbanizacion por region
Datos %>%
  select(Region, porc_urbano) %>%
  group_by(Region) %>%
  summarise_all(.funs = c(
    n = length, 
    media = mean, 
    sd = sd, 
    min = min, 
    max = max
  )) %>%
  mutate_if(is.numeric, round, 3)

#La pampa tiene un porcercaje de urbanizacion mas alto que el resto de los ambientes

#### **************************
#### Graficos descriptivos
#### **************************

# por region
qplot(x=Region, y=prop_aves, data=Datos, geom=c("boxplot","jitter"), fill=Region)

# en funcion del porcentaje de urbano discriminado por region
ggplot(Datos, aes(x=porc_urbano, y=prop_aves, colour=Region, group=Region)) + geom_point(size=3)



#### ******************
#### Ajuste del modelo 
#### ******************

## Regresion binomial - Dos maneras de acerlo

## variable respuesta proporcion (se debe incluir el n total como "weights")
M1 <- glm(prop_aves ~ Region * porc_urbano, family = binomial, weights = N_presas, data = Datos)

## variable respuesta "exitos, fracasos")
M1b <- glm(cbind(N_aves, (N_presas-N_aves)) ~ Region * porc_urbano,family = binomial,data = Datos)

AIC(M1,M1b) # son exactamente iguales

#### COLINEALIDAD Y SUPUESTOS

### Evaluamos colinealidad (vif del paquete car)

# Se chequea con modelo aditivo (en el modelo con interaccion ?que pasaria?)
# En un modelo con interaccion habria colinealidad espuria
# lA INTERACCION FUERZA LA COLINDEALIDAD
# Esto es exactamente igual a los modelos de arriba, no se porque se hace de nuevo
M2 <- glm(prop_aves ~ Region + porc_urbano, family = binomial, weights = N_presas,data = Datos)

# el operador "::"  permite utilizar solo una funcion de la libreria sin cargarla completa

car::vif(M2) ### con esta sintaxis se llama a la funcion vif del paquete car

# existe colinealidad entre region y porcentaje urbano? Creeria que no porque es cercano a 1

#GVIF Df GVIF^(1/(2*Df))
#Region      1.489434  2        1.104728
#porc_urbano 1.489434  1        1.220424

# ?Que es GVIF?
# GVIF: Generalized Collinearity Diagnostics
# Link al paper: 
# https://www.tandfonline.com/doi/abs/10.1080/01621459.1992.10475190#.U2jkTFdMzTo
# To make GVIFs comparable across dimensions, we suggested using GVIF^(1/(2*Df)), where Df is the number of coefficients in the subset
# Chequeamos esto porque es un estudio observacional, capaz se pisaban

### Analisis de los SUPUESTOS
# A DIFERENCIA DE BERNOULLI, ACA SI CHEQUEAMOS DISPERSION YA NO HYA VR DICOTOMICA!
# F dispersion
sum(resid(M1, type="pearson")^2)/M1$df.residual
# Da 6.42, su dispersion es 6 veces la esperada

#Calculen del parametro de dispersion:

library(DHARMa)
testDispersion(M1, type = "DHARMa")  # la funcion testDispersion permite setear el calculo del F dispersion (tipo DHARMa o Tipo Pearson -este ultimo es equivalente a sum(resid(M1, type="pearson")^2)/M1$df.residual)

testDispersion(M1, type = "PearsonChisq") # ESTE DA IGUAL AL ORIGINAL, AL QUE HACEMOS A MANO
# El pvalor es <0.05, rechazo debido a que hay SOBREDISPERSION.

#  ?que ocurre con la dispersion? ?que tipo de error aumenta en este caso? ?Podemos usar este modelo?

# Cuando hay sobredispersion los EE son subestimados (se establecen menores de lo que realmente son) entonces los pvalores son mas bajos de los que deberian ser. Hay mas chances de falsos positivos (error de tipo I)

# !!!! Rechazamos la relacion varianza media asumida por binomial !!!! 

# Gráficos de res reescalados 
sim <- simulateResiduals(fittedMod = M1, plot = T)
# RECHAZO TODO. Hay mayor dispersion, hay outliers, no hay distribucion uniforme
# Tambien rechaoz el test KS, no hay bondad de ajuste 
plotResiduals(sim, Datos$Region)
plotResiduals(sim, Datos$porc_urbano)


### Alternativas para modelado de sobredispersion en dist binomial: 

### ***1)*** quasibinomial
### ***2)*** distribucion Beta-binomial
### ***3)*** OLRE


### ***1)*** quasibinomial
##############################

# Modelo con interaccion
M1q <- glm(prop_aves ~ Region * porc_urbano,family = quasibinomial, weights = N_presas, data = Datos)
drop1(M1q, test = "F") 

# Cuando saco la interaccion el estadistico F da 0.511---> no deberia incluirla?

?drop1
## Por que usamos test F y no Chisq? Ver en help del drop1
## ?drop1 : "The F test is only appropriate for lm and aov models or perhaps for glm fits with estimated dispersion." 

# Modelo aditivo
M2q <- glm(prop_aves ~ Region + porc_urbano, family = quasibinomial, weights = N_presas, data = Datos)
drop1(M2q, test = "F")
# Cuando saco la region se re pudre pero cuando saco porc_urbano da casi 1

# Diria que lo que mas explica es solo region

# Modelo simple solo con Region
M3q <- glm(prop_aves ~ Region,family = quasibinomial, weights = N_presas, data = Datos)
drop1(M3q, test = "F")


### Modelo final e interpretacion
##  M3q  OSEA SOLO UNA COMPARACION DEMEDIAS!!!

#% de Devianza explicada (pseudo R2)
(summary(M3q)$null.deviance-summary(M3q)$deviance)/summary(M3q)$null.deviance*100
# el modelo expica el 36.94% de la variacion observada

# comparaciones a posteriori

library(emmeans)
options(emmeans= list(emmeans = list(infer = c(TRUE, FALSE)),contrast = list(infer = c(TRUE, TRUE))))

#en escala del PL
comp <- emmeans(M3q, pairwise ~ Region, adjust = "tukey") #Tukey por default  
summary(comp)
plot(comp, comparisons = TRUE)

# Delta difiere de espinal y de pampa 
# Espinal y pampa no difieren entre si

#en escala de la VR
compVR <- emmeans(M3q, pairwise ~ Region, type = "response") #Tukey por default  
summary(compVR)
plot(compVR, comparisons = TRUE)
# La mayor proporcion de aves consumidas se da en Delta

# La proporcion de aves cnsumidas en el Delta es 3.455 veces la del Espinal y 2.130 veces la de la pampa

# La proporcion de aves consumidas en el Delta es 2.45 veces MAYOR (RESTO 1 PARA PODER DECIR MAYOR!!!)

# Grafico final
# grafico de medias con IC95% en escala de pb

## modelo final resumen 
library(ggeffects)
predichos <-ggpredict(M3q, 
                      terms = ~ Region,
                      interval = "confidence")   
predichos

plot(ggpredict(M3q, terms = ~Region))+
  labs(y="Region") +
  labs(x="prob consumo de aves")

# grafico de medias con IC95% en escala de pb
# opcion con emmeans
estad <-as.data.frame(compVR$emmeans)
estad

# Las barras de error representan el DE
library(ggplot2)
ggplot(estad, aes(x=Region, y=prob)) + 
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), width=.1) +
  geom_point(colour = "red", size = 3)+ ylab("prob consumo de aves") + 
  theme_grey(base_size = 16) +
  annotate("text", x = c(1, 2, 3),
                   y = c(0.1, 0.04, 0.06), 
               label = c("B", "A", "A")) +
  theme_bw()



### ***2)*** Beta-binomial 
############################

#### Modelo betabinomial
library("glmmTMB")
modelo1BetaBin <-glmmTMB(prop_aves ~  Region*porc_urbano, family=betabinomial, weights=N_presas, data=Datos)
summary(modelo1BetaBin)
# Solo region pampa da significativo
drop1(modelo1BetaBin, test = "Chisq")

modelo2BetaBin <-glmmTMB(prop_aves ~  Region+porc_urbano, family=betabinomial, weights=N_presas, data=Datos)
summary(modelo2BetaBin)
# Region pampa tambien da significativa
drop1(modelo2BetaBin, test = "Chisq")

modelo3BetaBin <-glmmTMB(prop_aves ~  Region, family=betabinomial, weights=N_presas, data=Datos)
summary(modelo3BetaBin)
drop1(modelo3BetaBin, test = "Chisq")



### ***3)*** OLRE
### (para la proxima clase)
############################

# Este enfoque incorpora al ultimo nivel de observacion como factor aleatorio. Por lo tanto, ahora se trata d eun modelo mixto

### Mas adelante (GLMM)
modelo1OLRE <- glmmTMB(prop_aves ~ Region*porc_urbano + (1|ID),family = binomial,weights = N_presas,data = Datos)
summary(modelo1OLRE)
drop1(modelo1OLRE, test = "Chisq")

modelo2OLRE <- glmmTMB(prop_aves ~ Region+porc_urbano + (1|ID),family = binomial,weights = N_presas,data = Datos)
summary(modelo2OLRE)
drop1(modelo2OLRE, test = "Chisq")

modelo3OLRE <- glmmTMB(prop_aves ~ Region+porc_urbano + (1|ID),family = binomial,weights = N_presas,data = Datos)
summary(modelo3OLRE)
drop1(modelo3OLRE, test = "Chisq")

