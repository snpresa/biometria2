##########################
#####   Problema 4   #####
##########################
library(glmmTMB)
library(DHARMa)

setwd("C:/Users/snpre/Documents/Carrera/Biometria II/tp8")


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

# Variable respuesta: el número de aves consumidas por egaropila. La distribucion es Binomial y tiene dos parametros (pi, n)

# VE: Region. CUalitativa con 3 niveles
# VE: Porcentaje de urbanizacion. Cuantititativa


Datos$prop_aves <- Datos$N_aves/Datos$N_presas
# Chequear que se haya creado correctamente la nueva variable
class(Datos$prop_aves)   # numeric
Datos$prop_aves
summary(Datos$prop_aves)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.01623 0.02961 0.04080 0.05908 0.14019 

# El modelo clasico binomial presentaba sobre dispersion

### ***1)*** quasibinomial
##############################
# Modelo simple solo con Region
Mquasi <- glm(prop_aves ~ Region,family = quasibinomial, weights = N_presas, data = Datos)

### ***2)*** Beta-binomial 
############################

mBetaBin <-glmmTMB(prop_aves ~  Region, family=betabinomial, weights=N_presas, data=Datos)

#a) Ajuste un modelo que contemple la sobredispersión identificada en la proporción de aves consumidas por Tyto Alba a partir de un modelo OLRE, es decir, incorporando un efecto aleatorio de individuo al modelo.

### ***3)*** OLRE
############################

# Este enfoque incorpora al ultimo nivel de observacion como factor aleatorio. Por lo tanto, ahora se trata d eun modelo mixto
modelo1OLRE <- glmmTMB(prop_aves ~ Region*porc_urbano + (1|ID),family = binomial,weights = N_presas,data = Datos)
summary(modelo1OLRE)
drop1(modelo1OLRE, test = "Chisq")

modelo2OLRE <- glmmTMB(prop_aves ~ Region+porc_urbano + (1|ID),family = binomial,weights = N_presas,data = Datos)
summary(modelo2OLRE)
drop1(modelo2OLRE, test = "Chisq")

# Modelo simple, sin porc urbano
modelo3OLRE <- glmmTMB(prop_aves ~ Region+ (1|ID),family = binomial,weights = N_presas,data = Datos)
summary(modelo3OLRE)
drop1(modelo3OLRE, test = "Chisq")

# Supuestos
# CREO QUE NO TIENE SENTIDO CALCULAR DISPERSION ACA
sum(resid(modelo3OLRE, type="pearson")^2)/modelo3OLRE$df.residual
testDispersion(modelo3OLRE, type = "DHARMa")
testDispersion(modelo3OLRE, type = "PearsonChisq") 
# Gráficos de res reescalados 
sim <- simulateResiduals(fittedMod = modelo3OLRE, plot = T)
# NO rechazo nada



#b) Compare los 3 enfoques analíticos utilizados para modelar sobredispersión en binomial (cuasibinomial, betabinomial y OLRE) en relación a: distribución asumida, cantidad de parámetros estimados por el modelo, posibilidad de analizar supuestos, métricas de comparación de modelo (ej AIC) y resultados obtenidos.

AIC(modelo3OLRE, mBetaBin, Mquasi)
# OLRE asumen binomial
# beta binomial
#quasibinomial

# Ambos parecen estimar misma cantidad de parametros
# Creo que ni en quasi ni en olre chequeo dispersion

# Me quedaria con la beta binomial