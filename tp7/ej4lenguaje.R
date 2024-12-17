rm(list=ls())
setwd("C:/Users/snpre/Documents/Carrera/Biometria II/tp7")

######################################################
####################  Problema 4   ################### 
############  Adquisicion del lenguaje  ##############
######################################################

# La primer columna es la edad (meses), la segunda el sexo(Masculino/Femenino), la tercera la cantidad de veces que un verbo se usó en tiempo pasado, y la cuarta la cantidad de errores cometidos de sobre-regularización. El objetivo en este caso es predecir la probabilidad de ocurrencia de sobre-regularización con respecto a la edad del niño, y si existen diferencias de adquisición según el sexo.

datos<- read.delim("lenguaje.txt", stringsAsFactors = T)

# 1. Indique cuál es la variable respuesta y las variables explicativas ¿Qué valores toman y cómo es su distribución?

# Variable respuesta: proporcion de errores cometidos de sobre-regularizacion. Posible distribucion ~ Binomial (pi_i, n)
# Variables explicativas: edad (meses), cuantitativa
#                         sexo, cualitativa 2 niveles: masculino femenino

#2. Construya a partir de los datos una variable que sea la proporción de errores de sobreregularización sobre el total de verbos usados en tiempo pasado.

datos$prop_errores<-datos$N_sobrereg/datos$N_verbos_pasado
summary(datos$prop_errores)

library(ggplot2)
ggplot(data=datos, aes(edad_meses, prop_errores, color=sexo)) + geom_point(size=2, aes(color=sexo))+geom_smooth(method=lm)+ labs(title="Proporcion de errores segun edad y sexo")

#3. Genere un modelo de regresión de la ocurrencia de errores de sobre-regularización en función de la edad.
M1 <- glm(prop_errores ~ edad_meses, family = binomial, weights = N_verbos_pasado, data = datos)

M1b <- glm(cbind(N_sobrereg, (N_verbos_pasado-N_sobrereg)) ~ edad_meses,family = binomial,data = datos)

AIC(M1,M1b)
#a. Analice el modelo obtenido, los supuestos asociados, y grafique el modelo obtenido en función de la variable explicativa. ¿Cuánto vale el odds para la misma?

# Supuestos
### Analisis de los SUPUESTOS
# A DIFERENCIA DE BERNOULLI, ACA SI CHEQUEAMOS DISPERSION YA NO HYA VR DICOTOMICA!
sum(resid(M1, type="pearson")^2)/M1$df.residual # 3.445

#Calculen del parametro de dispersion:

library(DHARMa)
testDispersion(M1, type = "DHARMa")
testDispersion(M1, type = "PearsonChisq")
# El pvalor es <0.05, rechazo debido a que hay SOBREDISPERSION.

# Cuando hay sobredispersion los EE son subestimados (se establecen menores de lo que realmente son) entonces los pvalores son mas bajos de los que deberian ser. Hay mas chances de falsos positivos (error de tipo I)

# !!!! Rechazamos la relacion varianza media asumida por binomial !!!! 

# Gráficos de res reescalados 
sim <- simulateResiduals(fittedMod = M1, plot = T)
# RECHAZO TODO menos outliers. Hay mayor dispersion, no hay distribucion uniforme
# Tambien rechaoz el test KS, no hay bondad de ajuste 
plotResiduals(sim, datos$sexo)
plotResiduals(sim, datos$edad_meses)

##### tengo que modelar zzzzzzzzz

#b. Escriba la ecuación del modelo obtenido.


#4. Incorpore al modelo la información sobre el sexo de la persona. Analice la presencia de interacción en el modelo. Grafique y escriba el modelo obtenido para ambos sexos.

#5. Concluya en términos del problema e Informe sus resultados. 