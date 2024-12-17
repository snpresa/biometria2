
# Antes que nada veamos si hay algun comando u objeto almacenado en la memoria
ls()
# Si te aparece character(0) es que la memoria esta limpia, si no ejecuta
# la siguiente sentencia para borrar la memoria.
rm(list=ls())
# revisamos...
ls()
# No deberia haber nada en el entorno de trabajo ahora!


#### setear el directorio de trabajo .... (cada uno tendra una ruta diferente)

# setwd("~/")

#### Cargar el data.frame (si esta en el mismo directorio seteado no es necesario indicar la ruta)

Datos  <- read.csv("BIOMASARATONES.csv", stringsAsFactors = T) 


# Librerias necesarias ( si no están cargadas en el R deberán antes de activarlas instalarlas con la función install.packages (""))

library(dplyr)
library(pastecs)
library(ggplot2)
library(ggcorrplot)
library(corrplot) 
library(car) # 
library(caret)
library(sjPlot)
library(faraway)
library(ggeffects)
library(lm.beta)
library(MuMIn) 
library(performance)

install.packages ("ggcorrplot")
install.packages ("caret")
install.packages ("sjPlot")
install.packages ("MuMIn")
install.packages ("performance")
install.packages ("faraway")
install.packages ("lm.beta")

# luego se indican nuevamente antes de llamar a las funciones


#############################
# Inspeccion del data.frmae #
#---------------------------#
#############################

names(Datos)
head(Datos)
str(Datos)
summary(Datos)


#############################
# Analisis exploratorio     #
#---------------------------#
#############################
 # 1° Analizar relaciones entre variables explicativas ( para detectr colinealidad) y también entre cada una y la variable respuesta (linealidad)

summary(Datos)

# library(pastecs)
round(stat.desc(Datos[2:5]), 2)

Datos %>% 
  summarise_all(.funs = c(
    media = mean, 
    sd = sd, 
    min = min, 
    max = max
  )
  )


# para explorar asociaciones entre variables
plot(Datos[,2:6])

# Correlation matrix
names(Datos)
corr_datos <- round(cor(Datos[,2:6]), 2)
print(corr_datos)

#por lo pronto la relacion lineal que se ve es ratones llvuia, capaz semillas y ratones y semillas y lluvia

# library(ggplot2)
# library(ggcorrplot)
# library(corrplot) 


# Asociaciones entre variables
# Diferentes opciones graficas

ggcorrplot(corr_datos,  
           type = "lower", 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           outline.color="black",
           ggtheme=theme_bw)

# Correlacion positiva entre 
#lluvia y ratones - fuerte - 0.92
# semillas y ratones - fuerte - 0.83
# semillas y lluevia (media) - 0.77

# Ngetaiva entre
# pred y cob -0.33
# lluvia y cob 0.30

ggcorrplot(corr_datos,  
           type = "lower", 
           method="square", 
           outline.color="black",
           ggtheme=theme_bw)

# con otra libreria 
corrplot(corr_datos, 
         type = "upper",
         method = "number")

# mixto
corrplot.mixed(corr_datos, 
               lower.col = "black", 
               number.cex = .7)

# ¿Que variables se encuentran asociadas? (el VIF lo calculamos en la siguiente sección)
#¿Cómo es la relación entre las predictoras y la respuesta?
 
# ver arriba
# la variable respuesta es biomasa de ratones

###############
# Modelo(s)   #
#------------ #
###############

#2° Implementar modelos con distintas combinaciones de variables uilizando un método de selección hacia atrás


# m1
# modelo aditivo completo 
m1 <- lm(ratones ~ lluvia+predadores+cobertura+semillas, Datos)

# factor de inflacion de la varianza (VIF)
# library(car)
vif(m1) 

# ¿Existe colinealidad "importante" entre las variables incluidas en 
# el m1?

#  i el vif da 1 las variables son completamnete independientes
#vemos cierta colinealidad entre lluvia y semillas pero como es menor a 5 no parece critica

# Evaluo los supuestos del modelo, as? posibles datos at?picos en el m1 que podr?an afectar la selecci?n
e<-resid(m1) # residuos
re<-rstandard(m1) #residuos estandarizados
pre<-predict(m1) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gr?fico de dispersi?n de RE vs PRED m1" )
abline(0,0)
qqPlot(e, main = "QQplot -Modelo 1")
shapiro.test(e) #0.78

# se cumplen los supuestos

#tambien podemos evaluar los supuestos y la colinealidad con la libreria "performance"
library(performance)
check_model(m1) 

#uniendo los dos graficos, vemos que el residuo no hace tanta palanca
# continuo ...
summary(m1) # puedo ver el summary?
#anova(m1)
# comienza la "Backward selection"

# Existen varios criterios distintos (entre otros):

#  Identificar los terminos NS en el summary. ¿Que variable eliminarian con este criterio?

#  Comparar a traves de cambios en el AIC . 
#   Una forma de ver esto es con la funcion drop1, que compara el modelo con y sin
#    "X" termino. Ademas esta funcion drop1 devuelve la Suma de cuadrados residual del modelo. Ejemplo:

drop1(m1)

# "none" indica la RSS y el AIC del modelo SIN REMOVER ningun termino
# "lluvia" indica la RSS y el AIC del modelo Si decidimos eliminar lluvia del modelo
# "predadores" indica 23.8
# "cobertura" indica 24.7
# " semillas" indica 27.0 

# el mejor modelo es sacando predador

drop1(m1, test="F") # y agregando el test vemos la significacion de la disminución en la SC residual ( variabilidad no explicada) del modelo con una VE más y el reducido


##Interpretación de una salidad de drop1: 
#si P valor< 0.05): Indica que al eliminar el predictor del modelo, hay una diferencia significativa entre el modelo completo y el modelo reducido. Esto sugiere que el predictor eliminado es importante y tiene un efecto significativo en la respuesta porque se reduce significativamente la variabilidad no explicada. Por lo tanto, no debería eliminarse del modelo.

#P valor > 0.05): Indica que eliminar el predictor no produce una diferencia significativa en el ajuste del modelo. Esto sugiere que el predictor podría no ser importante y que el modelo podría simplificarse eliminando ese predictor sin perder capacidad explicativa.

# En el proximo paso se decide comenzar por eliminar del modelo a la variable predictora predadores
# ¿está de acuerdo con esta decision? Como es su resultado en el 'summary'? ¿y en el drop1? Como se modifica la RSS y el AIC al eliminar la variable predadores del modelo?

#si! mejora el AIC Y EL RSS (ambos bajan)
# su pValor es casi 1

#...continuamos

# m2
# modelo aditivo sin predadores como variable predictora

m2 <- lm(ratones ~ lluvia+cobertura+semillas, Datos)

e<-resid(m2) # residuos
re<-rstandard(m2) #residuos estandarizados
pre<-predict(m2) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gr?fico de dispersi?n de RE vs PRED m2")
abline(0,0)
qqPlot(e, main = "QQplot -Modelo 2")
shapiro.test(e) # 0.7785
# se cumplen los supuestos

# 
check_model(m2)


# comparacion m1 vs m2
# prueba de hipotesis para modelos anidados
# determina la significacion de la reducción en la SC residual

#anova solo: descomposicion de suma de cuadrados
#anova multiple: mira el cambio en la SCresidual
anova(m1,m2) # comparando modelo con y sin la variable predadores
#nos dice si al dejarla la reducción en la SC es significativa
# siempre que uno agregue variables va a explicar mas varianza, es por eso que se fijaen esta prueba si 
#la perdida de explicabilidad(?) es significativa

drop1(m1, test="F") # y tambien se puede ver con drop1, agregando el test 


# Analizamos resultados de m2
summary(m2)

drop1(m2, test = "F")

# corresponde elinar alguna nueva variable del m2? Si? NO? Por que?
# el AIC sigue bajando si sacamos cobertura. El mejor modelo es sin predadores y sin cob

# m3
# modelo aditivo sin predadores y sin cobertura como variables predictoras

m3 <- lm(ratones ~ lluvia+semillas, Datos)

e<-resid(m3) # residuos
re<-rstandard(m3) #residuos estandarizados
pre<-predict(m3) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gr?fico de dispersi?n de RE vs PRED m3" )
abline(0,0)
qqPlot(e, main = "QQplot -Modelo 3")
shapiro.test(e) # se cumplen los supuestos. p=0.6585


# comparacion # m1 vs m2 (aunque ya la teniamos) y m2 vs m3

anova(m1,m2,m3)
# no hay significancia entre los modelos

# Analizamos resultados de m3

summary(m3)

drop1(m3, test = "F")


# ?semillas queda en el modelo o sale?
# el mejor modelo es con semillas!

# m4
# Dejamos semillas y exploramos la posible interaccion entre lluvia y semillas

m4 <- lm(ratones ~ lluvia*semillas, Datos)

e<-resid(m4) # residuos
re<-rstandard(m4) #residuos estandarizados
pre<-predict(m4) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gr?fico de dispersi?n de RE vs PRED m4" )
abline(0,0)
qqPlot(e, main = "QQplot -Modelo 4")
shapiro.test(e) # 0.6012
# se cumplen los supuiestos

# comparacion # m4 vs m3

anova(m4,m3) # el p valor es alto, no cambian los modelos al agregar interaccion

# considera que el modelo debe incluir la interaccion entre lluvia y semillas?
# naaa

# (((
# Parentesis
# Analicemos que ocurre con el vif en el m4, que incorpora un termino de interaccion
vif(m4)
# La inclusion de una interaccion suele inducir colinealidad, 
#   solo afecta los EE de los terminos de menor orden
# Fin de parentesis
# )))


# m5
# Y si excluimos semillas del m3?

m5 <- lm(ratones ~ lluvia, Datos)
e<-resid(m5) # residuos
re<-rstandard(m5) #residuos estandarizados
pre<-predict(m5) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gr?fico de dispersi?n de RE vs PRED - m5" )
abline(0,0)
qqPlot(e, main = "QQplot -Modelo 5")
shapiro.test(e) #0.4575

# Analizamos resultados de m5

summary(m5)

drop1(m5, test = "F")


# modelos alternativos

# Y si no quisiera incluir en el mismo modelo lluvia y semillas? 
# comparar modelos alternativos para comenzar la seleccion ...

# modelo comenzando con lluvia (+predadores+cobertura)
m_lluvia <- lm(ratones ~ lluvia+predadores+cobertura, Datos)

# modelo comenzando con semillas (+predadores+cobertura)
m_semillas <- lm(ratones ~ semillas+predadores+cobertura, Datos)

# Puedo comparar esos modelos, y continuar la seleccion
AIC(m_lluvia, m_semillas)
#el AIC del modelo incluyendo lluvia es menor


###############################
# Comparacion de modelos
###############################

# CMe 
CMe <- round(c(summary(m1)$sigma^2,summary(m2)$sigma^2,summary(m3)$sigma^2,summary(m4)$sigma^2,summary(m5)$sigma^2), 2)

# R2 (no para comparar entre modelos)
R2 <- c(summary(m1)$r.squared, summary(m2)$r.squared, summary(m3)$r.squared, summary(m4)$r.squared, summary(m5)$r.squared)

# R2 ajustado
R2aj <- c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared, summary(m3)$adj.r.squared, summary(m4)$adj.r.squared, summary(m5)$adj.r.squared)

#AIC
AIC <- c(AIC(m1), AIC(m2), AIC(m3), AIC(m4), AIC(m5))

# Nombre modelo (para generar un data frame bonito)
modelo <- c(1,2,3,4,5)

comp <- cbind(modelo, CMe, round(R2,2), round(R2aj,2), AIC)

colnames(comp)<-c("modelo", "CMe", "R2", "R2 ajust", "AIC")

comp

summary(m3)

#Otra opci?n para la comparaci?n de modelos
compare_performance(m1,m2,m3,m4,m5)

#La raiz del error cuadratico medio (RMSE) es una metrica que nos dice que tan lejos estan nuestros valores predichos de nuestros valores observados en un analisis de regresion, en promedio.
#Un RMSE bajo ("bueno") significa que el modelo genera predicciones precisas (residuos pequenios)
#El AIC weights se pueden interpretar como probabilidades condicionales para cada modelo

####################
# Validacion cruzada
####################

# LOO CV
library(caret)

# comparo mis cuatro modelos candidatos
# agrego un m5, muy simple

# Indicamos la funci?n para el entrenamiento 
train.control<-trainControl(method = "LOOCV") 

# Entrenamos (estimamos) el modelo  (n modelos con n-1 observaciones) 

m1loo <- train(ratones ~ lluvia+predadores+cobertura+semillas, 
               data=Datos, method ="lm",trControl= train.control)
m2loo <- train(ratones ~ lluvia+cobertura+semillas, 
               data=Datos, method ="lm",trControl= train.control)
m3loo <- train(ratones ~ lluvia+semillas, 
               data=Datos, method ="lm",trControl= train.control)
m4loo <- train(ratones ~ lluvia*semillas, 
               data=Datos, method ="lm",trControl= train.control)
m5loo <- train(ratones ~ lluvia, 
               data=Datos, method ="lm",trControl= train.control)

# resultados

print(m1loo)
print(m2loo)
print(m3loo)
print(m4loo)
print(m5loo)

#dataframe con los resultados
b<-m1loo$results
c<-m2loo$results
d<-m3loo$results
e<-m4loo$results
f<-m5loo$results

comparacion_modelos <-rbind(b,c,d,e,f)
comparacion_modelos <- comparacion_modelos[,2:4] 

#agrego error relativo
comparacion_modelos$ER=comparacion_modelos$RMSE/mean(Datos$ratones)*100  
comparacion_modelos



###########################################################
# Modelo(s) seleccionado(s): Resultados e interpretacion  #
#-------------------------------------------------------  #
###########################################################

# Analicemos m3
summary(m3)

# chequeamos VIF
vif(m3)

# Ecuacion estimada del modelo

# Biomasa= -3.38+1.30*lluvia+0.82*semillas


# concluya, control bottom-up o top-down?
# Bottom-up: el mejor modelo, el que mas explica, es el que incluye las semillas


# Interpretacion de b0

# Esta fuera del rango

# Interpretacion de b0 si centramos lluvia y semillas

#seria la biomasa media de los ratones de la comunidad cuando la lluvai y las semillas es 0

# por cada mm adiiconal se espera un aumento en el promedio de la biomasa de ratones 
# entre 0.69 y 1.00 kg manteniendo constante la disponibilidad de semillas (para disponiblidad
# de semillas de entre 3 y 9 kg/anual) con una confianza del 95

########################
# Grafico/s final/es   #
# -------------------  #
########################


# Efecto parcial de cada variable. Predicciones marginales 
# a que valor ajusta a la otra variable? a una media ponderada
#library(ggeffects)


## opcion con ggpredict
#  si se explora el objeto "a" se puede ver a que valor de la otra variable se 
#   esta graficando

(a<-ggpredict(m3))
plot(a, add.data = TRUE, grid = TRUE)
?plot

## opcion plot model
# efectos parciales a valores especificos
# seleccionando el valor al que mantiene constante la otra variable
# ratones en fc de semilla para 3 valores de semillas
plot_model(m3, type = "pred", terms = c("lluvia", "semillas [3, 6, 9]"))

# ratones en fc de semillas para 3 valores de lluvia
plot_model(m3, type = "pred", terms = c("semillas", "lluvia [1, 5, 9]"))



# estimacion y comparacion de la magnitud del efecto

tab_model(m3) # sjPlot

# puede chequear que son los IC
confint(m3)


# install.packages("lm.beta")
#   library(lm.beta)
lm.beta(m3) #coeficientes estandarizados



###########################
# Seleccion automatica
###########################

# library(MuMIn)
# modelo aditivo
dredge(lm(ratones ~ lluvia+predadores+cobertura+semillas, data = Datos, na.action = "na.fail"))

# modelo con interaccion
dredge(lm(ratones ~ lluvia*predadores*semillas, data = Datos, na.action = "na.fail"))

# Modelos candidatos? donde se hubica el "m3"?


### FIN ###
