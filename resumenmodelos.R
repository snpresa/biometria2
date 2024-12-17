rm(list=ls())
#### Librerias ####
library(reshape2)
library(lme4)
library(lmerTest) #IMPORTANTE PARA CORRER LMER()
library(psych)
library(dplyr)
library(pastecs)
library(ggcorrplot)
library(corrplot) 
library(caret)
library(sjPlot)
library(faraway)
library(lm.beta)
library(MuMIn) 
library(performance)
library(car)
library(ggplot2)
library(gridExtra) 
library(nlme) # "Nonlinear Mixed-Effects Models"
library(ggeffects) # para ggpredict

#### MODELO LINEAL TP1####
m1 <- lm(LongSepalo~especie, data=datos)  

## Con la funcion lm (linear model) se ajusta un modelo lineal, 
# en este caso, la longitud del sepalo variable respuesta/dependiente vs 
# la variable predictora/explicatoria/independiente especie -que es una variable categorica con 3 niveles-)

#### Supuestos RL####
e<-residuals(m1) # residuos
re<-rstandard(m1) # residuos estandarizados
pre<-predict(m1) # predichos
#Gráficos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Graf de dispersion de RE vs PRED" )
abline(0,0, col="red")
qqPlot(e, main = "QQ Plot residuos")
par(mfrow = c(1, 1))
#Pruebas analíticas de los supuestos
shapiro.test(e) 
shapiro.test(modelo_1$residuals) #0.0008822
# Homogeneidad de varianza. Prueba de Levene
leveneTest(datos$LongSepalo, datos$especie) 
leveneTest(cra~fertilizacion*riego, datos)

#Para conclusiones e intervalos de confianza
confint(m1)

#tambien podemos evaluar los supuestos y la colinealidad con la libreria "performance"
library(performance)
check_model(m1) 

# Funcion supuestos
supuestos <- function (modelo) {      
  
  residuos <- resid(modelo, type = "pearson")
  predichos <- predict(modelo)
  
  residuos <- resid(modelo, type = "pearson")
  predichos <- predict(modelo)
  
  par(mfrow = c(1, 2))
  
  plot(x = predichos,
       y = residuos,
       xlab = "Predichos",
       ylab = "Residuos de Pearson",
       main = "Grafico de dispersion de residuos v. predichos", 
       cex.main = 0.8 )
  abline(h=0, lty=2)
  qqPlot(residuos)
  shapiro.test(residuos)
  
}

#### MODELADO DE VARIANZA GLS TP3 ####
## Modelado con minimos cuadrados generalizados. (nlme)
# SIN MODELAR VARIANZA
modelo_2 <- gls(T_ing ~ Tratamiento, data = Datos) 
plot(modelo_2) # GRAFICO DE DISPERSION DE PRED VS RESIDUOS ESTANDARIZADOS.
r2 <- residuals(modelo_2, type = "pearson") # = estandarizados.
pred2 <- fitted(modelo_2)
# VARIDENT
modelo_varIdent <- gls(T_ing ~ Tratamiento,
                       weights = varIdent(form = ~1 | Tratamiento),
                       data = Datos)
modelo_varIdent <- gls(crecimiento ~especie*ubicacion,
                       weights = varIdent(form = ~1 | especie*ubicacion),
                       data = data)

# VARPOWER
modelo_varPower <- gls(T_ing ~ Tratamiento,
                       weights = varPower(),
                       data = Datos)
# VAREXP
modelo_varExp <- gls(T_ing ~ Tratamiento,
                     weights = varExp(),
                     data = Datos)

#### SELECCION DE MODELOS####
AIC(modelo_varIdent, modelo_varPower, modelo_varExp)
drop1(m1, test="F")
##Interpretación de una salidad de drop1: 
#si P valor< 0.05): Indica que al eliminar el predictor del modelo, hay una diferencia significativa entre el modelo completo y el modelo reducido. Esto sugiere que el predictor eliminado es importante y tiene un efecto significativo en la respuesta porque se reduce significativamente la variabilidad no explicada. Por lo tanto, no debería eliminarse del modelo.

#P valor > 0.05): Indica que eliminar el predictor no produce una diferencia significativa en el ajuste del modelo. Esto sugiere que el predictor podría no ser importante y que el modelo podría simplificarse eliminando ese predictor sin perder capacidad explicativa.

#La raiz del error cuadratico medio (RMSE) es una metrica que nos dice que tan lejos estan nuestros valores predichos de nuestros valores observados en un analisis de regresion, en promedio.
#Un RMSE bajo ("bueno") significa que el modelo genera predicciones precisas (residuos pequenios)
#El AIC weights se pueden interpretar como probabilidades condicionales para cada modelo


#### COLINEALIDAD ####
vif(m1) 

#### Comparaciones####
library(emmeans)
# seteo de opciones de salida (opcional, modificando los argumentos T / F puede ver como se modifican las salidas del emmeans)
options(emmeans= list(emmeans = list(infer = c(TRUE, FALSE)),contrast = list(infer = c(TRUE, TRUE))))
# comparaciones
comp <- emmeans(m1, pairwise ~ especie)
comp
plot(comp, comparisons = TRUE)

#      2.2    Comparaciones de Efectos simples- ma spotencia de comparaicon
#             Dentro de cada nivel de uno de los factores comparo los niveles del otro

#       (*) Efecto simple A. Comparar los niveles de densidad dentro de cada nivel provincia

efsimple_densidad <- emmeans(modelo, pairwise ~ densidad | provincia)
efsimple_densidad

plot(efsimple_densidad$emmeans, comparisons = TRUE)

#       (*) Efecto simple B. Comparar entre provincias dentro de cada nivel de densidad

efsimple_provincia <- emmeans(modelo, pairwise ~ provincia | densidad )
efsimple_provincia

plot(efsimple_provincia$emmeans, comparisons = TRUE)


#### Comparar pendientes - Regresion multiple ####
comp_pendientes <- emtrends(modelo1,  pairwise ~ config, var="dosis") #, contr="cld"
comp_pendientes
plot(comp_pendientes$emtrends, comparisons = TRUE)

#### Comparar variable respuesta a VE media ####
# Comparaciones de los valores de DBO medios de cada fuente
comp_fuentes <- emmeans(modelo1, pairwise ~ fuente|pH)
comp_fuentes #dif entre fuentes para pH promedio
# Existen diferencias  significativas entre el DBO medio entre las 3 fuentes a pH promedio

plot(comp_fuentes$emmeans, comparisons = TRUE)

#### Comparar variable repsuesta a VE min/max####
Fuentes_en_PH_PH_min_max <- emmeans(modelo1, pairwise~ fuente:pH, cov.reduce = range)
Fuentes_en_PH_PH_min_max
plot(Fuentes_en_PH_PH_min_max$emmeans, comparisons = TRUE)

#### Predecir a cierto nivel - Regresion multiple ####
#Con predict:
nuevo<-data.frame(config="circulo", dosis=60)
predict(modelo1,nuevo, interval="prediction")


#### Modelos mixtos y marginales####

# Aca re usamos la funcion supuestos
# Modelo condicional - Se usa igual si es regresion o comp de medias
m1 <-lmer(Glucemia ~ Tratamiento*Tiempo+(1|Equino), datos)

# Saber cuantos random effects estimamos:
length(ranef(m1)$manada$'(Intercept)')
coef(m2) #muestra los coef para cada nivel aleatorio (camada)

fitted(m2)  #predicciones parte fija + aleatoria
fixef(m2) #estimacion parametros efectos fijos
pred_fija<-fixef(m2)#predicciones parte fija 
alfai<-round(ranef(m2)$camada$'(Intercept)', 4)
pred<-cbind(bd$etanol,bd$camada, bd$vol,pred_fija, alfai,round(fitted(m2),4))
colnames(pred)<-c("etanol", "camada","vol", "pred fija", "efecto aleat", "pred fija + aleat")
pred

# ICC
# manual o
library(performance)
model_performance(Modelo, metrics = "ICC")


# Marginal con simetria compuesta (es decir, condicional)
m11 <- gls(Glucemia ~ Tratamiento * Tiempo,
           correlation = corCompSymm(form = ~ 1 | Equino),
           datos)
# Modelo alternativo 2: Autoregresivo de orden 1
m2 <- gls(Glucemia ~ Tratamiento * Tiempo,
          correlation = corAR1(form = ~ 1 | Equino),
          datos)
#NUEVO SUPUESTO h0 los efectos aleatorios se ajustan a una dist normal con media 0 y varianza entre ratones asignados al mismo tratamiento
alfai<-ranef(m1)$raton$'(Intercept)'
shapiro.test(alfai) #0.672
qqPlot(alfai, main="QQ Plot residuos")

#Al estimar el modelo se obtuvieron (completar con valor numérico) __40__ ranef (efectosaleatorios, uno para cada raton). El estimador de la varianza de los efectos aleatorios es de (completar con valor                                              numérico) ___460___ y sus unidades son (completar o poner una “x” si no tiene unidades) __cc**2**2____ . El valor del CCI es de (completar con valor numérico) __0.85__ y su interpretación, en contexto, es "85 % de la variabilidad esta explicada por la variacion entre ratones (tambein su complemento)" Ó "hay correlacion entre meidicones dentro de cada raton, cada uno es mas homogeneo" . Se concluye que hay (mucha/******poca******) poca variabilidad en el volumen de tumores de un mismo raton

####Estudiar la correlacion en Marginales####
Datos_wide <- dcast(datos, individuo + tratamiento ~ tiempo,
                    value.var="t_reac")
# exploramos el nuevo data frame
head(Datos_wide)
# para armar la matriz de correlacion, se seleccionan las columnas que tienen informacion sobre la biomada microbiana, para el calculo de las correlaciones.
datos_correlaciones <- Datos_wide[,3:6]
# exploramos el nuevo data frame
ggpairs(datos_correlaciones)+theme_bw()
ggcorrplot(cor(datos_correlaciones), lab=TRUE)


####Validacion del modelo####
# Predichos vs observados
p<-ggplot(Datos, aes(x =DBO , y = predict(modelo1), colour =fuente)) + geom_point(size=4)
p + geom_abline(intercept = 0, slope =1) +  ggtitle("Predichos vs observados")
# correlacion entre predichos y observados
cor <- cor(predict(modelo1), Datos$DBO)
cor
cor^2

##### Graficos finales####
## una opcion es extraer los valores predichos por el modelo del emmeans y luego construir el grafico
resumen_modelo <-as.data.frame(comp$emmeans)
# exploramos el objeto resumen_modelo
resumen_modelo  # emmeans es la media estimada
ggplot(resumen_modelo, aes(x=especie, y=emmean)) +
  labs(x="especie") + labs(y="longitud sepalo") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), color="blue", width=0.2)+
  geom_point(shape=1, size=2, color="blue") +
  ylim(0,8)+
  ggtitle("Comparaci?n de long de sepalo entre especies", "Media ? Error est?ndar") +
  annotate("text", x = c(1,2,3), y=c(6,7,8), label = c("A", "B", "C"))

## Otra opcion es graficar los valore spredichos con la libreria ggeffects
# Me gusta mas esta

model_plot <-ggpredict(m1, 
                       terms = c("especie"),
                       interval = "confidence")   
model_plot
grafico <- plot(model_plot, add.data = F) # para agregar los puntos observados add.data = T

# AGREGUE LOS DATOS CON GEOM_JITTER

grafico + ggtitle("valores predichos") + labs(y="Long sepalo (mm))") +
  annotate("text", x = c(1,2,3), y=c(5.3,6.3,7), label = c("A", "B", "C")) + geom_jitter(data = datos, aes(x = as.numeric(especie), y = LongSepalo), width = 0.2, height = 0, color = "blue", size = 1.5, alpha = 0.6)

# Lo mismo, otro ejercicio
model_plot <- ggpredict(modelo, 
                        terms = c("RU"),
                        interval = "confidence")   
model_plot
plot(model_plot, add.data = T)   +
  xlab("Dosis de RU (ug/huevo)") +
  ylab("Indice de daño")

library(sjPlot)
plot_model(Modelo, type = "pred", terms = c("dosis", "hongo"))


# Validacion cruzada en biomasa