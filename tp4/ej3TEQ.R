rm(list=ls())
#Ejercicio 3, tratamiento de tumores

# 1. Identifique las variables explicativas, el diseño y la cantidad de réplicas.
# explicativas: configuracion (cualitativa) y dosis (cuantitativa)
# Estudio experimental, Diseño Completamente Aleatorizado
# variable respuesta:volumen de tumor en ml cubicos a los 20 dias, cuantitatvia continua. Dist normal
# ue: cada raton
# 5 replicas para cada combinacion dosis configuracion
# 2 predictoras y su interaccion

## las ecuaciones estan en la carpeta

datos<-read.csv("TEQ.csv", stringsAsFactors = TRUE)
str(datos)
summary(datos)


library(ggplot2)
library(emmeans)
library(car)
library(dplyr)

datos %>% 
  select(config, volumen) %>%
  group_by(config) %>%
  summarise_all(.funs = c(
    n = length, 
    media_volumen = mean, 
    sd = sd, 
    min = min, 
    max = max
  )
  )

datos %>% 
  select(dosis, volumen) %>%
  group_by(dosis) %>%
  summarise_all(.funs = c(
    n = length, 
    media_dosis = mean, 
    sd = sd, 
    min = min, 
    max = max
  )
  )
# 2. Escriba el modelo en parámetros, indicando el significado de cada término en
# contexto. Si alguna/s variables podrían ser incluida/s de diferente manera, elija
# aquella que resulte en un modelo con menor cantidad de parámetros.
#hecho en carpeta

#3. Realice un gráfico descriptivo en donde se muestre la relación entre el volumen del
#tumor a los 20 días y la dosis de TEQ aplicada, discriminando por tratamiento.

q1<-ggplot(data=datos, aes(x=dosis, y=volumen, colour=config))+theme_minimal()+geom_point()+labs(x="Dosis[ml]")
r1 <- q1 + geom_smooth(method = "lm", se = FALSE)
r1


# 4. Ajuste el modelo propuesto. ¿Cuáles y cuántos parámetros deben estimarse?
# Se estiman 5 parametros: b0, b1, b2, b3 y la varianza
#Modelo con interaccion

modelo1<-lm(volumen ~ dosis*config, datos)
anova(modelo1)
summary(modelo1)

# Supuestos!

e<-resid(modelo1) # residuos
re<-rstandard(modelo1) #residuos estandarizados
pre<-predict(modelo1) #predichos
par(mfrow = c(1, 2))
#5. Realice un gráfico de residuos del modelo vs valores predichos. En base al patrón
#observado, ¿qué supuestos evalúa y qué concluye?
# Se evalua homocedasticidad y linealidad 
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo 1" )
abline(0,0)

qqPlot(e, main = "QQplot -Modelo 1")

#6. Evalúe el supuesto de normalidad. ¿Cuál es la hipótesis nula de la prueba de
#hipótesis correspondiente? ¿Cuáles y cuántos datos se utilizan? Efectúe la prueba y
#concluya.
#Ho) Los errores siguen una distribución normal (N (0; sigma^2) ) y están independientemente distribuidos

shapiro.test(e)#0.94

# No hay evidencia suficiente para rechazar que los errores se ajustan a N (0; sigma^2)
# homocedasticidad lo veo unicamente en el grafico

# 7. Ajuste el modelo y concluya en relación al objetivo del estudio. Interprete el
#coeficiente estimado para configparábola*dosis (con un IC95%)

#AHora que se cumplen los supuetsos, miro los resultados
summary(modelo1)
anova(modelo1)

# VolTumoral=3199.77 -11.76*dosis -18.16*parabola+6,97*dosis*parabola

# LA INTERACCION ENTRE DOSIS Y PARABOLA ES SGINIFICATIVA
# es decir, el volumen tumoral, por lo menos por alguna dosis, se ve afectada segun la configuracion parabola

# existe relacion lineal entre el volumen y la dosis y difiere segun la configuracion

#8. A partir del modelo ajustado, ¿puede predecir el volumen medio que tendrá un tumor
#sometido a la configuración círculo con una dosis de 60 Coulombs? En caso que se
#pueda, estimar dicho valor, en caso que no sea posible, justifique indicando la razón

#Si se puede
# A mano:
3199.77 -11.76*60 # 2494.17 mm3
#Con predict:
nuevo<-data.frame(config="circulo", dosis=60)
predict(modelo1,nuevo, interval="prediction")
#      fit      lwr      upr
#1 2493.943 2456.909 2530.978

#Comparaciones

options(emmeans= list(emmeans = list(infer = c(TRUE, F)),
                      contrast = list(infer = c(TRUE, TRUE)))) # acá estamos seteando la salida de las tablas del emmeans , el primer True de cada paréntesis es para pedir los intervalos de confianza y el segundo es para pedir los valores P. 
# Comparacion de pendientes de las 3 fuentes (relacion entre DBO y pH para cada fuente)

comp_pendientes <- emtrends(modelo1,  pairwise ~ config, var="dosis") #, contr="cld"
comp_pendientes
plot(comp_pendientes$emtrends, comparisons = TRUE)

# Al tener solo dos factores, al ver que la diferencia es significativa ya sabemos que circulo y parabola difiere
# Fue al pedo hacer comparaicones aca

#9. Informe e interprete el IC 95% para la magnitud del máximo efecto entre
#configuraciones.

# Intervalo de confianza 95% para la interaccion
confint(modelo1)
confint(modelo1, "dosis:configparabola")

#                        2.5 %   97.5 %
#dosis:configparabola 6.509782 7.428487

# La diferencia media en este intervalo de confianza para que el efecto diferencial
# maximo entre consifguraciones es de 7,43
