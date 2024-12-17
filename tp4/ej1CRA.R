library(ggplot2)
library(dplyr)     # La vamos a utilizar para armar tablas resumen
library(gridExtra)
setwd("C:/Users/snpre/Documents/Carrera/Biometria II/Posit/4")

datos<-read.csv("CRA.csv")
datos$envase<-factor(datos$envase)
datos$riego<-factor(datos$riego)
datos$fertilizacion<-factor(datos$fertilizacion)
head(datos)
str(datos)

# 4. Describa estadísticamente los datos. Efectúe un gráfico de perfiles, concluya en relación a la potencial interacción entre los factores involucrados en el ensayo
## Tabla resumen (replicas, tendencia central y dispersion)
head(datos)
datos[2:4] %>% 
  group_by(fertilizacion, riego) %>%
  summarise_all(.funs = c(
    n = length, 
    media = mean, 
    sd = sd, 
    min_CRA = min, 
    max_CRA = max
  )
  )

ggplot(data=datos, aes(x=fertilizacion, y=cra, colour=riego))+geom_boxplot()+geom_jitter(alpha=0.3, size=2)+ labs(title="Efecto de la fertilizacion y el riego sobre el CRA")
# En este boxplot podemos ver que las varianzas no parecerian ser constantes entre los grupos

# Grafico de perfiles
ggplot(data = datos, aes(x = riego, y = cra, fill = fertilizacion)) +
  geom_smooth(aes(group = fertilizacion, colour = fertilizacion), se = FALSE) +labs(title = "CRA por riego y por fertilizacion", x = "riego",    y = "CRA(%)") + theme_bw()

# Las curvas de respuesta del CRA vs distintos riegos a distintas fertilizaciones tiene la misma tendencia salvo que esta desplazada hacia arriba. Es decir, en todo momento parecria que las plantas con fertilizacion tienen mayor cra promedio que las no fertilizadas (no lo sabemos con significacion todavia). Esto pareceria indicar que el riego y la fertilizacion no presentan interaccion, sino que sus efectos resulta aditivo
# Sin embargo, esto lo vamos a testear en el modelo

modelo<-lm(cra~fertilizacion*riego, datos)


#Supuestos
#### Analisis de los supuestos 

# calculo de los residuos

e<-resid(modelo) # residuos
re<-rstandard(modelo) #residuos estandarizados
pre<-predict(modelo) #predichos

# Graficos diagnosticos
par(mfrow = c(1, 2))
# Residuos est vs valores predichos
plot(pre, re, xlab="Predichos", ylab="Res estandarizados",main="Res Estand vs PRED" )
abline(0,0)

# qqplot 
qqPlot(e, main = "QQ Plot residuos")

# Pruebas estadisticas

# prueba de shapiro
shapiro.test(e) # no rechazo
# Levene
leveneTest(cra~fertilizacion*riego, datos) #no rechazo

# SE CUMPLEN LOS SUPUESTOS

#6. Ponga a prueba las hipótesis adecuadas en relación a los objetivos del ensayo. Informe la magnitud del efecto. ¿Con algún tratamiento cree que se lograron condiciones que favorezcan la adaptación al estrés hídrico? ¿Recomendaría una fertilización con K para mitigar los efectos del estrés hídrico?

anova(modelo)
# No hay  interaccion ni efecto de la fertilizacion. Si hay efecto del riego pero no sabemos cual nivel.

# Hay que hacer comparaciones
library(emmeans)
options(emmeans= list(emmeans = list(infer = c(TRUE, F)),
                      contrast = list(infer = c(TRUE, TRUE))))
## Comparaciones

compsimples<-emmeans(modelo, pairwise ~ riego)
summary(compsimples)

plot(compsimples, comparisons = T)

# los unicos riegos que difieren son los mas extremos: el riego cada 6 dias y el riego cada un dia. Luego el riego intermedio no difiere significativamente de los otros dos.

# La magnitud de efecto maxima se ve entre el riego1 y el riego6. El riego6 aumenta el cra medio con respecto al riego 1 en 17.8 %

# 7. Escriba el apartado de Metodología, Resultados y acompañe con una figuraconvenientemente rotulada.
#### Grafico de valores predichos por el modelo (hay más opciones graficas...)
model_plot <-ggpredict(modelo, 
                       terms = c("fertilizacion", "riego"),
                       interval = "confidence")   
model_plot
grafico <- plot(model_plot, add.data = F) # para agregar los puntos observados add.data = T
grafico + ggtitle("valores predichos") + labs(y="CRA(%)", colour="riego")
#8. Analice los datos considerando a la variable frecuencia de riego como cuantitativa (1, 3 y 6 días). Plantee el nuevo modelo. ¿Esperaría los mismos resultados que en el abordaje analítico anterior? Ajuste este nuevo modelo y concluya.

datos$riego<-as.integer(datos$riego)
str(datos)

modelo2<-lm(cra~fertilizacion*riego, datos)
anova(modelo2)
anova(modelo)
# Mismo resultado, variaciones en el p valor
