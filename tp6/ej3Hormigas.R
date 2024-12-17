# Problema 3. Cebos alternativos más eficientes para el control de hormigas

rm(list=ls())
library(ggplot2)     # La vamos a utilizar para graficas
library(dplyr)       # La vamos a utilizar para armar tablas resumen
library(car)         # La vamos a utilizar para analisis de algunos supuestos 
library(glmmTMB)     # La vamos a utilizar para ajustar el modelo 
library(DHARMa)      # La vamos a utilizar para analizar supuestos
library(performance) # La vamos a utilizar para analizar supuestos y más ...
library(emmeans)     # La vamos a utilizar para realizar comparaciones 
library(ggeffects)   # La vamos a utilizar para obtener (y graficar) los valores predichos por el modelo
library(sjPlot)      # Alternativa para ggeffcets y grafico de efectos aleatorios

setwd("~/Carrera/Biometria II/tp8")
datos <- read.delim("Hormigas.txt", stringsAsFactors = T)
str(datos)
summary(datos)
head(datos, 10)

#a) ¿Identifica alguna variable predictora aleatoria que podría ser tomada en cuenta al estudiar los efectos de los tratamientos sobre el número de pausas durante la ingesta? ¿De qué tipo de diseño experimental se trata y por qué cree que se llevó a cabo de esa manera?

# Debido a que el experimento tomo 5 dias, se podria incluir el efecto aleatorio del dia para absorber la variabilidad entre dias.
# Yo creo que se hizo asi para minimizar los costos y el esfuerzo de capturar hormigas y controlar las variables necesarias.
# Es un diseño completamente aleatorizado de medidas repetidas.

#b) Escriba el modelo correspondiente incluyendo la VE aleatoria en el modelo. ¿Cuál es la distribución de probabilidad potencial de la variable respuesta?

# N_i = mu + alfa_i + D_ik
# donde i va del i al 3, indicando cada tratamiento
# k del 1 al 5 indicando el dia



# c) Ajuste el modelo que considere adecuado. Evalúe los supuestos
m1 <-  glmmTMB(Nro_pau ~  Tratamiento + (1|Dia),
               data =datos, family = poisson)

#### Supuestos!

# parametro de dispersion
e1 <- resid(m1, type = "pearson")
dispersion <- sum(e1^2) / df.residual(m1)
dispersion # 2.31
# analisis supuestos con Dharma
simm1 <- simulateResiduals(fittedMod = m1) # , refit = T 
plot(simm1) # uff no se cumplen los supuestos
testDispersion(simm1)

# Hay evidencias de sobredispersion

# supuestos del componente aleatorio de parcelas
alfai <- ranef(m1)
### QQ plot 
car::qqPlot(alfai$cond$Dia$`(Intercept)`)
### prueba de shapiro
shapiro.test(alfai$cond$Dia$`(Intercept)`) # 0.82

# d) Estime los componentes fijos y aleatorios del modelo y discuta la importancia de la variabilidad aportada por la VE aleatoria.

drop1(m1, test="Chisq") # *** Tratamiento, alguna de las medias difiere!
summary(m1)
# La variabilidad de la variable aleatoria Dia es 0.01849, es muy baja


# Pruebo otros modelos
# Binomial negativa
# Se comienza ajustando un modelo nulo (sin variables predictoras)
m2 <- glmmTMB(Nro_pau ~  Tratamiento + (1|Dia), data = datos, family = nbinom2)

sum(resid(m2, type="pearson")^2)/(df.residual(m2)) # da re cercano a 1

# Simulaciones Dharma
simNB <- simulateResiduals(m2, n = 10000)
plot(simNB)
hist(simNB)

# prueba de dispersion DHARMa
testDispersion(simNB, plot=F) # no rechazo

# Dispersion con performance
check_overdispersion(m2) #No overdispersion detected.
# supuestos del componente aleatorio de parcelas

alfai <- ranef(m2)

### QQ plot 
car::qqPlot(alfai$cond$Dia$`(Intercept)`)
### prueba de shapiro
shapiro.test(alfai$cond$Dia$`(Intercept)`)

#### Ajuste del modelo "compois" (Conway-Maxwell Poisson distribution)

m3 <- glmmTMB(Nro_pau ~  Tratamiento + (1|Dia), data = datos, family=compois)

# tarda en correr

#### Supuestos!

# parametro de dispersion
e1 <- resid(m3, type = "pearson")
dispersion <- sum(e1^2) / df.residual(m3)
dispersion #1.07

# analisis supuestos con Dharma
simCMP <- simulateResiduals(fittedMod = m3) # , refit = T 
plot(simCMP)

testDispersion(simCMP)
# dispersion = 1.0987, p-value = 0.432

# supuestos del componente aleatorio de parcelas

alfai <- ranef(m3)

### QQ plot 
car::qqPlot(alfai$cond$Dia$`(Intercept)`)
### prueba de shapiro
shapiro.test(alfai$cond$Dia$`(Intercept)`)

# Analizamos los resultados del modelo CMP

# termino de interaccion: 
drop1(m3,test="Chi") # da signifiicativo

m4 <- glmmTMB(Nro_pau ~  Tratamiento + (1|Dia), family = tweedie, data = datos)
#Vemos los supuestos
#Simulaciones Dharma
sim6 <- simulateResiduals(m4, n = 1000)
plot(sim6) # Se desvia un solo cuantil

m5 <- glm(Nro_pau ~  Tratamiento + (1|Dia), family = "quasipoisson", data = datos)


AIC(m2,m3, m4, m5)

# Me quedo con m4 Tweedie

# e) De acuerdo al resultado del modelo ajustado, efectúe comparaciones o efectos simples que le permitan responder a la pregunta de investigación. Presente un gráfico final con los principales resultados. Estime la magnitud del (máximo) efecto con un nivel de confianza del 95%.

emm_options(emmeans = list(infer = c(TRUE, FALSE)),
            contrast = list(infer = c(TRUE, TRUE)))

# Escala del Predictor lineal
efsimpleZona<- emmeans(m4, pairwise ~ Tratamiento) #Tukey por default
plot(efsimpleZona$emmeans, comparisons = TRUE)
efsimpleZona

# Escala de la variable respuesta
efsimpleZona<- emmeans(m4, pairwise ~ Tratamiento, type="response") #Tukey por default
plot(efsimpleZona$emmeans, comparisons = TRUE)
efsimpleZona
