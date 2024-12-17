###############################
# Biometria 2 - 2024          #
#       TP 8                  #
#      Problema 1             #
###############################


# Veamos si hay algun comando u objeto almacenado en la memoria
ls()
# Si te aparece character(0) es que la memoria esta limpia, si no ejecuta
# la siguiente sentencia para borrar la memoria:
rm(list=ls())

# revisamos...
ls()

#### Cargamos las librerias que vamos a utilizar

library(ggplot2)     # La vamos a utilizar para graficas
library(dplyr)       # La vamos a utilizar para armar tablas resumen
library(car)         # La vamos a utilizar para analisis de algunos supuestos 
library(glmmTMB)     # La vamos a utilizar para ajustar el modelo 
library(DHARMa)      # La vamos a utilizar para analizar supuestos
library(performance) # La vamos a utilizar para analizar supuestos y más ...
library(emmeans)     # La vamos a utilizar para realizar comparaciones 
library(ggeffects)   # La vamos a utilizar para obtener (y graficar) los valores predichos por el modelo
library(sjPlot)      # Alternativa para ggeffcets y grafico de efectos aleatorios

# setear el directorio de trabajo correspondiente

# setwd("ruta de acceso")
setwd("~/Carrera/Biometria II/tp8")


#### Cargar el data.frame en el entorno de trabajo (Environment)

Datos  <- read.csv("aves.csv", stringsAsFactors = T)

#### Exploramos la estructura del data.frame (dimensiones, tipo de variables, datos faltantes, etc.)

class(Datos)     # tipo de objeto Datos
str(Datos)       # estructura
dim(Datos)       # dimensiones
head(Datos)      # primeras 6 filas (se pueden pedir mas filas si lo desean)
summary(Datos)   # resuemn de las variables

Datos$Parcela <- as.factor(Datos$Parcela) #¿por qué pasamos únicamente a parcela como factor? Porque tecnicamente es cualitativa, la parcela 16 no es 16 veces la parcela 1

#### Analisis exploratorio (descriptivo)

## Tabla resumen (replicas, tendencia central y dispersion)

# Agrupando las parcelas
descAgrupParcelas <- Datos %>% 
  group_by(Zona, Uso) %>%
  summarise_at("Abundancia", c(
    n = length, 
    media_aves = mean, 
    sd_aves = sd)) 

descAgrupParcelas
# Discriminado por parcela
descPorParcela <- Datos %>% 
  group_by(Zona, Uso, Parcela) %>%
  summarise_at("Abundancia", c(
    n = length, 
    media_aves = mean, 
    sd_aves = sd))  %>% 
  arrange(Parcela)

descPorParcela


## Gráficos
# descriptiva - gráficos

# todas las observaciones ... 
ggplot(data = Datos, aes(x = Zona, y = Abundancia, fill=Uso)) +
  geom_boxplot() +
  ylab("Abundancia aves nativas (n°/5 min)") +
  geom_point(alpha=0.3, position=position_jitterdodge()) # dogde desplaza horizontalmente los puntos, asi evitamos solapamiento y ayuda a la visualización

# con los promedios de las parcelas 
ggplot(data = descPorParcela , aes(x = Uso, y = media_aves, fill=Zona)) +
  geom_point(aes(colour=Parcela), alpha=0.5) + # se puede agregar el jitter 
  ylab("Abundancia aves nativas (n°/5 min)") +
  facet_grid(. ~Zona) 

# pueden pensar más opciones gráficas


#### Ajuste del modelo - poisson

## modelando la abundancia con GLMM
m1 <- glmmTMB(Abundancia ~  Uso*Zona + (1|Parcela),
            data =Datos, family = poisson)

#### Supuestos!

# parametro de dispersion
e1 <- resid(m1, type = "pearson")
dispersion <- sum(e1^2) / df.residual(m1)
dispersion # 0.59

# subdispersion? 

# analisis supuestos con Dharma
simm1 <- simulateResiduals(fittedMod = m1) # , refit = T 
plot(simm1) # uff no se cumplen los supuestos
plotResiduals(simm1, Datos$Uso)
plotResiduals(simm1, Datos$Zona)

testDispersion(simm1)
# 
# DHARMa nonparametric dispersion test via sd of residuals fitted vs. simulated
# 
# data:  simulationOutput
# dispersion = 0.68484, p-value = 0.008
# alternative hypothesis: two.sided

# El parámetro de dispersión del modelo es 0.5862931 , y en base a su valor y la prueba de hipótesis del
# paquete DHARMA, se diagnostica subdispersión.


# supuestos del componente aleatorio de parcelas

alfai <- ranef(m1)

### QQ plot 
car::qqPlot(alfai$cond$Parcela$`(Intercept)`)

### prueba de shapiro
shapiro.test(alfai$cond$Parcela$`(Intercept)`)



# m1 --> diagnosticamos SUB dispersion


#### Ajuste del modelo "compois" (Conway-Maxwell Poisson distribution)

m2_cmp <- glmmTMB(Abundancia ~  Uso*Zona + (1|Parcela), data = Datos, family=compois)

# tarda en correr

#### Supuestos!

# parametro de dispersion
e1 <- resid(m2_cmp, type = "pearson")
dispersion <- sum(e1^2) / df.residual(m2_cmp)
dispersion

# analisis supuestos con Dharma
simCMP <- simulateResiduals(fittedMod = m2_cmp) # , refit = T 
plot(simCMP)

testDispersion(simCMP)
# dispersion = 1.0987, p-value = 0.432
plotResiduals(simCMP, Datos$Uso)
plotResiduals(simCMP, Datos$Zona)


# supuestos del componente aleatorio de parcelas

alfai <- ranef(m2_cmp)

### QQ plot 
car::qqPlot(alfai$cond$Parcela$`(Intercept)`)
# alternativa grafica
library(sjPlot)
plot_model(m2_cmp, type = "diag") 

### prueba de shapiro
shapiro.test(alfai$cond$Parcela$`(Intercept)`)
#W = 0.93053, p-value = 0.2486


# Analizamos los resultados del modelo CMP

# termino de interaccion: 
drop1(m2_cmp,test="Chi") # da signifiicativo

summary(m2_cmp)

# efectos aleatorios
(alfai<-ranef(m2_cmp)) # escala PL
exp(alfai$cond$Parcela) # escala VR


# ¿comparaciones múltiples o efectos simples? 

emm_options(emmeans = list(infer = c(TRUE, FALSE)),
             contrast = list(infer = c(TRUE, TRUE)))

# Dada las preguntas de investigación del problema, podemos hacer contrastes planeados de efectos simples: evaluar el efecto de un factor (uso) en cada nivel del otro factor (Zona)

# Escala del Predictor lineal
efsimpleZona<- emmeans(m2_cmp, pairwise ~ Uso | Zona) #Tukey por default
plot(efsimpleZona$emmeans, comparisons = TRUE)
efsimpleZona

# Escala de la variable respuesta
efsimpleZona<- emmeans(m2_cmp, pairwise ~ Uso | Zona, type="response") #Tukey por default
plot(efsimpleZona$emmeans, comparisons = TRUE)
efsimpleZona

# La abundancia de aves obsevradas en 5 minutos por parcela en zona colon en eucalipto es 0.63 veces la abundancia en soja. En otras palabras, es 37% menor

# La abundancia de aves obs en 5 mins por parcela en zona hernan en eucalipto es 1.3 veces la abundancia en soja. Es decir, es 30% mayor

#Se espera que en Colón (zona de pastizal), la abundancia de aves nativas bajo un cultivo de eucalipto disminuya en promedio entre un 20% y un 50% respecto a un cultivo de soja ***para una parcela típica*** .

#En cambio, en Hernandarias (zona de selva), se espera que la abundancia de aves nativas bajo cultivo de eucalipto aumente en promedio entre un 5% y un 62% respecto a un cultivo de soja en una parcela típica; ambos intervalos con una confianza del 95%


# Grafico final Modelo predicho

## modelo final resumen (tabla y grafico predictivo)
# con ggeffects
library(ggeffects)
a <- ggpredict(m2_cmp, terms= c("Zona","Uso")) 
a
plot(a, add.data=F, grid = F )

# En Colón (pastizal), se registró en promedio mayor abundancia de aves en plantación de soja respecto a la de eucalyptus, mientras que en la zona de Hernandarias (selva) se registró en promedio mayor abundancia en eucalyptus respecto a soja. Los resultados fueron obtenidos a partir del ajuste de un modelo lineal generalizado mixto con distribución "compois" para contemplar la subdispersión obtenida en el modelo poisson. 


#Estos resultados aportan evidencia a favor de que el impacto del monocutivo sobre la abundancia de aves típicas de una región es mayor cuanto mayor es la discrepancia en la estructura vertical de la vegetación entre el ambiente original y el monocultivo. Se menciona que el ensayo no tiene verdaderas réplicas de pastizal y selva, pero constituye una primera aproximación


# Podemos graficar los "re"


# Grafico escala exponencial 
plot_model(m2_cmp,
           type="re", 
           vline.color = "grey4",
           show.values = TRUE,
           sort.est = F,
           grid = F,
           value.offset = .3, 
           title = "ranef (exponencial)") 

alfai <- ranef(m2_cmp)
exp(alfai$cond$Parcela$`(Intercept)`) # escala exp
sort(exp(alfai$cond$Parcela$`(Intercept)`))

# estimados e IC
get_model_data(m2_cmp, type = "re")

# escala del PL
plot_model(m2_cmp,
           type="re", 
           vline.color = "grey4",
           show.values = TRUE,
           sort.est = T,
           grid = F,
           value.offset = .3, 
           transform = NULL,
           title = "ranef") 

alfai <- ranef(m2_cmp)
alfai$cond$Parcela$`(Intercept)`
