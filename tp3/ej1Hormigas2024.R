
###################################################
## Problema 1.                                    #
## Cebos alternativos para el control de hormigas #
###################################################
#estudiar aspectos comportamentales de la ingestión de cebos que expliquen la menor efectividad de los cebos comerciales en el control de hormigas con respecto a cebos alternativos preparados en laboratorio

rm(list=ls())

#############################
# Librerias #
#############################

library(car)
library(dplyr)
library(ggplot2)
library(gridExtra) 
library(emmeans)
library(nlme) # "Nonlinear Mixed-Effects Models"

### Carga de datos y estadistica descriptiva. ----------------------------------

## Carga de datos.

setwd("/cloud/project") # Configurar directorio de trabajo.

Datos <- read.delim("Hormigas.txt") # Cargar datos.

# opcional: puede optar por configurar que se carguen los strings como factores:
Datos <- read.delim("Hormigas.txt", stringsAsFactors = T) # Cargar datos. # carga strings como factor


#############################
# Inspeccion del data.frmae #
#############################

View(Datos)  # Visualizar Base de datos en pestaña nueva


str(Datos)  # Observar estructura de la base de datos.


names(Datos) # Visualizar los nombres de la variables.


# Para ver el tipo de la variables individualmente.

class(Datos$Tratamiento)
class(Datos$T_ing) 
class(Datos$MTox_ing)

#############################
# Analisis exploratorio     #
#---------------------------#
#############################

# Parte A: Analiza la masa de cebo toxico consumido por las hormigas en cada uno
# de los tratamientos.

#hacer tarea

# Parte B: Analiza el "t ing" en cada uno de los tratamientos (tiempo de ingestion).

# Estadistica descriptiva 

summary(Datos)

library(dplyr)
resumen <- Datos %>% 
  group_by(Tratamiento) %>%
  summarise_all(.funs = c(
    media = mean, 
    DE = sd, 
    min = min, 
    max = max
  )
  )

resumen$T_ing_media

resument <- as_tibble(cbind(nms = names(resumen), t(resumen)))
print(resument, n=30)

?summarise_all
# Realizar algunos graficos exploratorios.
plot(Datos$Tratamiento,Datos$T_ing)

ggplot(data = Datos, aes(x=Tratamiento, y=T_ing))+geom_boxplot(mapping = ,color="darkviolet", outlier.colour = "red", outlier.size = 4)+
  theme_bw()+labs(x="Tratamiento",y="Tiempo de ingesta")

# El tiempo de ingesta es menor en el tratamiento SAC30

class(Datos$Tratamiento)
#Datos$Tratamiento <- as.factor(Datos$Tratamiento) # si "tratamiento" esta como character, el gráfico no la reconocerá

# Con R base

plot(Datos$MTox_ing ~ Datos$Tratamiento,
     main = "Masa de toxico ingerida en funcion del tratamiento",
     ylab = "Masa de toxico ingerida",
     xlab = "Tipos de toxicos")

# La masa de toxico por lo pronto pareceria ser menor en el cebo comercial y no pareceria haber diferencias entre SAC30 y SAC68, sin embargo habria que ver si la diferencias son significativas

plot(Datos$T_ing ~ Datos$Tratamiento,
     main = "Tiempo de ingesta de toxico en funcion del tratamiento",
     ylab = "Tiempo de ingesta",
     xlab = "Tipos de toxicos")
# Este grafico ya lo hicimos arriba pero con ggplot


## Con ggplot 2
library(ggplot2)
library(gridExtra) 

# Realice dos graficos: "MTox_ing" y "T_ing",  discriminado por tratamiento
# Coloque ambos graficos en una misma grilla

graficoMTox_ing <- ggplot(Datos, aes(x=Tratamiento, y=MTox_ing)) +
        geom_boxplot(aes(color=Tratamiento), color="black") +        # outlier.shape = NA
        theme_bw()+
        geom_jitter(alpha=0.3, size=2,aes(color=Tratamiento), 
                    position = position_jitter(width = .2))+
        theme(legend.position="top", 
              legend.text=element_text(size = 12),
              legend.title = element_text(size=12, face="bold")) +
        ylab("Masa toxico ingerida")+xlab("Tratamiento") 
        
graficoMTox_ing 

graficoT_ing <- ggplot(Datos, aes(x=Tratamiento, y=T_ing)) +
  geom_boxplot(aes(color=Tratamiento), color="black", outlier.shape = NA) +        # outlier.shape = NA
  theme_bw()+
  geom_jitter(alpha=0.3, size=2,aes(color=Tratamiento), 
              position = position_jitter(width = .2))+
  theme(legend.position="top", 
        legend.text=element_text(size = 12),
        legend.title = element_text(size=12, face="bold")) +
  ylab("Tiempo de ingesta")+xlab("Tratamiento") 

graficoT_ing

## Nota: para remover el punto del dato 'outlier' del grafico (para que no aparezca doble),
#        agregar el argumento outlier.shape = NA en geom_boxplot


# Arreglo con los graficos construidos 
# (reemplace por los nombres de los graficos)
grid.arrange(graficoMTox_ing, graficoT_ing,
             ncol=2, nrow=1)
# SOSPECHAMOS QUE PUEDE QUE NO SE CUMPLA HOMOCEDASTICIDAD EN TIEMPO DE INGESTA--- A MAYOR MEDIA MAYOR VARIANZA! 

# EN MASA DE TOXICO INGERIDA NO PARECE HABER DIFERENCIAS EN LA VARIANZA

#################################################################
## PARTE A                                                      #
## La masa de toxico ingerida, difiere entre los tratamientos? # 
#---------------------------------------------------------------#
#################################################################


### Evaluacion de supuestos. ---------------------------------------------------

# Para evaluar los supuestos primero tenemos que ajustar el modelo. Esto permite calcular los residuos del modelo.

modelo_0 <- lm(MTox_ing ~ Tratamiento, data = Datos) # Generar modelo.

# Calcular los residuos y los predichos del modelo.

e <- residuals(modelo_0) # Residuos.
re <- rstandard(modelo_0) # Residuos estandarizados.
pre <- predict(modelo_0) # Predichos.

# Unir todo en un solo objeto.

res <- data.frame(Datos$Tratamiento, Datos$MTox_ing, pre, e, round(re, 2))

# Darle nombre a las columnas.

colnames(res) <- c("Tratamiento", "MTox_ing", "Predichos", "Residuos",
                   "Residuos std") 

# Observar el contenido del data frame.

head(res)

class(res) # Es un data frame.

## Metodos graficos.

# Grafico de dispersion residuos v. predichos y QQplot.

par(mfrow = c(1, 2))
# Grafico de dispersion.
plot(x = pre,
     y = re,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED" )

abline(h = 0, lty = 2) # Agregar linea horizontal y = 0.

# QQplot.

# libreria car:
qqPlot(e, main = "QQ Plot residuos")

par(mfrow = c(1, 1)) # Restaura a un grafico por pantalla.

# con {stats}:

qqnorm(resid(modelo_0), main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(resid(modelo_0))



## Metodos analiticos.

# Prueba analitica para la homogeneidad de varianzas

#library("car")

leveneTest(Datos$MTox_ing, Datos$Tratamiento) #0.33

# Cual es la Ho de la prueba de Levene?
# La h0 de Levene es que las varianzas de masa de toxico ingerida entre los tratamientos son iguales

#No tengo evidencia para rechazar, no hay evidencia de heterocedasticidad

# Prueba analitica para normalidad 
shapiro.test(e) # 0.81

# No tengo evidencias para rechazar la h0, no hay evidencias de que no se cumpla la normalidad
#  Cual es la Ho de la prueba de shapiro?
# La h0 de shapiro es que la masa de toxico ingerida se distribuye de manera normal para cada uno de los tiempos evaluados??

### Resultados e interpretacion del modelo. -----------------------------------------------------

## Resultados generales.

# anova() y summary()

modelo_0

anova(modelo_0)

summary(modelo_0)

# Que informacion ofrece cada una de estas salidas del modelo ajustado?
#Tanto modelo_0 como summary me muetran los estimadores del modelo. Summary ademas me muestra el error estandar y la varianza estimada del modelo, ademas de los grados de libertad.
# Anova me muetsra la significancia de la pruebas de hipotesis. Vemos que por lo menos algun tratamiento difiere del resto, al estimar su media da significativamente distinta, sin embargo no sabemos cual. Aca tambien vemos la varianza del modelo y los grados de libertad

7.825**2 #61.23063 (sigma dado por summary)
61.23 # Mean Sq., sigma cuadrado dado por anova

## Comparaciones multiples.
# Ahora que sabemos que por lo menos algun tratamiento difiere, vamos a buscar cual/es

library(emmeans)
options(emmeans= list(emmeans = list(infer = c(TRUE, FALSE)),contrast = list(infer = c(TRUE, TRUE))))

comp <- emmeans(modelo_0,pairwise ~ Tratamiento)
comp

plot(comp$emmeans, comparisons = TRUE)
# Vemos que la masa de toxico ingerida no difiere para las hormigas tratadas con  SAC30 de las de SAC68. En cambio la masa de toxico ingerida es menor para el tratamiento de cebo comercial. Es decir, el cebo utilizado importa en la masa de toxico ingerida si es comercial o no pero no difiere para los no-comerciales

## Armar un grafico para presentar los resultados (puede utilizar ggeffect o extraer las medias predichas como se hizo en el problema de Cadmio del TP 1, cuando se presentaron los resultados del modelo Cadmio como factor)
install.packages("ggeffects")
library(ggeffects)
model_plot0 <-ggpredict(modelo_0, 
                       terms = c("Tratamiento"),
                       interval = "confidence")   
model_plot0
grafico <- plot(model_plot0, add.data = F, color="darkcyan") # para agregar los puntos observados add.data = T

grafico + ggtitle("valores predichos") + labs(y="Masa de toxico ingerida") +
  annotate("text", x = c(1,2,3), y=c(5.3,6.3,7), label = c("A", "B", "B")) + geom_jitter(data = Datos, aes(x = as.numeric(Tratamiento), y = MTox_ing), width = 0.05, height = 0,color="darkcyan", size = 1.5, alpha = 0.6)



###################################
## PARTE B                        #
## Modelado del tiempo de ingesta # 
#---------------------------------#
###################################
#1. ¿Cuál es la variable respuesta? ¿Cuál es la variable explicativa? ¿Cuál podría ser la potencial distribución de probabilidades de la variable respuesta?

#VR : Tiempo de ingesta, es cuantitativa continua la asumimos normal
#VE: Tratamiento
## Grafico de relacion entre varianzas y valores esperados.

# Calcular medias y varianzas, y graficar
# con plot()
media <- matrix(tapply(Datos$T_ing, Datos$Tratamiento, mean))
varianza <- matrix(tapply(Datos$T_ing, Datos$Tratamiento, var))
sd  <- matrix(tapply(Datos$T_ing, Datos$Tratamiento, sd))

# Con ggplot (armo previamente un data frame con medias-varianzas)
DF <- as.data.frame(media) # DF es el nombre del data.frame. Agrego la media
colnames(DF) <- c("media")
DF$sd <- round(sd,1)             # Agrego sd a DF, y redondeo a 1 decimal
DF$varianza <- round(varianza,1) # Agrego varianza a DF, yredondeo a 1 decimal
DF                               # Exploro el data.frame
# Genero y agrago la columna Tratamiento (en el orden que corresponde)
Tratamiento <- c("CC", "SAC30", "SAC68")  
DF$Tratamiento <- Tratamiento
DF                               # Exploro el data.frame


plot <- ggplot(DF, aes(x=media, y=varianza)) + 
  theme_bw() +
  geom_point(size=6, shape=1) +
  annotate("text", x=70,  y=2000,   label= "SAC30", size=6) +
  annotate("text", x=350, y=21000, label= "SAC68", size=6) +
  annotate("text", x=400, y=18000, label= "CC", size=6)  
plot + theme(axis.text.x = element_text(size=14),
               axis.text.y = element_text(size=14), 
               axis.title.x = element_text(size = 16), 
               axis.title.y = element_text(size = 16))

# Hecho por mi
ggplot(DF,aes(x=media, y=varianza, color="maroon"))+geom_point(size=4)+annotate("text", x=70,  y=2000,   label= "SAC30", size=4) +
  annotate("text", x=350, y=21000, label= "SAC68", size=4) +
  annotate("text", x=400, y=18000, label= "CC", size=4) 

################################
# Ajuste del modelo en R #
#------------------------------#
################################

### Evaluacion de supuestos. ---------------------------------------------------

# A partir del analisis exploratorio discuta si el supuesto de homogeneidad de varianzas se sostiene para estos datos. 

# Ya de entrada vemos que no

## Evaluacion de los supuestos. 

# Obtener modelo, predichos y residuos.

modelo_1 <- lm(T_ing ~ Tratamiento, data = Datos)

# Graficamente.

e <- residuals(modelo_1)
re <- rstandard(modelo_1)
pre <- predict(modelo_1)

par(mfrow = c(1, 2))

# Grafico de dispersion.

plot(x = pre,
     y = re,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion 
     de RE vs PRED")

abline(h = 0)

# QQplot.
qqPlot(e, main = "QQ Plot residuos")

par(mfrow = c(1, 1))

# a medida que aumenta la media, aumenta la dispersion 
# En RE vs PRED vemos que no se cumple la homocedasticidad, se ve ua forma de cono. En el QQplot vemos que no se cumple la normalidad

# Analiticamente.
# Normalidad (Prueba de Shapiro)
shapiro.test(modelo_1$residuals) #0.0008822

# Homocedasticidad (Prueba de Levene)
leveneTest(Datos$T_ing, Datos$Tratamiento) #8.19e-05

### RECHAZO AMBOS
# Tengo evidencias de heterocedasticidad y falta de normalidad

### Modelado con minimos cuadrados generalizados. ------------------------------

library(nlme) # incluye funcion gls().

# Con funcion gls() pero SIN modelar varianza

modelo_2 <- gls(T_ing ~ Tratamiento, data = Datos) 

# Estudiar los supuestos. Es el mismo modelo que antes, no se van a cumplir

# Se puede utilizar la funcion plot() en los objetos que genera la funcion gls()
# para graficar. Indique el tipo de grafico que se obtiene.

plot(modelo_2) # GRAFICO DE DISPERSION DE PRED VS RESIDUOS ESTANDARIZADOS. Mismo cono!

# Alternativamente, Calcular de los residuos de pearson.

r2 <- residuals(modelo_2, type = "pearson") # = estandarizados.

# Calcular los valores predichos por el modelo.

pred2 <- fitted(modelo_2)

# Graficar los residuos en funcion de los valores predichos.

plot(x = pred2,
     y = r2,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")

abline(h = 0, lty = 2)
#Por 3era vez hicimos el mismo grafico para el mismo modelo. Yay

# Graficar un boxplot de los residuos del modelo. Este grafico esta bueno

boxplot(r2 ~ Datos$Tratamiento,
        xlab = "Tratamiento",
        ylab = "Residuos estandarizados")

# VEMOS QUE LOS RESIUDOS SON VARIABLES ENTRE LOS TRATAMIENTOS

# Graficar un QQplot.
qqPlot(r2, main = "QQ Plot residuos estandarizados")
# sin car:
qqnorm(modelo_2, abline = c(0,1)) #QQ plot

# LA NORMAlidad esta fea fea

# Prueba de Levene.
leveneTest(r2, Datos$Tratamiento) # rechazo 8.19e-05
shapiro.test(r2) #0.0008822
# Es la misma prueba que hciimos antes pero con gls y (tiene sentido) obtuvimos los mismos pvalores

# Analizar los resultados de los graficos diagnosticos
# Analizar los resultados de la prueba de Shapiro y Levene
# Explicar los resultados en terminos del problema.
# Es exactamente lo mismo que lo que ya discutiumos para lm()

################################
### Modelado de varianzas.     #
#   ---------------------      #
################################


## Modelos gls() modelando varianza

## ################################
## Modelo 3: incorpora esttructura VarIdent(Tratamiento)#
## para modelar varianza ##
###################################    


modelo_varIdent <- gls(T_ing ~ Tratamiento,
                       weights = varIdent(form = ~1 | Tratamiento),
                       data = Datos)

# Evaluar supuestos.

par(mfrow=c(1,3))

r3 <- residuals(modelo_varIdent, type="pearson")
pred3 <- fitted(modelo_varIdent)

plot(x = pred3,
     y = r3,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion 
     de RE vs PRED")

abline(h = 0, lty = 2)

# Graficar un boxplot de los residuos del modelo

boxplot(r3 ~ Datos$Tratamiento,
        xlab = "Tratamiento",
        ylab = "Residuos estandarizados")

# Graficar un qqplot.

qqPlot(r3, main = "QQ Plot residuos estandarizados")

# Prueba de Levene.

leveneTest(r3, Datos$Tratamiento, center = "median") # 0.9395
shapiro.test(r3) # 0.3597
# se cumplen los supuestos
## ################################
## Modelo 4: "varPower"           #
###################################    

modelo_varPower <- gls(T_ing ~ Tratamiento,
                       weights = varPower(),
                       data = Datos)

# Supuestos.

r4 <- residuals(modelo_varPower, type = "pearson")
pred4 <- fitted(modelo_varPower)
plot(x = pred4,
     y = r4,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion 
     de RE vs PRED")

abline(h = 0)

boxplot(r4 ~ Datos$Tratamiento,
        xlab = "Tratamiento",
        ylab = "Residuos estandarizados")

qqPlot(r4, main = "QQ Plot residuos estandarizados")

leveneTest(r4, Datos$Tratamiento, center = "median") #0.7236
shapiro.test(r4) #0.4183
# se cumplen los supuestos

###################################
##  Modelo 5: "varExp".           #
###################################    

modelo_varExp <- gls(T_ing ~ Tratamiento,
                     weights = varExp(),
                     data = Datos)

# Supuestos.

r5 <- residuals(modelo_varExp, type = "pearson")
pred5 <- fitted(modelo_varExp)

plot(x = pred5,
     y = r5,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion 
     de RE vs PRED")

abline(h = 0)

boxplot(r5 ~ Datos$Tratamiento,
        xlab = "Tratamiento",
        ylab = "Residuos estandarizados")

qqPlot(r5, main = "QQ Plot residuos estandarizados")

leveneTest(r5, Datos$Tratamiento, center = "median") #0.3538
shapiro.test(r5) #0.4032

## En base al análisis, cuáles son modelos candidatos? 
# los tres!! var ident, var power y var exp porque todos cumplen los supuestos ahora

###################################
##  Seleccion de modelos.         #
###################################  

AIC(modelo_varIdent, modelo_varPower, modelo_varExp)

# el mejor modelo es el varpower!! Tiene el MENOR AIC


######################################
## Interpretacion del modelo elegido.#  
######################################  

# Presentamos Modelo 4: "varPower" (pudo haber elegido otro?) Podriamos haber elegido otro, el AIC varia por reee poco

modelo_varPower
summary(modelo_varPower)
anova(modelo_varPower)

# Indique si se rechaza la H0 del AnOVa.# siii
#  Hay efecto del tratamiento sobre el tiempo de ingesta

# Interprete los parametros del modelado de varianza.

# Comparaciones a posteriori.

library(emmeans)
options(emmeans= list(emmeans = list(infer = c(TRUE, F)),
                      contrast = list(infer = c(TRUE, TRUE)))) # opciones de seteo de las opciones de salida


comp <- emmeans(modelo_varPower, pairwise ~ Tratamiento)

comp


plot(comp$emmeans, comparisons = TRUE)

# El timepo de ingesta es significativamente menor para el tratamiento SAC30

# extraemos las medidas resumen 

resumen_modelo <-as.data.frame(comp$emmeans)

# exploramos el objeto resumen_modelo
resumen_modelo  # emmeans es la media estimada

# Plot
library(ggplot2)
ggplot(resumen_modelo, aes(x=Tratamiento, y=emmean)) +
  labs(x="Tratamiento") + labs(y="Tiempo ingesta [seg]") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), color="blue", width=0.2)+
  geom_point(shape=1, size=2, color="blue") +
  ggtitle("Comparaciones", "Media ? Error est?ndar") +
  annotate("text", 
           x = c("CC","SAC30","SAC68"), 
           y = c(650,650,650), 
           label = c("B", "A", "B"))



### FIN ###

######################################
#  Y si elegian VARIDENT?            #  
######################################  

modelo_varIdent
anova(modelo_varIdent)
summary(modelo_varIdent)

# interprestacion de varident: estima un devio estandar para el tratamiento de refenrencia y las proporciones del resto

