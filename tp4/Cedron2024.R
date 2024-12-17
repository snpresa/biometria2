
## Modelos multiples

## Problema: Regulación de la producción de aceites esenciales en cedrón (Aloysia citriodora)

# Veamos si hay algun comando u objeto almacenado en la memoria
ls()
# Si te aparece character(0) es que la memoria esta limpia, si no ejecuta
# la siguiente sentencia para borrar la memoria:
rm(list=ls())

# revisamos...
ls()

#### Cargamos las librerias que vamos a utilizar

library(ggplot2)   # La vamos a utilizar para graficas
library(dplyr)     # La vamos a utilizar para armar tablas resumen
library(car)       # La vamos a utilizar para analisis de algunos supuestos 
library(emmeans)   # La vamos a utilizar para realizar comparaciones 
library(ggeffects) # La vamos a utilizar en el ultimo item, para obtener (y graficar) los valores predichos por el modelo


#### setear el directorio de trabajo .... (cada uno tendra una ruta diferente)

# setwd("~ ")  #Comentario:  Luego del "~" escribir la ruta. 

setwd("/cloud/project")

#### Cargar el data.frame en el entorno de trabajo (Environment)


Datos  <- read.csv("CEDRON.csv", stringsAsFactors = T) 



#### Exploramos la estructura del data.frame (dimensiones, tipo de variables, datos faltantes, etc.)

class(Datos)     # tipo de objeto Datos
str(Datos)       # estructura
dim(Datos)       # dimensiones 30,4
head(Datos)      # primeras 6 filas (se pueden pedir mas filas si lo desean)
summary(Datos)   # resuemn de las variables
#maceta, provincia, densidad y aceite


levels(Datos$densidad) # chequeamos orden 1_planta, 5_plantas

# Opcional dar vuelta los niveles de densidad (puede resultar util sobre el final del ejercicio)
Datos$densidad <- factor(Datos$densidad, levels = c("5_plantas", "1_planta"))


#### Analisis exploratorio (descriptivo)

# Comentario: como para casi todo, existen muchas librerias y posibles formas de presntar los resultados. Aqui presentamos una opcion, pero no es la unica. 

## Tabla resumen (replicas, tendencia central y dispersion)
Datos %>% 
  group_by(densidad, provincia) %>%
  summarise_all(.funs = c(
    n = length, 
    media_aceite = mean, 
    sd_aceite = sd, 
    min_aceite = min, 
    max_aceite = max
  )
  )



## Grafico

#Recordar que siempre hay muchas opciones graficas ... 

(medias.Datos<-aggregate(aceite~densidad+provincia, Datos,mean)) # tabla de medias, tambien se puede extraer de la tabla resumen anterior

# Opcion grafica 1 (de muchas...) con densidad en eje x
#Exploratorios
g1 <- ggplot(medias.Datos, aes(x=densidad, y=aceite, colour=provincia, group=provincia))
g2 <- g1 + geom_line(aes(linetype=provincia), size=.6) +geom_point(aes(shape=provincia), size=3) 
g2

# no en todos los casos conviene plantas con densidad. Se sospecha interaccion
g3 <- g2 + geom_jitter(data = Datos, width = 0.05, height = 0) # es util visualizar los datos obtenidos, ademas de las tendencias
g3


#### Ajuste del modelo

modelo <- lm(aceite~densidad*provincia, data=Datos) 

# Ajustamos el modelo para calcular los residuos, que serán nuestros insumos para evaluar los supuestos del mismo

#### Analisis de los supuestos 

# calculo de los residuos

e<-resid(modelo) # residuos
re<-rstandard(modelo) #residuos estandarizados
pre<-predict(modelo) #predichos

# Tabla para visualizar los valores de los residuos y predichos por el modelo calculados

res<-cbind.data.frame(Datos$maceta, Datos$provincia, Datos$densidad, Datos$aceite,pre,e,round(re,3)) # cbind: "Combine R Objects by Columns"
colnames(res)<-c("maceta", "provincia", "densidad", "aceite", "Predichos", "Residuos", "residuos std") # agregamos los "Column Names"
res

#Supuestos

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
leveneTest(aceite~provincia*densidad, Datos) #no rechazo


#### Modelo: Resultados e interpretacion  

# Volvamos al modelo
#modelo <- lm(aceite~densidad*provincia, data=Datos) es lo mismo que corrimos arriba
modelo <- lm(aceite~densidad*provincia, data=Datos)  #alternativa=sumar todos los terminos y la interaccion expresarla con :

# test "global" 
anova(modelo)

# la rta de plantas de generar mas aceite depende de la interaccion genotipo-competencia
# el efecto de la copetencia depende del genotipo
#el modelo no es aditivo
#no miro efectos principales

#### Comparaciones  a posteriori

# Cuando tenemos interacciones involucradas en el modelo podemos estar en alguno de los siguientes casos:
# ejemplo de nuestro modelo (2 factores con interaccion):

# 1) interaccion NO SIG
# 2) interaccion SIG    

# Si ocurre 1) -> 
#  Efectos principales: se comparan los 3 genotipos entre si (sin discriminar la densidad) y los dos niveles de densidad (sin discriminar el genotipo)


# Si ocurre 2) -> 
# Comparaciones multiples de interaccion (todas las medias contra todas) 
#  "o" 
# Comparaciones de Efectos Simples (Fijar un nivel de uno de los factores y comparar entre los niveles del otro)


# En funcion del resultado del modelo [ver anova(modelo2)] , 
# ?Que tipo de comparaciones realizaria?

### Estamos en el caso 2) 
##    Dos posibilidad de enfoques analiticos

#       2.1   Comparaciones de interaccion (todas las medias contra todas) 

options(emmeans= list(emmeans = list(infer = c(TRUE, F)),
                      contrast = list(infer = c(TRUE, TRUE)))) # opciones de seteo para la salida del emmeans


comp <- emmeans(modelo, pairwise ~ densidad*provincia) # Tukey por default
comp


plot(comp$emmeans, comparisons = TRUE)


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


#### Grafico de valores predichos por el modelo (hay más opciones graficas...)

#Banda de confianza

model_plot <-ggpredict(modelo, 
                      terms = c("densidad", "provincia"),
                      interval = "confidence")   
model_plot
grafico <- plot(model_plot, add.data = F) # para agregar los puntos observados add.data = T
grafico + ggtitle("valores predichos") + labs(y="concentracion de aceite (ml/100 grMS)", colour = "procedencia")




##a) ¿El efecto de la densidad sobre la producción de aceites esenciales es el mismo en los tres genotipos?
# al dar significativa la interaccion, vemos que el efecto difiere segun genotipo-competencia


#b) Si el efecto de la densidad no es el mismo para los tres genotipos, para decidir qué tipo
#de comparación realizar (comparaciones múltiples de interacción o comparaciones de
#                         efectos simples) suponga dos escenarios distintos según los objetivos del ensayo:
#  i) se desea iniciar una producción comercial eligiendo la combinación de genotipo y
#densidad que rinda más ¿qué combinación de genotipo y densidad recomendaría? ¿Cuál es
#la máxima diferencia en el rendimiento (magnitud de efecto) observada entre combinaciones
#de genotipo y densidad?, Expresarla en términos absolutos y relativos 0.83
#MULTIPLES

#ii) se desea trabajar con baja densidad de plantas ¿qué genotipo le recomendaría para
#maximizar la producción de aceites esenciales? Informe e interprete el IC de la máxima
#magnitud de efecto.
#SIMPLES
