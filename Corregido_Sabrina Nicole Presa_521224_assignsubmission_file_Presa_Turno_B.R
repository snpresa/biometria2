# Importo las librerias
library(ggplot2)
library(emmeans)
library(car)
library(ggeffects)
library(lme4)
library(lmerTest)
#####################################################
#### Biometria 2 - 1er Parcial - 25 Septiembre de 2024 ####
#####################################################

### Nombre y apellido: Sabrina Presa

### Turno de Trabajos Prácticos: B
# Lunes y miércoles = A
# Martes y Jueves= B

### Correo electrónico: snpresa@gmail.com

#################################
### Antes de comenzar:
#################################

## Lea atentamente el enunciado y responda lo que se pregunta en el script con comentarios '#' ( son 7 preguntas en total, algunas tienen subitems)

## Recuerde que su script debe correr. Esto significa que solo con el script y la base de datos en el mismo directorio el docente que corrija su examen debería poder ejecutar y visualizar todos los resultados sin necesidad de agregar o eliminar líneas de código. 

## Comente el código con #. Esto significa que debe explicitar para qué incluye cada línea o grupo de líneas (según corresponda) de comando (ej. si lo hizo para evaluar un supuesto, para ajustar un modelo, para recodificar una variable, para generar un gráfico, etc.).  

## Para aprobar el examen, además de la correcta resolución del ejercicio, tenga en cuenta que:
#	El script debe correr correctamente
#	No deben faltar comentarios en relación a las líneas de código que decida utilizar
#	Debe existir coherencia interna entre comandos y comentarios.
#	Las conclusiones deben estar fundamentadas


### *****************************
#    El examen es INDIVIDUAL    #
### *****************************

### **********************************
#   Vaya guardando su Script !!!!
#   Apellido_Turno
#   Ej: "Perez_Turno_A"
# Una vez finalizado, subir al campus
### **********************************



#################################
### Enunciado

##La generación de basura y su disposición sin manejo contribuyen a la contaminación del suelo y del agua, así como también a la emisión de gases de efecto invernadero (GEI). En CABA cada persona genera en promedio alrededor de 1,5 kg de basura por día, 30% de los cuales son residuos orgánicos. Si estos residuos fueran separados en origen, podrían compostarse para reducir los efectos nocivos de su disposición sin manejo. El proceso del compostaje está regulado principalmente por microorganismos aerobios que descomponen la materia orgánica generando además suelo fértil de alta calidad. 

##Para poder compostar residuos orgánicos a gran escala, así sean de origen domiciliario o de una producción agropecuaria por ejemplo, es necesario conocer los factores que permiten hacer más eficiente el proceso y reducir las emisiones de GEI. Se sabe que la temperatura ambiente regula la actividad de los microorganismos, y que la falta de oxigenación puede generar que proliferen organismos anaerobios que enlentecen el proceso y son emisores de GEI como el metano. Un grupo de investigación desea  estudiar cómo es la relación entre la temperatura y la emisión de GEI en el proceso de compostaje y si esta se modifica  según la frecuencia de volteo del sustrato a descomponer, que sirve como una técnica de oxigenación. 

##Para ello en un laboratorio se realizó un ensayo balanceado, en donde se dispusieron 75 reactores de 2 litros c/u en los que se colocó un sustrato orgánico común de peso conocido a descomponer. A cada reactor se le asignó al azar una de 5 condiciones de temperatura externa controlada (20, 25, 30, 35 y 40°C), y un nivel de  frecuencia de volteo (sin, media y alta). Al cabo de 120 días se registró, entre otros gases,  la emisión de metano en mg CH4/kg Materia Seca/día. 

##Datos en el archivo: datos3.csv (75 observaciones y  4 variables)

##Diccionario de variables: 
#  Id: identificador único de cada reactor, numeric
# temp: temperatura (°C) del reactor, numeric .
# volteo: frecuencia de volteo, factor ("sin","media", “alta”)
# metano: emisión de metano (mg CH4/kg Materia Seca/día), numeric


# En base al enunciado, responda cada una de las preguntas de manera intercalada (debajo de cada pregunta) 

#PREGUNTA 1 (15 puntos)
## a) Identifique las variables explicativas (tipo y condición de fijas o aleatorias, cruzadas (con) o   anidadas (en)), el tipo de diseño y la cantidad de réplicas. 

# Variables explicativas (2):
#                   Temperatura del reactor en C°. Es cuantitativa y fija. (podria potencialmente manejarse como una cualitativa con 5 niveles). Cruzada con frecuencia de volteo
#                   Frecuencia de volteo, es cualitativa con 3 niveles (sin, media, alta) y fija. Cruzada con Temperatura del reactor.
# El tipo de diseño es completamente aleatorizado (DCA) y tiene 5 replicas por combinacion temperatura*frecuencia de volteo.

## b) Escriba en parámetros el modelo  más parsimonioso para el diseño presentado. Incluya los supuestos distribucionales. Indique qué significa cada parámetro en el contexto del problema. Para letras griegas utilice palabras, por ej: “beta”. ¿Cuáles y cuántos parámetros estima el modelo?

# El modelo mas parsimonioso en este caso es una regresion lineal. La variable cualitativa frecuencia de volteo la convierto en dummy, al ser de 3 niveles voy a tener 2 dummys y un nivel sera de referencia. La referencia que propongo es frecuencia de volteo "sin"

# EmisionMetano_i = Beta0 + Beta1 * Temp_i + Beta2 * FrecuenciaVolteoMedia_i + Beta3 * FrecuenciaVolteoAlta_i+ Beta4 * Temp_i * FrecuenciaVolteoMedia_i + Beta5 * Temp_i * FrecuenciaVolteoAlta+ Epsilon_i

# Supuesto distribucional:      Epsilon_i ~ NID(0, sigma**2) (Los errores son independientes y su distribucion es normal con media 0 y varianza constante sigma**2, que la vamos a estimar)

# Se estiman 6 parametros: beta0, beta1, beta2, beta3, beta4, beta5 y sigma**2

#### Comnetario: son 7 parametros (6 betas y 1 varianza)

# beta0: En el contexto del problema este valor no seria de interes porque se encuentra fuera del rango de temperatura estudiado. Habiendo diho esto, su representacion (que no nos interesa) seria el valor de la emision de metano media para sin frecuencia de volteo y temperatura 0.

# beta1: Es el cambio esperado en la emision de metano media por cambio unitario de la temperatura para sin frecuencia de volteo

# beta2: Es la diferencia media esperada entre sin frecuencia de volteo y frecuencia de volteo media en la emision de metano cuando la temperatura es 0

# beta3: Es la diferencia media esperada entre sin frecuencia de volteo y frecuencia de volteo alta en la emision de metano cuando la temperatura es 0

# beta4: Es la diferencia media esperada en beta1 cuando la frecuencia de volteo es media

# beta5: Es la diferencia media esperada en beta1 cuando la frecuencia de volteo es alta

# sigma**2: es la varianza de la variable aleatoria epsilon


#### Comentario: falta interpretar en contexto sigma2 - beta 4 y beta 5 , ¿diferencias en que? -falta aclarar- 

#### Pregunta 1: 12/15


#PREGUNTA 2  (10 puntos)
## Realice un gráfico descriptivo en donde se muestre la relación de la variable respuesta con la/s explicativa/s. Concluya en relación a tendencias observadas en la muestra y la existencia de potenciales interacciones. 
setwd("~/Documentos/Biometria II/Posit") # Seteo mi directorio de trabajo
datos<-read.csv("datos4.csv") # Importo la base de datos

# Primero la voy a inspeccionar
head(datos)
dim(datos) # 75, 4 (como esperaba)
str(datos) # veo el tipo de datos en cada columna
datos$volteo<-datos$volteo # la paso a factor

# Grafico descriptivo
q<-ggplot(datos, aes(x =temp , y = metano, colour =volteo)) +
  geom_point(aes(), size=2) +
  xlab("Temperatura (°C)") +
  ylab("Emision de metano (mg CH4/kg Materia Seca/día)") +
  ggtitle("Emision de metano en funcion de la temperatura")+
  geom_smooth(method = "lm")
q

# Se puede observar una relacion lineal positiva entre la temperatura y la emision de metano: a mayor temperatura, la emision de metano aumenta linealmente para cada una de las frecuencias de volteo. Esta relacion pareceria ser igual para los 3 volteos: el efecto de la temperatura pareceria ser independiente de la freuencia de volteo, las pendientes son similares. 

#### Comentario: falta aclarar que es un grafico descriptivo, a partir de la muestra (-1p)
#### Pregunta 2: 9/10


#PREGUNTA 3 (10 puntos)
## a) Analice gráficamente, si corresponde, los supuestos de linealidad y homogeneidad de varianzas. Concluya y justifique en base a la observación gráfica.
# Al ser una regresion lineal, son relevantes ambos supuestos mencionados, tanto linealidad como homocedasticidad.
# Para chequear los supuestos planteo el modelo
modelo1<-lm(metano ~ temp*volteo, datos)
e<-resid(modelo1) # Calculo los residuos
re<-rstandard(modelo1) # Calculo los residuos estandarizados
pre<-predict(modelo1) # Calculo los predichos
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo 1" )
abline(0,0, col="lightblue")
abline(h=2, lty=2, col="red")
abline(h=-2, lty=2, col="red")
# No observo evidencias de incumplimiento de los supuestos. Los residuos parecen distribuirse aleatoriamente sin patrones, no hay evidencia de falta de linealidad. Ademas, pareceria haber variabilidad constante alrededor del 0 por lo que no hay evidencia de heterocedasticidad. En conclusion, no modelaria varianza.


## b) ¿Cuántas pruebas de normalidad debería realizar? Escriba la/s hipótesis nula/s.

# En este caso tenemos solo una hipotesis de normalidad: los errores se distribuyen con una distribucion normal (con media 0 y varianza sigma**2)
# H0: Epsilon_i~ Normal, es decir, la variable aleatoria sigue una distribucion normal
# Este supuesto lo evaluamos con el correlato de los errores, los residuos.
shapiro.test(e)

# El p valor es de 0.1116, no rechazo la hipotesis nula. No hay evidencias de que no se cumpla la normalidad en los errores.

# No hay evidencia de que no se cumplan los supuestos

#### Pregunta 3: 10/10


#PREGUNTA 4 (15 puntos)
## Analice los resultados del modelo ajustado y responda:
summary(modelo1)
anova(modelo1)
## a) La relación de la emisión de metano con la temperatura, ¿se modifica con la frecuencia de volteo? Justifique. Si la respuesta es afirmativa, informe la máxima diferencia para dicha relación.   

# En base a la salida del anova, vemos que la interaccion dio significativa. Esto significa que el efecto de la temperatura sobre la emision del metano varia segun por lo menos una frecuencia de muestreo. Para ver que frecuencia/s y la maxima diferencia para la relacion hago comparaciones a posteriori de las pendientes.

options(emmeans= list(emmeans = list(infer = c(TRUE, FALSE)),contrast = list(infer = c(TRUE, TRUE)))) # seteo de opciones de salida
# comparaciones
comp_pendientes <- emtrends(modelo1,  pairwise ~ volteo, var="temp")
comp_pendientes
plot(comp_pendientes$emtrends, comparisons = TRUE)

# Podemos ver que la pendiente de la frecuencia de volteo alta no difiere de la media pero que ambas difieren y son menores de la pendiente sin frecuencia de volteo. Esto nos dice que  aumentar la frecuencia de volteo hace el efecto del aumento de la temperatura sobre el aumento medio del metano sea menor. Amortigua los efectos de emision del gas metano.
# La diferencia maxima para la relacion se da entre alta - sin: disminuir la frecuencia de volteo de "alta" a "sin" hace que por cada aumento unitario de la temperatura, la emision de metano aumente en promedio 1.377 mas

## b) Si se decidiera compostar este sustrato a una temperatura promedio, ¿qué frecuencia de volteo recomendaría? 

# Para eso comparo los valores de emision de metano a temperatura promedio para cada frecuencia de volteo.
comp_volteos <- emmeans(modelo1, pairwise ~ volteo|temp)
comp_volteos # Podemos ver en la salida que se realizo a temp=30
plot(comp_volteos$emmeans, comparisons = TRUE)

# Recomendaria la frecuencia de volteo alta, donde se encuentra la menor media, para que la emision de metano media sea lo menor posible.

## c) Informe e interprete la magnitud del máximo efecto estimado en b) con un IC95.

# Maximo efecto: Con una confianza del 95% se estima que con frecuencia de volteo alta la emision de metano esperada media sea entre 87.4 y 75.4 mg CH4/kg Materia Seca/día menor que con una frecuencia de volteo nula ("sin"). 

#### Pregunta 4: 15/15


#PREGUNTA 5 (15 puntos)
## a) Presente un gráfico con los principales resultados del ensayo. Escriba un epígrafe que indique variables graficadas y medida de dispersión, principales resultados a destacar y modelo ajustado. Rotule ejes e informe un título. 

anova(modelo1)
summary(modelo1)

a<-ggpredict(modelo1, 
              terms = c("temp", "volteo"),
              interval = "confidence") 

p<-plot(a, add.data = TRUE)
p + ggtitle("Relación predicha entre emision de metano y la temperatura para las frecuencias de volteo") +
  labs(x="Temperatura(°C)") + labs(y="Emision de metano (mg CH4/kg Materia Seca/día) ")

# Epígrafe: Se grafica la relacion predicha por el modelo entre la emision de metano en mg CH4/kg Materia Seca/día y la temperatura en °C para las tres frecuencias de volteo: sin, media y alta. El modelo explica el 94.09% de la variabilidad en la emisión del metano por lo que tiene gran capacidad predictiva dentro del rango. La ecuacion final es:

# EmisionMetano_i=44.23 +0.29 * Temp_i + 24.27 * FrecVolteoMedia_i + 40.07 * FrecuenciaVolteoSin_i + 0.52 * Temp_i * FrecuenciaVolteoMedia_i + 1.38*Temp_i * FrecuenciaVolteoSin

# y se estimo una varianza de 79  (mg CH4/kg Materia Seca/día)**2
#En conclusion, se obtuvo una interaccion significativa entre temperatura y frecuencia de volteo, lo que significa que el cambio en la emision de carbono depende de la frecuencia de volteo. Haciendo comparaciones a posterori, vimos que la relacion entre temperatura y metano difiere para las frecuencias de volteo "sin" y "alta/media"(estas ultimas no difieren entre si). SIn embargo, la emision de metano a temperatura media difiere significativamente para las 3  frecuencias de volteo, indicando que la manera mas efectiva de reducir la emision de metano a t=30 era con frecuencia de volteo alta. Esto respalda la hipotesis de los investigadores de que la frecuencia de volteo alta podria favorecer la oxigenacion y por lo tanto evitar la proliferacion de microorganismos anaerobios productores de metano.

### ACLARACION: el modelo que yo plantee inicialmente tenia como referencia a la frecuencia de volteo "sin", cuando hice el ajuste R me puso como referencia a Frecuencia de volteo alta. Los parametros son analagos cambiando eso. No me di cuenta antes como para cambiar con levels la referencia o para cambiar mi modelo inicial. (D:)

## b) Prediga la emisión de metano en un reactor sometido a una temperatura de 28°C y una frecuencia de volteo media. Escribir el código utilizado para llegar a dicha predicción. 

nuevo = data.frame(volteo= "media", temp=28)
nuevo 
predict(modelo1, nuevo, interval="prediction") 

# La emision de metano media esperada para un reactor sometido a una temperatura de 28°C y una frecuencia de volteo media se encuentra entre 72.83 y 109.13 mg CH4/kg Materia Seca/día con un 95% de confianza

#### pregunta 5: 15/15

#PREGUNTA 6 (20 puntos)
## Si se registrara la emisión de metano por triplicado en cada reactor (tres determinaciones por reactor).
## a)Indique si cambia (se agrega o quita) algún/os términos / factor/es del modelo anterior. 

# Al registrar por triplicado la emision de metano en cada reactor no habria independencia entre las observaciones pertenecientes a cada reactor. Es por eso que habria que sumarle al modelo una variable de efectos aleatorios para implicitar esa falta de independencia e inducir esa estructura en los datos. Esa variable seria cada reactor y el modelo que yo propondria seria un modelo condicional, asumiendo misma correlacion entre observaciones de un mismo reactor.

## b)Escriba la sintaxis en R para implementar el modelo. 
modelo2<-lmer(metano ~ temp*volteo+(1|Reactor), datos) # considerando que en la columna metano tengo las nuevas observaciones
# Tambien podria usarse la columna id que hace referencia al reactor, habria que pasarla a factor para darle forma cualitativa.

## c) Complete el siguiente párrafo:

##  Este modelo estima 2 varianza/s que se interpreta/n en contexto como la variabilidad dentro de cada reactor (Varianza clasica residual) y la variabilidad entre reactores (varianza de la variable de efectos aleatorios) 


#### Comentario: "variabilidad ALEATORIA entre reactores sometidos a una misma combinación de temp*frec de volteo" 

## d) Explique qué diferencias, ventajas o desventajas tendría haber analizado los datos promediando las tres determinaciones

# En ese caso no habria que modelar la falta de independencia entre las observaciones, lo que podria considerarse una ventaja. Sin embargo, al promediar estamos perdiendo información y la variabilidad entre reactores ahora estaria yendo al error. Al explicitar esa variable aleatoria absorbo variabilidad.


#### Pregunta 6: 18/20

#PREGUNTA 7 (15 puntos)
## A partir de los hallazgos del ensayo anterior, otro grupo de investigación realiza un nuevo estudio con el objetivo de evaluar qué método de volteo resulta más adecuado para favorecer la actividad de los microorganismos que descomponen la materia orgánica y cómo se modifica dicha actividad en el tiempo. Para ello realizan un ensayo en condiciones controladas a una temperatura de 30°C en el que se registra un seguimiento de 15 reactores previamente asignados de manera balanceada a cada una de las frecuencias de volteo (sin, baja y alta), midiendo la actividad microbiana a través de la respiración como mg CO2/gr MateriaSeca/hora, al inicio, a los 15, 30 y 45 días.
#Uno de los resultados exploratorios fue el siguiente:
#> coef_corr <- round(cor(datos), 2)
#> coef_corr
##        1       2       3      4
## 1     1       0.77    0.53    0.31
## 2     0.77    1       0.68    0.42
## 3     0.53    0.68    1       0.72
## 4     0.31    0.42    0.72    1


## a) Escriba la sintaxis en R del modelo más parsimonioso que ajustaría, en base a la información brindada. Justifique. 

# Al tratarse de un diseño de medidas repetidas en el tiempo, es necesario explicitar una matriz de correlaciones y plantear un modelo marginal
m2 <- gls(metano ~ volteo * Tiempo,
          correlation = corAR1(form = ~ 1 | reactor),
          datos)
# Al haber correlacion en el tiempo, propongo una matriz autoregresiva de primer orden (con homocedasticidad). Esta matriz indica que el tiempo 1 estara mas correlacionado con el tiempo 2 que con el tiempo 3 que con el tiempo 4. A medida que aumenta el "gap" entre tiempos, la correlacion disminuye. Si miramos la matriz de correlacion dada, los valores en las diagonales son similares, lo que respalda la hipotesis de que este modelo presentaria el mejor ajuste con menor cantidad de estimadores.
# Sin embargo, propondria varias matrices (simetria compuesta, autoregresiva y desestructurada) y haria una seleccion de modelos con aquellos que cumplen los supuestos, me quedaria con el de menor AIC.

#### Comentario: ver "aquellos que cumplen los supuestos"

## b) ¿Cuántos y cuáles parámetros estima este modelo? Informe la cantidad de parámetros, mencionando para cada uno de ellos si se trata de coeficientes, medias, varianzas, etc. 

# Este modelo estimará, asumiendo que se cumplen los supuestos de regresion (como la linealidad): beta 0, beta1, beta2, beta3, beta4, beta5, la varianza residual y un coeficiente de correlacion (rho). 

# Si no se cumpliera la linealidad, plantearia un modelo de comparacion de medias que estime 12 medias, una varianza y un rho.

#### Pregunta 7: 14/15
