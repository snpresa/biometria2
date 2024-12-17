#####################################################
#### Biometria2 - 1er Parcial - 28.9.223 ####
#####################################################
library(reshape2)
install.packages("lme4")
library(lme4)
install.packages("lmerTest")
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
install.packages("car")
library(car)
library(ggplot2)
library(gridExtra) 
library(nlme) # "Nonlinear Mixed-Effects Models"
library(ggeffects) # para ggpredict


### Nombre y apellido:

### Turno de Trabajos Pr?cticos: A
#  [ Soledad = A / José = B / Nicolás = C ]

### Correo electronico:

#################################
### Antes de comenzar:
#################################

## Lea atentamente el enunciado y responda lo que se pregunta en el script con comentarios '#'.

## Recuerde que su script debe correr. Esto significa que solo con el script y la base de datos en el mismo directorio el docente que corrija su examen deberia poder ejecutar y visualizar todos los resultados sin necesidad de agregar o eliminar lineas de codigo. 

## Comente el codigo con #. Esto significa que debe explicitar para que incluye cada linea o grupo de lineas (segun corresponda) de comando (ej. si lo hizo para evaluar un supuesto, para ajustar un modelo, para recodificar una variable, para generar un grafico, etc.).  

## Para aprobar el examen, ademas de la correcta resolucion del ejercicio, tenga en cuenta que:
#	El script debe correr correctamente
#	No deben faltar comentarios en relacion a las lineas de codigo que decida utilizar
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
#################################

#################################
### Enunciado
#################################
rm(list=ls())
# La teoría de forrajeo se centra en las decisiones comportamentales que toman los animales para optimizar el esfuerzo dedicado a la búsqueda y explotación de recursos, la exposición al riesgo de depredación y las preferencias dietarias. Estas decisiones conductuales pueden traducirse en patrones de distribución y abundancia. Un proceso clave que vincula los movimientos individuales con la distribución de las especies es la formación y el mantenimiento de las áreas de acción (HR, por su siglas en inglés, home range), es decir, el espacio utilizado para satisfacer las necesidades vitales.

# El tamaño del HR refleja una compensación entre los costos y beneficios asociados a la adquisición de recursos, como el alimento, durante un periodo de tiempo determinado. Un determinante clave del tamaño del HR es la disponibilidad de recursos forrajeros, ya que el tamaño del HR disminuye en los entornos más productivos.

# Con el fin de estudiar factores que determinan el tamaño del HR, se realizó un estudio en Canis lupus (lobo) en Canadá. Esta especie se alimenta principalmente de animales silvestres pero, ante la ausencia de estos, puede alimentarse de animales de cría. Se estudió el HR en 4 ambientes con diferente uso (A1: "bosque nativo", A2: "forestación", A3: "ganadería" y A4: "pastizal") en zonas con nieve y zonas sin nieve. Los investigadores predicen que las zonas con nieve tendrán menor productividad de recursos alimenticios y por lo tanto menor cantidad de presas disponibles, por lo que los individuos de las manadas de esas zonas deberán tener un mayor HR. Sin embargo, sospechan que la disponibilidad de animales de cría puede atenuar estas diferencias.

# En cada tipo de ambiente y cada condición de nieve se seleccionaron en forma aleatoria 3 manadas de C. lupus y se determinó el HR diario (en km2) de 4 de sus individuos, elegidos al azar entre todos los integrantes de la manada.

# Datos en el archivo datos.csv., contiene 96 filas y 5 columnas.

# En base al enunciado (INTERCALE LAS RESPUESTAS LUEGO DE CADA PREGUNTA),
datos<-read.csv("datos.csv")
head(datos)
str(datos)
datos$individuos<-factor(datos$individuos)
datos$manada<-factor(datos$manada)
datos$nieve<-factor(datos$nieve)
datos$areas<-factor(datos$areas)
### Pregunta 1
###############

# 1 a) Complete el diccionario de variables indicando sus clases. Para las variables explicativas declare además su condición de fijas o aleatorias, cruzadas (con) o anidadas (en) y la cantidad de réplicas.

#Diccionario de variables:
#Tipo de ambiente: Explicativa, fija, cualitativa. 4 niveles (A1,A2,A3,A4).  Cruzada con nieve
#Condición de nieve:Explicativa, fija, cualitativa, 2 niveles (con, sin). Cruzada con ambiente
#Manada: Explicativa, aleatoria, cualitativa, 3 niveles por condicion de nieve y ambiente. Anidada en la interaccion nieve ambiente
#Individuo: identificador unico de cada indivudio, cualitativa, 96 niveles

# unidad muestral: la manada. Tiene que tener indepenedencia
# unidad observacionales: un lobo
# replicas---3
#HR: Variable respuesta, cuantitativa continua. Proponemos una distribucion normal

# 1 b) Escriba el modelo en parámetros (junto con subíndices y supuestos distribucionales), indicando el significado de cada término en contexto (para las letras griegas utilice palabras). Si existiera alguna/s variable/s de efectos aleatorios incorpórela/s de forma implícita.

# HR(km2)=mu + alfa_i+ beta_j + alfabeta_ij + M_k(j,i)+epsilon_ijkl

#M_k(j,i)~N(0,sigma**2_manada) variabilidad en el HR entre manadas

#Epsilon_ijkl~N(0, sigma**2_res) variabilidad en el HR dentro de cada manada

### Pregunta 2
###############

# Realice un gráfico de perfiles en donde se muestre la relación entre el HR y la condición de nieve, discriminando por tipo de ambiente. Escriba una frase describiendo dicha relación.

ggplot(data = datos, aes(x = nieve, y = HR, fill = areas)) +
  geom_line(aes(group = areas, colour = areas), size=1) +labs(title = "Home Range por area y condicion de nieve", x = "Condicion de nieve",    y = "HR(km2") + theme_bw()

# Las rectas se cruzan indicando que  no son parelas por lo tanto no existe paralelismo, esperamos que la respuesta a la condicion de nieve difiera segun el ambiente (interaccion entre emabiente y nieve).

### Pregunta 3
###############

# Ajuste el modelo propuesto por los investigadores. Evalúe los supuestos de dicho modelo y concluya, si corresponde, en base a:
# diseño anidado, observacional
# Modelo condicional
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

m1 <-lmer(HR ~ areas*nieve+(1|manada), datos)
supuestos(m1)
leveneTest(HR~areas*nieve, datos) #no rechazo

# Falta el supuesto de los efectos aleatorios!

#3.1- normalidad
#3.2- homocedasticidad
#3.3- linealidad
#Para cada conclusión indique de dónde obtiene la información (si es de un gráfico, de cuál, si es de una prueba análitica, de cuál, e informe p-valor si corresponde).

### Pregunta 4
###############

# Analice los resultados del modelo ajustado e indique si las siguientes afirmaciones son V o F, justificando su decisión. Cuando corresponda, indique de qué salida obtuvo dicho resultado.
anova(m1)
install.packages("emmeans")
library(emmeans)
# seteo de opciones de salida (opcional, modificando los argumentos T / F puede ver como se modifican las salidas del emmeans)
options(emmeans= list(emmeans = list(infer = c(TRUE, FALSE)),contrast = list(infer = c(TRUE, TRUE))))
# comparaciones
comp <- emmeans(m1, pairwise ~ nieve|areas)
comp
plot(comp, comparisons = TRUE)

# Vemos que la interaccion da significativa, rechazo esa hipotesis. La media de al menos alguna combinacion ambiente*nieve difiere del resto. # El efecto de la condicion de nieve no es el mismo para los 4 ambientes.

# a) Los cambios en el HR entre sitios con y sin nieve difieren significativamente entre(en) los 4 ambientes estudiados. (f) Podemos ver esto en la comparacion de tukey. La media del area A3 con nieve no difiere de sin nieve

# b) Si se ajustase un modelo marginal con matriz de correlación de simetría compuesta los coeficientes estimados por el summary() variarán ligeramente. (f) Los coeficientes estimados por ambos modelos seran iguales, la parte fija del modelo es igual. 

# c) Las fuentes de variación aleatorias del modelo ajustado son dos: manadas e individuos. (v) #V, la varianza entre manadas de una misma combinación ambiente-nieve esta representada por sigma^2manada y la varianza entre individuos de una misma manada porsigma^2 y la suma de ambas es la varianza aleatoria total.


# d) La varianza del modelo es 0.88550 km2.
summary(m1) # (F) las unidades estan mal, son las de HR **2, es decir, km**4
# e) Es indistinto evaluar la significación de los términos del modelo utilizando summary o anova
# F, en summary no se controla el error global y por lo tanto lo correcto es evaluar la significacion con el anova dado que hay una variable categorica con mas de 2 niveles.
### Pregunta 5
###############

#Efectúe las pruebas que considere de mayor potencia para responder si se verifican las predicciones y sospechas de los investigadores.

#Los investigadores predicen que las zonas con nieve tendrán menor productividad de recursos alimenticios y por lo tanto menor cantidad de presas disponibles, por lo que los individuos de las manadas de esas zonas deberán tener un mayor HR. Sin embargo, sospechan que la disponibilidad de animales de cría puede atenuar estas diferencias.

# Lasc omparaciones pertinentes son para cada area comparar con o sin nieve (nieve | areas)
# # Las predicciones de los investigadores se verifican dado que para todos los ambientes el HR es significativamente menor (p<0.0001, o limites del intervalo de confianza para las diferencias de media que no incluyen al 0) en zonas "sin nieve", a excepcion del ambiente 3 que es el unico con disponibilidad de animales de cria ("ganaderia"), en donde no se encuentran diferencias entre los diferentes zonas de nieve (p = 0.7554).

### Pregunta 6
###############
comp
plot(comp, comparisons = TRUE) # SON CUATRO COMPARACIONES DE EFECTOS SIMPLES

# Informe e interprete el IC 95% para la magnitud de efecto del ambiente con el máximo cambio en el HR medio entre zonas con y sin nieve.

# La magnitud de efecto maxima en el cambio en HR medio entre zonas con y sin nieve se da en el area 4. En un ambiente pastizal la variacion en el home range medio de un lobo de una manada tipica de un area con nieve aumenta entre 29.155 y 30.83 km2 con 95% de confianza con respecto a un lobo de un area sin nieve.

# # Interpretación: Con una confianza del 95% se estima que en el ambiente 4 (pastizal) el home range esperado (o medio) es entre 29.155 y 30.83 km2 mayor en zonas con nieve comparado con zonas sin nieve para lobos de una mandada tipica.


# Informe e interprete el HR medio estimado para el ambiente bosque nativo en condiciones de nieve.

# En el A1, bosque nativo, con nieve el home range estimado en condiciones de nieve para un lobo de una manada tipica es de 40.1 km2

### Pregunta 7
###############

# Se sabe que el costo de identificar una manada en terreno es alto. ¿Haría alguna sugerencia orientada a aumentar los esfuerzos en incorporar más manadas o más individuos por manada en futuros estudios? Concluya en base a aportes a la variabilidad aleatoria del HR.

summary(m1)

# La varianza entre manadas es de 0.01159 km4, mientras que la varianza entre individuos de una misma manada es de 0.88550 km4. Podemos calcular el aprote de la variabilidad de las manadas con respecto a la de los individuos con el CCI

CCI<-0.01159/(0.01159+0.88550)
CCI*100 
# El porcentaje de la variabilidad total explicada por la variabilidad entre camadas de un mismo ambiente y condicion de nieve es de 1.3%. La variabilidad entre manadas es muchisimo menor que la variabilidad entre individuos de una misma camada (98.7 % de la var total), por lo que para futuros estudios se podria considerar mas importante la inclusion de mas indivudios que de camadas

### Pregunta 8
###############

# Presente un gráfico con los principales resultados del ensayo con su epígrafe. Escriba un párrafo con la metodología estadística utilizada y las conclusiones del estudio.
library(ggeffects) # para ggpredict
model_plot <-ggpredict(m1, 
                       terms = c("nieve", "areas"),
                       interval = "confidence")   
model_plot
grafico <- plot(model_plot, add.data = F) # para agregar los puntos observados add.data = T

# AGREGUE LOS DATOS CON GEOM_JITTER

grafico + ggtitle("Home Range de lobos en manadas en diferentes ambientes y condiciones de nieve") + labs(y="HR(km**2))") 
### Pregunta 9
###############

# Completar el siguiente párrafo:
length(ranef(m1)$manada$'(Intercept)')
ranef(m1) # 24
# A partir de los datos recopilados en el estudio realizado se pueden obtener (complete con valor numerico) __24_ "ranef" que corresponden a ____las variaciones de cada camada_______________ .
CCI
# El valor del CCI es de (completar con valor numérico) _0.0129___ y sus unidades son (Km /Km2 / no tiene unidades) ______No tiene ynidades___________ . Su interpretación, en contexto, es _La proporcion de la variabilidad total que es explicada por la variabilidad entre camadas de un mismo ambiente/nieve. El valor máximo que puede tomar es __1 en cuyo caso significa que toda la variabilidad del modelo esta explicada por ella, dentro de cada camada los individuos son homogeneos__.

# Las conclusiones de la parte fija del estudio son válidas para ("todas las manadas que formaron parte de este ensayo" / "todas las manadas del área de estudio") todas las manadas del area de estudio . Esto es porque e la inferencia de la parte fija se hace sobre los niveles estudiados de las variables fijas, que en este caso son 4 areas distintas..

### Pregunta 10
###############

# Un investigador propone que los ejemplares más jóvenes tienden a tener HR más pequeños, por su temor a alejarse de la madriguera, y que esta relación es independiente del tipo de ambiente y de las condiciones climáticas. Al haber relevado el dato de la edad de cada individuo en campo,se decide incorporar como predictora a la edad del individuo (en meses).

# ¿Cómo se modifica el modelo planteado? Escriba el modelo en parámetros indicando si cambia la cantidad de parámetros estimada respecto al anterior.

# Agrego una variable explicativa fija CUANTITATIVA que es la edad del individuo. No agrego interaccion porque es "indep del tipo de amb y de las cond clim"
# Al ser una VE cuanti, el modelo migra a una regresion!

# Si incorpora coeficientes en el modelo, explíquelos en contexto.
# Incroporo dummys!! 3 para ambientes, 1 para nieve
# se agrega un coeficiente que establece el efecto de la edad, representa cuanto cambia el home range en promedio con un aumento unitario de la edad

# Escriba la sintaxis en R del nuevo modelo (no tiene que ejecutarlo).
m2<-lmer(HR ~ edad + nieve * areas + (1 | manada), datos)