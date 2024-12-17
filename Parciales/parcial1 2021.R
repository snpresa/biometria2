#####################################################
#### Biometr?a 2 - 1er Parcial - 6.10.21 ####
#####################################################

### Nombre y apellido: 

### Turno de Trabajos Pr?cticos: 
#  [ Martin = A / Soledad = B / Jose = C ] 

### Correo electr?nico: 

### Versi?n de R:R version 4.1.1 (2021-08-10)
#   [ Si no lo sabe, escriba la palabra version en la consola y copie y pegue aqu? la l?nea 'version.string' . Ejemplo: R version 3.6.3 (2020-02-29) ]

### Sistema operativo: 
#  [ Linux/Windows/Mac ]




#################################
### Antes de comenzar:
#################################

## Lea atentamente el enunciado y responda lo que se pregunta en el script con comentarios '#'.

## Recuerde que su script debe correr. Esto significa que solo con el script y la base de datos en el mismo directorio el docente que corrija su examen deber? poder ejecutar y visualizar todos los resultados sin necesidad de agregar o eliminar l?neas de c?digo. 

## Todo c?digo que escriba debe ser comentado (con #). Esto significa que debe explicitar para qu? incluye cada l?nea o grupo de l?neas (seg?n corresponda) de comando (ej. si lo hizo para evaluar un supuesto, para implementar un modelo, para recodificar una variable, para generar un gr?fico, etc.).  

## Si toma decisiones en base a alg?n resultado debe incluir los comentarios pertinentes en el script (ej. eliminaci?n de un dato at?pico, cambio de la distribuci?n para modelar, etc.).

## Para aprobar el examen, adem?s de la correcta resoluci?n del ejercicio, tenga en cuenta que:
#	El script debe correr correctamente
#	No deben faltar comentarios en relaci?n a las l?neas de c?digo que decida utilizar
#	Debe existir coherencia interna entre comandos y comentarios. 
#	Las conclusiones deben estar fundamentadas


### *****************************
#    El examen es INDIVIDUAL    #  
### *****************************

### *****************************
#   Vaya guardando su Script !!!!
#   Apellido_Turno
#   Ej: "Perez_Turno_A"
### *****************************



#################################
### Enunciado 
#################################

# La contaminaci?n de suelos con metales pesados representa una grave preocupaci?n por sus posibles consecuencias para
#  el medio ambiente y la salud humana. El cromo en particular es un contaminante industrial com?n y usualmente ocurre
#   en el ambiente en la forma de Cr(VI), que es t?xico y soluble con alta movilidad. Los hongos micorr?zicos son hongos
#    que establecen relaciones simbi?ticas con las plantas, particularmente en sus ra?ces. Investigaciones recientes
#     sugieren que los hongos micorr?zicos aumentan la tolerancia al estr?s de las plantas, por lo que podr?an protegerlas
#      contra las toxinas ambientales. 

# Se llev? a cabo un estudio a fin de investigar si el hongo micorr?zico Beauveria bassiana presenta un efecto protector
#  frente al cromo en el desarrollo de plantas de Arabidopsis thaliana. Para ello, se prepararon 40 cajas conteniendo
#   suelo esterilizado y en cada una se colocaron dos plantas. A cada caja se le asign? en forma aleatoria y balanceada
#    una combinaci?n de dosis de cromo (0, 10, 20 y 40 ppm) y de in?culo de hongo (s?/no). Las plantas crecieron hasta
#     alcanzar la madurez. En cada planta se registr? la biomasa total (en gramos) a los 45 d?as como indicador de
#      desarrollo vegetativo.

# VALOR DE CADA PREGUNTA: 1 punto. 
Datos <- read.delim("Datos_2021.txt")

head(Datos)

# 1) Identifique la/s variable/s explicatoria/s. Se?ale si son cualitativas (y niveles) o cuantitativas, de efectos fijos
#  o aleatorios y si los niveles de cada una est?n cruzados o anidados y con qui?n.
#  
#  VE1: Hongo: Cualitativa, dos niveles. Fija
#  VE2: Dosis: Cuantitativa. Fija. Cruzada con hongo.
#  VE3: Caja: Cualitativa, con 40 niveles. Aleatoria, anidada en interaccion de Dosis y hongo.
library(ggplot2)
plot <- ggplot(Datos, mapping = aes(x=dosis, y=biomasa, colour = hongo))+ 
  xlab("Dosis de Cr") +
  ylab("Biomasa (g)")+
  geom_point(size=3) +
  ggtitle("biomasa en funcion del cromo")+
  geom_smooth(method = "lm", se= F) # SE= FALSE o TRUE para intervalo de confianza
theme_bw() 
plot + theme(axis.text.x = element_text(size=10),
             axis.text.y = element_text(size=10), 
             axis.title.x = element_text(size = 12), 
             axis.title.y = element_text(size = 12),
             plot.title = element_text(size=14)) 

# 2) Se desea incluir en el modelo a Cromo como variable explicativa cuantitativa.PEro antes ?cu?ntos (y cu?les)
#  par?metros estimar?a el modelo en caso de haberla incluirla como CUALITATIVA?
# En un modelo de comparacion de medias se deber?an calcular 8 par?metros, seis medias y dos varianzas (residual y
#  de la variable aleatoria)  contra 6 par?metros en un modelo de regresion


# 3) Considerando las preguntas (1) y (2), Implemente el modelo lineal en R. De existir datos dependientes, incluir
#  la correlaci?n de manera impl?cita. 
library(lme4)
library(lmerTest)
Modelo <- lmer(biomasa~dosis*hongo + (1|caja), data = Datos)



# 4) ?Cu?ntas prueba de Shapiro realiza? Plantee las Ho que corresponda.
# Se deben realizar dos pruebas de Shapiro, una por la normalidad de los residos y otra por la de la variable aleatoria.

# 5) Realice el diagn?stico de los supuestos. a) Responda espec?ficamente si modelar?a la varianza. Justifique por qu?
#  SI o por qu? NO. b) ?Se rechaza el supuesto de linealidad? Justifique.
e<-residuals(Modelo) # Residuos
pre<-predict(Modelo) # Predichos

#Homogeneidad de varianza y linealidad
plot(Modelo) # No rechazo homocedasticidad ni linealidad.

# Normalidad de residuos
qqPlot(e, main = "QQ Plot residuos")
shapiro.test(e) #No rechazo normalidad de residuos

# Normalidad de variable aleatoria
alfai<-ranef(Modelo)$caja$'(Intercept)'
qqPlot(alfai, main = "QQ Plot ef aleat")
shapiro.test(alfai) # No rechazo normalidad de alfai
 #No modelo varianza porque no hace falta.

# 6) En base a su modelo final responda ?Considera que la presencia de hongos micorrizos tiene un efecto protector sobre
#  la tolerancia al Cromo de Arabidopsis? Justifique. Interprete la magnitud de efecto (IC95%) para una dosis de cromo
#   promedio. 
anova(Modelo) #Interaccion significativa, entendemos que el efecto de la dosis de cobre es disitno de acuerdo asi
 # hay in?culo de hongo o no.

confint(Modelo) # la magnitud de efecto de no tener in?culo es entre -2.02 y 0.88 g de biomasa de la planta ante una dosis
 # nula y un raton t?pico
 
 efsimple <- emmeans(Modelo, pairwise ~ hongo | dosis)
 efsimple
 
 plot(efsimple$emmeans, comparisons = TRUE)
 # Para una dosis promedio de cromo, la magnitud de efecto del in?culo es entre 14.1 y 15.5 g, comparando con una
 #  planta sin in?culo, el desarrollo de la planta es 2.97 y 4.97 g mas grande.


# 7) a) Interprete en contexto la estimaci?n del par?metro beta 2. 
summary(Modelo) # B2 es la diferencia entre la biomasa de las plantulas de las cajas cuando no fueron inoculadas con
# hongos micorrizos y las plantas que si, a una dosis de cromo de 0. Esta diferencia no es significativa para el modelo
#  con respecto de cero. 

#b) Escriba la ecuaci?n estimada para predecir la biomasa de Arabidopsis en presencia del hongo en funci?n de
# la dosis de Cromo.  
# biomasa_ = 18.43 - 0.21*dosis -0.57*hongo - 0.19 * dosis*hongo 
#  Como hongo = 0 queda:
#  biomasa_ = 18.43 - 0.21*dosis 


# 8) Calcule el coeficiente de correlaci?n intraclase e interpr?telo en contexto.
library(performance)
model_performance(Modelo, metrics = "ICC")
# Este  coeficiente dio de 0.942, por lo tanto el 94.2% de la varianza total se debe a diferencias entre las cajas.
# Mientras que el resto se debe a diferencias dentro de las cajas, entre las plantulas de cada caja.


# 9) ?Si tuviese que repetir el ensayo, mantendr?a la elecci?n de utilizar dos plantas por caja? Justifique su
#  respuesta utilizando una prueba de hip?tesis que permita responder esta pregunta. 

ranova(Modelo)

# 10) Concluya en funci?n de los objetivos del estudio. Acompa?e con un gr?fico con las predicciones del modelo y
#  un ep?grafe explicativo. 
library(sjPlot)
plot_model(Modelo, type = "pred", terms = c("dosis", "hongo"))

##########################
##########################
### FIN
##########################
##########################
