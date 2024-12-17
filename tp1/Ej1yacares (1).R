
### Problema 1. Modelo de regresión lineal: una aplicación. 

# ********************************************************** #
# Este es un codigo GUIA. Modifiquen y agreguen lo necesario!
# ********************************************************** #


# Antes que nada veamos si hay algun comando u objeto almacenado en la memoria
ls()
# Si te aparece character(0) es que la memoria esta limpia, si no ejecuta
# la siguiente sentencia para borrar la memoria.
rm(list=ls())
# revisamos...
ls()
# No deberia haber nada en el entorno de trabajo ahora!

#### seteo del directorio de trabajo
#setwd("~/Documentos/sabri biome")
setwd("C:/Users/snpre/Downloads")
#### cargar base de datos (hay muchas formas)

datos  <- read.csv("yacares.csv",  header = TRUE, sep = ",", dec = ",") 

#### Explorar la base de datos, corroborar su correcta carga (mas comandos)

str(datos) # 11 observaciones de 2 variables
class(datos$RU) #Es un numero entero
summary(datos) # Ambas son variables numéricas
head(datos)

# sigue ...

#### Exploracion grafica: relacion entre variables, existencia de datos atípicos, influyentes, etc. 

library(ggplot2)

plot <- ggplot(datos, mapping = aes(x=RU, y=DI))+
  xlab("Dosis de RU (ug/huevo)") +
  ylab("Indice de daño")+
  geom_point(size=3, color="black") 
  theme_bw() 
plot + theme(axis.text.x = element_text(size=12),
             axis.text.y = element_text(size=12), 
             axis.title.x = element_text(size = 14), 
             axis.title.y = element_text(size = 14))


#1.¿Es lógico suponer una relación lineal entre ambas variables?
#Sí, en este grafico exploratorio observamos que la dosis de Ru parece aumentar linealmente con el indice de daño. Podriamos verlo con una prueba de correlacion
cor(datos$DI, datos$RU) #0.987


#### Ajuste del modelo lineal

modelo <- lm(DI~ RU, data=datos)
modelo 
summary(modelo)
confint(modelo) # por default da el 95%
#                   2.5 %       97.5 %
#(Intercept) 102.97877787 110.07841226
#RU            0.03285004   0.04191994

#### Supuestos del modelo
# Normalidad, Linealidad, homocedasticidad
# proximas clases ... 


#### Grafico con predichos (hay muchas opciones)

library(ggeffects)
model_plot <- ggpredict(modelo, 
                      terms = c("RU"),
                      interval = "confidence")   
model_plot
plot(model_plot, add.data = T)   +
  xlab("Dosis de RU (ug/huevo)") +
  ylab("Indice de daño")

#2. Escribir el modelo lineal e interprete en el contexto del problema sus parámetros

# y=B_0+B_1 x

# la ordenada b0 es el valor de daño en el ADN cuando la dosis del tratamiento (Round up, sin herbicida) es nula. Tiene unidades de daño

# la pendiente b1 es el cambio medio esperado en el indice de daño por cada cambio unitario de dosis de Round Up. Tiene unidadex y/x

# por cada  unidad adicional de dosis de RU se observa u incremento medio en el indice de daño de 0.037 (unidades de ID)
# Nos interesa la prueba de hipotesis de b1

#3. Ajustar el modelo en R e interprete el resultado de la prueba de hipótesis sobre la pendiente
#con su IC95%. Informe la ecuación de la recta estimada.
summary(modelo)

# El p-valor es re bajo, por lo que rechazo la hipotesis nula que establece que b1 = 0 y que la variacion de daño no se explica por variacion de dosis

confint(modelo)

# El aumento unitario de la dosis de RU produce un aumento medio en el daño entre 0.033 y 0.04 con 95% de confianza
#RU            0.03285004   0.04191994
#Por c/unidad adiconal de round up se ve un incremento medio en el indic de daño entre esas unidades con una confianza de 95%
summary(modelo)

# IndiceDaño= 11.5 + 0.037 * Dosis_de_RU

#4. Informar el R2 e interprete su valor en el contexto del problema.

#R2 0.9748. Cuanto de la variabilidad total logra explicar el modelo. Medida de la capacidad predictiva. No depende de las unidades de medicion. A mas r2 mas cerca los puntos de la recta
#mas r2 ms fuerza para predecir dentro del rango

#5. Si se repite el experimento, ¿espera obtener los mismos valores para los
#estimadores de los parámetros (mismos valores de β0^ y β1^)?

# No. Existe variabilidad o varianza del modelo. Si repito el experimento 100 veces, espero que mi parametro este en el IC 95 veces.

#6. Presentar un gráfico con los valores predichos por el modelo que resuma los principales hallazgos.

#**