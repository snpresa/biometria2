
#  TP N° 1 - # Problema 6. Características morfológicas de plantas del género Iris

# Exploración gráfica


# Antes que nada veamos si hay algun comando u objeto almacenado en la memoria
ls()
# Si te aparece character(0) es que la memoria esta limpia, si no ejecuta
# la siguiente sentencia para borrar la memoria.
rm(list=ls())
# revisamos...
ls()
# No deberia haber nada en el entorno de trabajo ahora!


# Librerias necesarias
library(ggplot2)      # Graficos con ggplot
library(ggcorrplot)   # Correlaciones con libreria ggcorrplot
library(corrplot)     # Correlaciones con libreria corrplot 
library(gridExtra)    # Visualizacion en paneles de graficos con ggplot
library(GGally)
# Setear el directorio de trabajo

setwd("C:/Users/snpre/Documents/Carrera/Biometria II/tp1")

########### El dataset ############

Datos <- iris # el dataset iris se encuentra disponible en R

# por comodidad modificamos los nombres de las columnas al castellano:

colnames(Datos) <- c("LongSepalo", "AnchoSepalo", "LongPetalo", "AnchoPetalo", "especie")

head(Datos)
summary(Datos)
class(Datos$especie)
########################################

### ALGUNOS GRAFICOS UTILIZANDO R BASE ####

########################################

plot(Datos$LongSepalo, Datos$AnchoSepalo, col=Datos$especie)
# Los datos se agrupan por especie, pareceria haber dos especies mas parecidas entre si que con una 3ra
# Pareceria haber una relacion lineal de ancho vs largo de sepalo pero dependiendo de la especie

pairs(Datos[,2:5], pch=as.numeric(Datos$especie))	
ggpairs(Datos[,2:5]) # de ggally
cor(Datos[,2:4])
# Hay correlacion significativa entre varias 

hist(Datos$LongSepalo, ylab="Frecuencia", xlab="Longitud del Sepalo")
# Hay longitudes mas frecuentes del sepalo

plot(Datos$LongSepalo~Datos$especie, main="Longitud del sepalo por especie")
# El promedio de longitud de sepalo de virginica es mayor al de versicolor que es mayor al de setosa (no sabemos si es significativa)

barplot(tapply(Datos$LongSepalo,Datos$especie,mean), main="Longitud del sepalo por especie")
# La misma tendencia se obersva aca

# modificar breaks
hist(Datos$LongSepalo, main="Histograma",ylab="Frecuencia", xlab="Longitud del S?palo", breaks=16)

plot(density(Datos$LongSepalo), main="Densidad de LongSepalo")

# multiples subplots
# windows() # comando opcional si quiere el grafico en una ventana externa
par(mfrow=c(2,2))
hist(Datos$LongSepalo)
hist(Datos$LongPetalo)
hist(Datos$AnchoSepalo)
hist(Datos$AnchoPetalo)

# Vemos que hay longitudes de sepalo y ancho de sepalo mas comunes que otras. En cambio la longitud de petalo y el ancho de petalo se ven bimodales. Esto nos puede decir que hay 2 tendencias en nuestros datos con respecto a los petalos

# boxplot - datos atipicos
par(mfrow=c(1,1)) # seteo nuevamente la ventana
boxplot(Datos[2:5])
colnames(Datos[3])
# Solo se observan datos atipicos para el ancho del sepalo, cuya distribucion de datos es ancgosta. En cambio la longitud del petalo se ve mas variable

# boxplot - comparacion variables entre especies
# windows()
par(mfrow=c(2,2))
plot(Datos$LongSepalo~Datos$especie)
plot(Datos$LongPetalo~Datos$especie)
plot(Datos$AnchoPetalo~Datos$especie)
plot(Datos$AnchoSepalo~Datos$especie)

# La longitud del sepalo, la longitud del petalo y el ancho del petalo siguen la misma tendencia: virginica mayor, setosa menor.
# En cambio, el ancho del sepalo parece achatar las diferencias, setosa alli presenta el mayor ancho de sepalo (Ojo, no veo significancia)

par(mfrow=c(1,1)) # reestablece 

## almacenamiento de archivos

# png
# primero seteo el guardado
png("myplot2.png", width= 15, height= 10, units= "cm", res = 90)   # myplot2 nombre del grafico
# Recien lo siguiente es lo que voy a guardar
plot(Datos$LongPetalo,Datos$LongSepalo,
     col=Datos$especie,
     ylab="Longitud del S?palo (cm)",
     xlab="Longitud del P?talo (cm)")
dev.off()

# tiff
tiff("myplot2.tiff", width= 15, height= 10, units= "cm", res = 90)
plot(Datos$LongPetalo,Datos$LongSepalo,
     col=Datos$especie,
     ylab="Longitud del S?palo (cm)",
     xlab="Longitud del P?talo (cm)")
dev.off()

# pdf -- Muy copado
pdf("myplot2.pdf", width= 7, height= 8, paper="special") 
# ancho y alto en pulgadas de la region para graficar
# paper "special", setea el tama?o de la figura al del ancho y largo del grafico
# alternativas paper "a4", "letter" etc
plot(Datos$LongPetalo,Datos$LongSepalo,
     col=Datos$especie,
     ylab="Longitud del S?palo (cm)",
     xlab="Longitud del P?talo (cm)")
dev.off()



########################################

### Algunos graficos con ggplot2 ####

########################################

# A continuación se presentan 3 gráficos. Observar las lineas de código y los gráficos que producen. 

# (Son 3 maneras de hacer lo mismo!)

ggplot(data = Datos) +
  geom_point(mapping =  aes(LongSepalo, AnchoPetalo),  color = "black")  + 
  geom_smooth(mapping = aes(LongSepalo, AnchoPetalo), color = "red")

ggplot(data = Datos, mapping =  aes(LongSepalo, AnchoPetalo)) +
  geom_point(color = "black")  + 
  geom_smooth(color = "red")

ggplot(Datos, aes(LongSepalo, AnchoPetalo)) +
  geom_point(color = "black")  + 
  geom_smooth(color = "red")


## observar el siguiente grafico y comparar similitudes y diferencias con el codigo anterior, y 

ggplot(Datos,aes(LongSepalo,AnchoSepalo))+ 
  labs(x="Longitud del Sepalo (cm)",y="Ancho del Sepalo (cm)") + 
  geom_point(size=2, aes(color=especie)) + theme(legend.title = element_blank())

# notar que el #tema" puede ser modificado...

ggplot(Datos,aes(LongSepalo,AnchoSepalo))+ 
  theme_classic() + 
  labs(x="Longitud del Sepalo (cm)",y="Ancho del Sepalo (cm)") + 
  geom_point(size=2, aes(color=especie)) + theme(legend.title = element_blank())

# tambien se puede guardar un grafico con un nombre (como un objeto)

grafico_b <- ggplot(Datos,aes(LongSepalo,AnchoSepalo))+ 
  theme_classic() + 
  labs(x="Longitud del Sepalo (cm)",y="Ancho del Sepalo (cm)") + 
  geom_point(size=2, aes(color=especie)) + theme(legend.title = element_blank())

grafico_b


# Guardar este ultimo grafico como png, seteando el ancho y largo
# explorar las opciones del comando ggsave, puede ser muy útil! 

ggsave("miGrafico.png", grafico_b, width=10, height=5)


# BOXPLOT

box <- ggplot(Datos, aes(x=especie, y=AnchoSepalo))+        
  geom_boxplot()+ 
  stat_summary(fun = mean, geom="point", shape=8, size=4,color="darkcyan")
box
# Por lo que probe, fun es el punto. Uno puede pedirle que sea la media. Shape le dice que forma tiene el punto

# haciendo mas informativos los boxplot
box2 <- ggplot(Datos, aes(x=especie, y=LongSepalo)) +
  geom_boxplot(aes(color=especie), color="black")+
  theme_bw()+
  geom_jitter(alpha=0.3, size=2,aes(color=especie), position = position_jitter(width = .2))+theme(legend.position="top", legend.text=element_text(size = 14),legend.title = element_text(size=16, face="bold")) +
  ylab("Long Sepalo")+xlab("Especie")
box2


# barplot
# para frecuencias #NO ME FUNCIONA POR LA VERSION, fue descontinuado
barplot <- qplot(Datos$especie, geom="bar")
barplot

# histograma
hist <- ggplot(Datos, aes(x=LongSepalo))+
  geom_histogram(binwidth=.5, fill="blue", colour="black")+
  theme_bw()
hist

# Graficos de dispersion
# Grafico exploratorio con suavizado por defecto (geom_smooth)
grafico1 <- ggplot(Datos, aes(x=LongSepalo, y=AnchoPetalo)) + 
  geom_point(size=3, color="blue", shape=19) +  
  geom_smooth() 
grafico1


# setando geom_smooth
grafico2 <- ggplot(Datos, aes(x=LongSepalo, y=AnchoPetalo)) + 
  geom_point(size=3, color="red", shape=19) +  
  geom_smooth(method=lm, se=T, fullrange=F, size=0.5) 
grafico2



### Asociacion entre variables 

# Primero hay que armar la matriz de correlacion 
# Correlation matrix
corr_datos <- round(cor(Datos[1:4]), 2)
print(corr_datos)

# Plot con ggcprrplot (una opcion d emuchas...)

ggcorrplot(corr_datos,  
           type = "lower", 
           method="square", 
           outline.color="black",
           ggtheme=theme_bw)

# con la libreria corrplot
corrplot(corr_datos, 
         type = "upper",
         method = "number")

# otro grafico, con mas opciones
corrplot.mixed(corr_datos, 
               lower.col = "black", 
               number.cex = .7)



# Para guardar/visualizar multiples paneles en ggplot 2
# Ejemplo con 4 graficos

grafico_a <- ggplot(Datos,aes(LongSepalo,AnchoSepalo))+ 
  theme_classic() + 
  labs(x="Longitud del Sepalo (cm)",y="Ancho del Sepalo (cm)") + 
  geom_point(size=2) 
grafico_a

# cambiando el color de los puntos y agragando leyenda
grafico_b <- ggplot(Datos,aes(LongSepalo,AnchoSepalo))+ 
  theme_classic() + 
  labs(x="Longitud del Sepalo (cm)",y="Ancho del Sepalo (cm)") + 
  geom_point(size=2, aes(color=especie)) + theme(legend.title = element_blank())
grafico_b

# cambiando la forma de los puntos  
grafico_c <- ggplot(Datos,aes(LongSepalo,AnchoSepalo))+ 
  theme_classic() + 
  labs(x="Longitud del Sepalo (cm)",y="Ancho del Sepalo (cm)") + 
  scale_color_manual(values=c("red","green","orange"))+ 
  geom_point(size=2, aes(shape=especie)) + theme(legend.title = element_blank())
grafico_c

# idem mas suavizado
grafico_d <- ggplot(Datos,aes(LongSepalo,AnchoSepalo))+ 
  theme_classic() + 
  labs(x="Longitud del Sepalo (cm)",y="Ancho del Sepalo (cm)") + 
  geom_point(size=2, aes(color=especie)) + theme(legend.title = element_blank()) +
  facet_grid(~ especie, scales="free_x") +
  geom_smooth(method = "lm",se=T, color="red")
grafico_d
# Lo importante para que se separe en 3 rectas::: facet_grid

# funcion grid.arrange

grid.arrange(grafico_a, grafico_b,
             grafico_c, grafico_d, 
             ncol=2, nrow=2)

