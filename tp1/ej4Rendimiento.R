# Problema: Rendimiento de girasol en la provincia de Bs As

# ******** #
# Parte A  #
# ******** #


### carga de data frame
setwd("C:/Users/snpre/Downloads")
# setear el directorio de trabajo o escribir toda la ruta
datos  <- read.delim("misdatos_ej4.txt", sep = " ")

### exploracion de data.frame
str(datos) # 72 obs de 2 variables
summary(datos) # Ambas son numericas

### Gráfico de dispersión
par(mfrow=c(1,1))
plot(Rend~Fert, datos, 
     ylab = "Rendimimiento (kg/ha)", 
     xlab="gramos de nuevo Fertilizante / 100 gr de mezcla", 
     main="Rendimiento en fc de nuevo fertilizante en la mezcla", 
     cex.axis=0.5,
     cex.lab=0.5,
     cex.main=0.7)

### con ggplot2
library(ggplot2)
ggplot(data = datos, mapping = aes(x = Fert, y = Rend)) +
  geom_point()

# con más estética
ggplot(data = datos, mapping = aes(x = Fert, y = Rend)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Rendimiento en fc de nuevo fertilizante en la mezcla", x ="gramos de nuevo Fertilizante / 100 gr de mezcla", y = "Rendimiento (kg/ha)") 




### Ajuste del modelo
m1 <- lm(Rend ~ Fert, data=datos)

## Recta a estimar

# Rendimiento = B0 + B1 * DosisFert + ei

### resultados
summary(m1)
# Rendimiento = 487.7 + 25.46 * DosisFert
coef(m1)
confint(m1)
#                2.5 %    97.5 %
#(Intercept) 327.44748 647.94802
#Fert         22.09208  28.82393

### Todas las estimaciones 

### completar la planilla con los valores estimados de beta 1, beta0 y R2: 
#  https://docs.google.com/spreadsheets/d/1bymdBrTqOYl1ow_LwiTxaWoDEHvGy4nBPG_FfURmxHQ/edit?gid=0#gid=0

# utilizar "," para separador decimal 


### Decargar la planilla con los datos (.csv) de todo el curso y llamarla "Rendimiento2023Todos"

Rendimiento2024Todos<-read.csv("Rendimiento2024Todos - Hoja 1.csv", dec = ",")
summary(Rendimiento2024Todos)
### Construir un histograma con los valores estimados para β1 por todo el curso

hist(Rendimiento2024Todos$EstimadorBeta1, 
     main="muchas estimaciones", 
     xlab="valores de pendiente estimada", c="maroon")



# ******** #
# Parte B  #
# ******** #

## Sabiendo que los valores de los parámetros poblacionales son:  β0 =615 y β1 = 24. ¿Cuántos IC del curso contienen a β1? ¿Resultó lo esperado?

sum(Rendimiento2024Todos$LI<24 & Rendimiento2024Todos$LS>24, na.rm = TRUE)
# 41/43
sum(Rendimiento2024Todos$ContieneaBeta1...1..SI..0.NO., na.rm=TRUE)

41/43*100 # 95.35
## Ubicar el valor de β1 en el histograma construido en la parte anterior 

hist(Rendimiento2023Todos$EstimadorBeta1, main=" ", xlab="b1")
abline(v=24,col="red")

# ******** #
# Parte C  #
# ******** #

#1. Siguiendo el script proporcionado, simulamos la obtención de tantas muestras como queramos, para:
#Estimar muchas pendientes y calcular los IC95% para cada valor de la pendiente estimado y ver si se cumple empìricamente que de 100 intervalos 95 van a contener al parámetro y 5 no. Compruebe mediante una tabla y gráfico si se cumple aproximadamente lo esperado

set.seed(125) # 

## Parámetros del modelo
beta0 = 615                                          # Coeficiente beta0
beta1 = 24                                           # Coeficiente beta1
sigma = sqrt(120000)                                 # Desvio estandar residual

## Codigo para obtención de la muestra a partir del modelo

nivelesX = 9                                         # valores de x
repeticiones = 8                                     # repeticiones por valor de x 
n=nivelesX*repeticiones                              # n=Cantidad de datos total
X <- rep(seq(0, 80, length=nivelesX), repeticiones)  # Genera una secuencia de valores de X  
e = rnorm(n,0,sigma)                                 # Genera errores aleatorios rnorm (e)
Y = beta0 + beta1*X + e                              # Estima valores de Y (Rendimiento), según el modelo 


## Ajustar un modelo 
model <- lm(Y~X)

## explorar resultados
summary(model)
confint(model)


## explorar el objeto "model"

str(model)  ## contiene mucha informacion ...
# tambien se puede ver:
View(model)

## codigo para extraer algunos valroes estimados .... cuales?

model$coefficients
model$coefficients[2]
summary(model)[[4]][4]
confint(model)[2,][1] 
confint(model)[2,][2] 



### Codigo para obtener valores de pendiente estimada y su error estándar y guardarlos en un data.frame

data<-data.frame()            
for(i in 1:1000) {             
  X <- rep(seq(0, 80, length=nivelesX), repeticiones)           
  e = rnorm(n,0,sigma)        
  Y = beta0 + beta1*X + e
  model<-lm(Y~X)
  b1_se <- c(i, model$coefficients[2],summary(model)[[4]][4]) 
  data<-cbind(rbind(data, b1_se))              
}
colnames(data)<-c("simulacion", "b1","se")   
head(data)                                               

### Graficar en un histograma los valores de las pendientes estimadas y sus errores estándar 

par(mfrow=c(1,2))
hist(data$b1,freq=FALSE, xlab="b1", main="Valores de b1")
abline(v=beta1,col="red")
hist(data$se,freq=FALSE, xlab="se de b1", main = "valores de se")
abline(v=mean(data$se),col="blue")  



### Codigo para obtener lo mismo, y además los límites del IC95% para el valor de cada estimación de la pendiente

data<-data.frame()            
for(i in 1:100) {             
  X <- rep(seq(0, 80, length=nivelesX), repeticiones)           
  e = rnorm(n,0,sigma)        
  Y = beta0 + beta1*X + e
  model<-lm(Y~X)
  b1_se_IC <- c(i, model$coefficients[2],summary(model)[[4]][4], 
                confint(model)[2,][1], confint(model)[2,][2]) 
  data<-cbind(rbind(data, b1_se_IC))              
}
colnames(data)<-c("simulacion", "b1","se", "LI", "LS")   
head(data) 

### Agregar una columna indicando si el IC95% contiene o no (TRUE/FALSE) al parametro


data$ContieneABeta1 <- data$LI < 24 & data$LS > 24

head(data, n=10)   


### Cuantos IC$_{95}$ contienen al parámetro y cuantos no? 

mean(data$ContieneABeta1)


### Cuantos IC$_{95}$ contienen al parámetro y cuantos no? 
  
table(data$ContieneABeta1, useNA = "always")


### Graficos

par(mfrow=c(1,1))
plot(1:100, type = "n",
     xlim = range(min(data$LI), max(data$LS)),
     ylab = "Simulacion",
     xlab= "estimador de beta 1")

abline(v = beta1, lty = 2, col="blue") ## la media poblacional.

segments(data$LI[1], 1, data$LS[1], 1)

for(i in 1:nrow(data)) {
  segments(data$LI[i], i, data$LS[i], i,
           col = (data$ContieneABeta1[i]+2))
}




## Con ggplot2

library(ggplot2)
ggplot(data, aes(x=simulacion, y=b1, colour=ContieneABeta1)) + 
  geom_point() +
  geom_errorbar(aes(ymin=LI, ymax=LS),width=.2) +
  geom_hline(yintercept=24, linetype="dashed", color = "blue") +
  annotate("text", x = -5, y = 24.5, label = "Beta1", colour="blue") 



