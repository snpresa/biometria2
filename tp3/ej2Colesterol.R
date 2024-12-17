rm(list=ls())
library(nlme)
library(car)
library(emmeans)
# Problema 2:  Efecto de la fracción lipoproteica HDL sobre la progresión de las lesiones ateroescleróticas en conejos alimentados con dieta rica en colesterol

# dieta rica en colesterol 0,5% durante 60 días (Dieta 1).
# misma dieta durante 90 días (Dieta 2). 
#misma dieta durante 90 días, pero recibió 50 mg de proteína HDL-VHDL por semana durante los últimos 30 días (Dieta 3).

#Se midió el contenido de colesterol en aorta (en mg/g).

#1. Identifique la variable respuesta y explicativa. ¿Cuál es el diseño experimental? ¿Cuál es la cantidad de réplicas?

# La variable respuesta es contenido de colesterol en aorta (mg/g), es cuantitativa continua por lo que proponemos una distirbucion normal
# La variable explicativa es dieta, es categorica y tiene 3 niveles. No tiene sentido que sea un entero asi que la transformare en factor

datos<- read.delim("Colesterol.txt")
head(datos)
str(datos)
names(datos)

datos$Dietaf<-factor(datos$Dieta)
summary(datos)

#Podemos ver que hay 8 replicas por tratamiento(dieta)

#2. Escriba el modelo en parámetros y en términos del problema, indicando los supuestos distribucionales

# tenemos una comparacion de medias ya que nuestra VE es cualitativa

# Colesterol_ij = μ + αi + εij

#donde i va del 1 al 3
# j del 1 al 8

# εij ~ Normal (0, σ^2)
# μ representa al colesterol medio poblacional
# αi representa el efecto del tratamiento i

# 3. Concluya en relación al efecto de la dieta rica en colesterol y la suministración adicional de la proteína HDL-VHDL. Igual que antes, antes de efectuar las pruebas evalúe los supuestos del modelo.

m<-gls(colesterol~ Dietaf, data=datos)

par(mfrow=c(1,2))
#plot(m)
r2 <- residuals(m, type = "pearson") # = estandarizados.
pred2 <- fitted(m)

# Graficamente
plot(x = pred2,
     y = r2,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")

abline(h = 0, lty = 2)
qqPlot(r2)

# Analiticamente
leveneTest(r2, datos$Dietaf) # rechazo 0.04396
shapiro.test(r2) #0.8843

# Se observa grafica y analiticamente evidencias de heterocedasticidad mienstras que no hay evidencias que que no se cumpla normalidad
# Rechazo la h0 de Levene pero no la de Shapiro

par(mfrow=c(1,1))
boxplot(r2 ~ datos$Dietaf,
        xlab = "Dietaf",
        ylab = "Residuos estandarizados")

media <- matrix(tapply(datos$colesterol, datos$Dietaf, mean))
varianza <- matrix(tapply(datos$colesterol, datos$Dietaf, var))
df<-as.data.frame(media)
colnames(df) <- c("media")
df$varianza<-round(varianza,1)
dieta <- c("1", "2", "3")  
df$dieta <- dieta
df                # Exploro el data.frame


ggplot(df,aes(x=media, y=varianza, color="maroon"))+geom_point(size=4)+annotate("text", x=9,  y=3,   label= "3", size=4) + annotate("text", x=16.3, y=9, label= "1", size=4) +  annotate("text", x=19, y=24.5, label= "2", size=4) 


# MODELO VARIANZA:
m_varIdent <- gls(colesterol ~ Dietaf,
                       weights = varIdent(form = ~1 | Dietaf),
                       data = datos)
# Supuestos:
par(mfrow=c(1,2))
#plot(m)
rVI <- residuals(m_varIdent, type = "pearson") # = estandarizados.
predVI <- fitted(m_varIdent)
# Graficamente
plot(x = predVI,
     y = rVI,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")

abline(h = 0, lty = 2)
qqPlot(rVI)
# Analiticamente
leveneTest(rVI, datos$Dietaf) # 0.9443
shapiro.test(rVI) #0.4696

# Es modelo candidato, se cumplen los supuestos

## VAR POWER

m_varPower <- gls(colesterol ~ Dietaf,
                       weights = varPower(),
                       data = datos)

# Supuestos:
par(mfrow=c(1,2))
#plot(m)
rVP <- residuals(m_varPower, type = "pearson") # = estandarizados.
predVP <- fitted(m_varPower)
# Graficamente
plot(x = predVP,
     y = rVP,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")

abline(h = 0, lty = 2)
qqPlot(rVP)
# Analiticamente
leveneTest(rVP, datos$Dietaf) # 0.8205
shapiro.test(rVP) #0.6868

# Es modelo candidato, cumple los supuestos

## VAR EXP
m_varExp <- gls(colesterol ~ Dietaf,
                     weights = varExp(),
                     data = datos)
# Supuestos:
par(mfrow=c(1,2))
#plot(m)
rVE <- residuals(m_varExp, type = "pearson") # = estandarizados.
predVE <- fitted(m_varExp)
# Graficamente
plot(x = predVE,
     y = rVE,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")

abline(h = 0, lty = 2)
qqPlot(rVE)
# Analiticamente
leveneTest(rVE, datos$Dietaf) # 0.8997
shapiro.test(rVE) #0.6565

# Tambien es candidato, se cumplen los supuestos


###### SELECCION DE MODELOS
AIC(m_varIdent, m_varPower, m_varExp)

# El modelo de menor air es VAREXP. Selecciono ese modelo. (podria haber usado otro, el Akaike no es tan distinto)

###### Ahora si, me quede con m_VarExp

anova(m_varExp)

# Por lo menos alguna dieta difiere de la media!!, me dio significativo. Como hay 3 dietas posibles tengo que hacer contrastes
options(emmeans= list(emmeans = list(infer = c(TRUE, F)),
                      contrast = list(infer = c(TRUE, TRUE)))) # opciones de seteo de las opciones de salida


comp <- emmeans(m_varPower, pairwise ~ Dietaf)
comp # La dieta 3 difiere de la 1 y 2 pero la 1 y 2 no difieren entre si

plot(comp$emmeans, comparisons = TRUE)


# En conclusion, una dieta rica en colesterol de 60 dias no cambia los valores de colesterol depositado en la aorta de una de 90. En cambio, agregar proteina VDL-HDLV es efectivo para reducir el colesterol en aorta (NO SE REPORTAR POTENCIA).

anova(m) # Sin cumplir supuestos el anova da significativo
summary(m)
summary(m_varExp)

# Los parametros dan igual?

comp2 <- emmeans(m, pairwise ~ Dietaf)
comp2

# Me da que la dieta 3 difiere de 1 y 2 pero los p valores dan mas grandes-- Nu se
