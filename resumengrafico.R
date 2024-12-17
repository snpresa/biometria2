# Manejo de los datos, exploración grafica, estadistica descriptiva
####Librerias####
library(ggplot2)      # Graficos con ggplot
library(ggcorrplot)   # Correlaciones con libreria ggcorrplot
library(corrplot)     # Correlaciones con libreria corrplot 
library(gridExtra)    # Visualizacion en paneles de graficos con ggplot
library(GGally)
library(gridExtra) # me permite agrupar graficos ggplot grid.arrange(graf1,graf2)

#### Modificacion de datos####
# Opcional dar vuelta los niveles de densidad (puede resultar util sobre el final del ejercicio)
Datos$densidad <- factor(Datos$densidad, levels = c("5_plantas", "1_planta"))

#### GRAFICOS Y CORRELACION####
# Ver relacion entre variables
ggpairs(Datos[,2:5]) # de ggally

# ggplot2
#Diagramas de dispersion
# Con una recta suavizada
ggplot(Datos, aes(LongSepalo, AnchoPetalo)) +
  geom_point(color = "black")  + 
  geom_smooth(color = "red")
# Coloreando segun especie
ggplot(Datos,aes(LongSepalo,AnchoSepalo))+ 
  labs(x="Longitud del Sepalo (cm)",y="Ancho del Sepalo (cm)") + 
  geom_point(size=2, aes(color=especie)) 
# Agregando una recta de lm
# setando geom_smooth
grafico2 <- ggplot(Datos, aes(x=LongSepalo, y=AnchoPetalo)) + 
  geom_point(size=3, color="red", shape=19) +  
  geom_smooth(method=lm, se=T, fullrange=F, size=0.5) 
grafico2
# Cambiando la forma de los puntos
grafico_c <- ggplot(Datos,aes(LongSepalo,AnchoSepalo))+ 
  theme_classic() + 
  labs(x="Longitud del Sepalo (cm)",y="Ancho del Sepalo (cm)") + 
  scale_color_manual(values=c("red","green","orange"))+ 
  geom_point(size=2, aes(shape=especie)) + theme(legend.title = element_blank())
grafico_c
## modificando los limites de y
disp <- ggplot(datos, aes(x=Cd, y=Cdenplanta)) + 
  geom_point(size=1.5, color="red", shape=19) +  
  geom_smooth(method=lm, se=F, fullrange=F, size=0.5)+ 
  xlab("Cd suministrado (uM)") +  ylab("Cd acumulado (mg/kg)") +
  ylim(0,800)
disp

# Separando los graficos 
grafico_d <- ggplot(Datos,aes(LongSepalo,AnchoSepalo))+ 
  theme_classic() + 
  labs(x="Longitud del Sepalo (cm)",y="Ancho del Sepalo (cm)") + 
  geom_point(size=2, aes(color=especie)) + theme(legend.title = element_blank()) +
  facet_grid(~ especie, scales="free_x") +
  geom_smooth(method = "lm",se=T, color="red")
grafico_d
# Lo importante para que se separe en 3 rectas::: facet_grid

# Boxplot

box <- ggplot(Datos, aes(x=especie, y=AnchoSepalo))+        
  geom_boxplot()+ 
  stat_summary(fun = mean, geom="point", shape=8, size=4,color="darkcyan")
box
# Por lo que probe, fun es el punto. Uno puede pedirle que sea la media. Shape le dice que forma tiene el punto

ggplot(data=datos, aes(x=fertilizacion, y=cra, colour=riego))+geom_boxplot()+geom_jitter(alpha=0.3, size=2)+ labs(title="Efecto de la fertilizacion y el riego sobre el CRA")


# Haciendo mas informativo el boxplot
box2 <- ggplot(Datos, aes(x=especie, y=LongSepalo)) +
  geom_boxplot(aes(color=especie), color="black")+
  theme_bw()+
  geom_jitter(alpha=0.3, size=2,aes(color=especie), position = position_jitter(width = .2))+theme(legend.position="top", legend.text=element_text(size = 14),legend.title = element_text(size=16, face="bold")) +
  ylab("Long Sepalo")+xlab("Especie")
box2

# histograma
hist <- ggplot(Datos, aes(x=LongSepalo))+
  geom_histogram(binwidth=.5, fill="blue", colour="black")+
  theme_bw()
hist


#### Asociacion entre variables 

# Primero hay que armar la matriz de correlacion 
# Calculo de correlacion
cor(datos$DI, datos$RU) #0.987
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

#### Grafico media vs varianza####
# Grafico media vs varianza
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

#### Grafico de perfiles y Spaghetti ####
ggplot(data = datos, aes(x = nieve, y = HR, fill = areas)) +
  geom_line(aes(group = areas, colour = areas), size=1) +labs(title = "Home Range por area y condicion de nieve", x = "Condicion de nieve",    y = "HR(km2") + theme_bw()

ggplot(data = datos, aes(x = riego, y = cra, fill = fertilizacion)) +
  geom_smooth(aes(group = fertilizacion, colour = fertilizacion), se = FALSE) +labs(title = "CRA por riego y por fertilizacion", x = "riego",    y = "CRA(%)") + theme_bw()
# Por camada
ggplot(data = datos, aes(x = Hormona,
                         y = GanPeso,
                         colour = Camada,
                         group = Camada)) +
  labs(title = "Spaghetti-plot",
       x = "Hormona",
       y = "Ganancia de peso") +
  geom_point() +
  geom_line() +
  #  facet_grid(. ~Hormona ) +
  theme_bw()
ggplot(data = datos, aes(x = tiempo,
                         y = t_reac,
                         colour = individuo,
                         group = individuo)) +
  labs(title = "Spaghetti-plot",
       x = "Tiempo",
       y = "tiempo de rta") +
  geom_point() +
  geom_line() +
  facet_grid(. ~tratamiento ) +
  theme_bw()
# Grafico de dispersion con camadas
ggplot(bd, aes(etanol, vol)) + 
  geom_point(aes(colour=camada)) + 
  labs(x="dosis etanol (g/kg)", y="volumen cerebro (cm3)") + 
  geom_line(aes(colour=camada))

#### Boxplot residuos segun tratamiento cuando hay heterocedasticidad####
# Graficar un boxplot de los residuos del modelo. Este grafico esta bueno
boxplot(r2 ~ Datos$Tratamiento,
        xlab = "Tratamiento",
        ylab = "Residuos estandarizados")

####TABLAS RESUMEN Y FRECUENCIAS####
datos %>% 
  group_by(especie) %>%
  summarise_all(.funs = c(
    media = mean
  )
  )

# datos resumen
resumen <- Datos %>% 
  group_by(Tratamiento) %>%
  summarise_all(.funs = c(
    media = mean, 
    DE = sd, 
    min = min, 
    max = max
  )
  )

# tabla de frecuencias INCLUYE NAs

tabla_frecuencias <- table(datos$especie, useNA = "always")
tabla_frecuencias

# frecuencias relativas
prop.table(tabla_frecuencias)
