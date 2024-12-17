# Ejercicio 4:  Exposición prenatal a mercurio y actividad de la bomba de calcio

rm(list=ls())
setwd("/cloud/project")
data<-read.table("pelo.txt")
View(data)

install.packages("ggplot2")
library(ggplot2)

#1- Represente los datos en un diagrama de dispersión. ¿Existe una tendencia lineal en los datos? ¿Directa o inversa? ¿Débil o fuerte?

ggplot(data=data, aes(x=Hg_pelo,y=act_bomba))+geom_point(size=1.5, color="darkred")+theme_bw() + labs(x="Concentracion de mercurio en el pelo",y="Actividad de bomba de calcio")

cor(data$Hg_pelo,data$act_bomba) #-0.86, inversa, fuerte

# 2) Estime la relación funcional. Evalúe los supuestos del modelo

modelo1<-lm(act_bomba~Hg_pelo, data=data)
summary(modelo1)

# chequeo homocedasticidad




# 3- Interprete la pendiente en contexto. ¿Cuáles son sus unidades?



#4- ¿Existe evidencia significativa de que la actividad basal de la bomba de Ca en el recién nacido disminuye linealmente con el nivel de mercurio en el pelo materno?


#5- Sobre la base de su respuesta en (4) y el diseño de este estudio (experimental vs observacional), ¿qué puede decir con respecto a la pregunta de investigación sobre si la exposición materna al mercurio medida por depósitos de mercurio en cabello afecta la actividad de la bomba de Ca en recién nacidos?


