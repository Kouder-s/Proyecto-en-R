#Instalamos los paquetes rstatix y tidyverse

install.packages("rstatix")
install.packages("tidyverse")
install.packages("ggplot2")

#Llamamos a las librerías "rstatix" y "tidyverse"

library(rstatix)
library(tidyverse)
library(ggplot2)

#Leemos la base de datos

BASE.JUGADORES.TOTALES <- read.csv2("BASE JUGADORES TOTALES.csv")

#Tomamos las variables que vamos a trabajar y lsa generamos en un dataframe

Solotarjetasrojas <- select(BASE.JUGADORES.TOTALES, Equipo, Tarjetas_rojas)

aggregate(Tarjetas_rojas ~ Tarjetas_rojas, Solotarjetasrojas, sum)

#Convetimos las variables en vectores

Equipos <- c("Bayern Munich","Benfica","Chelsea","Inter", "Manchester City", "Milan", "Napoli", "Real Madrid")
Tarjetas_rojas_equipos <- c(1,0,1,0,1,1,1,0)
Tarjetas_rojas_totales_equipos <- c(5,5,5,5,5,5,5,5)

#Generamos el dataframe definitivo

Solo_tarjetas_rojas. <- data.frame(Equipos, Tarjetas_rojas_equipos, Tarjetas_rojas_totales_equipos)

#Calculamos la proporción de tarjetas rojas por equipo 

Solo_tarjetas_rojas. %>% 
  group_by(Tarjetas_rojas_equipos) %>% 
  summarise(n= Tarjetas_rojas_equipos) %>% 
  mutate(proporción = n / 5, porciento = proporción*100)

#Covertimos a vectores para generar un nuevo dataframe para las gráficas

Equipo <- c("Bayern Munich","Chelsea","Manchester City", "Milan", "Napoli")
Proporción_de_Tarjetas_rojas. <- c(0.2,0.2,0.2,0.2,0.2)

#Generamos el nuevo dataframe para la gráfica de proporción de las tarjetas rojas por equipos

Proporción_Tarjetas_Rojas <- data.frame(Equipo, Proporción_de_Tarjetas_rojas.)

#Generamos el gráfico de barras de las proporciones

ggplot(Proporción_Tarjetas_Rojas, aes(x = "" , y = Proporción_de_Tarjetas_rojas., fill = Equipo)) +
  geom_col(color = "black") +
  geom_text(aes(label = Proporción_de_Tarjetas_rojas.),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") 