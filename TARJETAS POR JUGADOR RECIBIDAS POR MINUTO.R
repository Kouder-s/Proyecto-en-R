#Cargamos e instalamos las librerias
install.packages("tidyverse")
install.packages("skimr")
install.packages("ggplot2")
install.packages("dplyr")

library(skimr)
library(ggplot2)
library(dplyr)
library(tidyverse)

#CARGAMOS LA BASE DE DATOS

BASEJUGADORESTOTALES <- read.csv("BASE JUGADORES TOTALES.csv", sep = ";", header = TRUE)

#REDUCIMOS A LOS CARACTERES NECESARIOS

BASEFINAL <- select(BASEJUGADORESTOTALES, Jugador, Tarjetas_amarillas, Tarjetas_rojas, Minutos_totales )

#GENERAMOS EL DATAFRAME PARA CALCULAR LAS TARJETAS RECIBIDAS POR MINUTO
#UTILIZAMOS UN FILTRO PARA QUE TENER SOLO LOS >0

RESULTADOS <- BASEFINAL %>%
  mutate(tarjetas_recibidas = Tarjetas_amarillas + Tarjetas_rojas) %>%
  mutate(tarjetas_por_minuto = tarjetas_recibidas / Minutos_totales) %>%
  filter(tarjetas_por_minuto > 0) %>%
  arrange(desc(tarjetas_por_minuto))

# Calculamos la suma de todos los minutos totales y la suma de las tarjetas recibidas

suma_minutos <- sum(RESULTADOS$Minutos_totales)
suma_tarjetas <- sum(RESULTADOS$Tarjetas_amarillas) + sum(RESULTADOS$Tarjetas_rojas)

# Calculamos el promedio

promedio_tarjetas_por_minuto <- suma_tarjetas / suma_minutos

# Agregamos una fila adicional a la base de datos RESULTADOS

RESULTADOS <- RESULTADOS %>%
  add_row(
    Jugador = "Promedio",
    Tarjetas_amarillas = NA,
    Tarjetas_rojas = NA,
    Minutos_totales = suma_minutos,
    tarjetas_recibidas = suma_tarjetas,
    tarjetas_por_minuto = promedio_tarjetas_por_minuto
  )

# Tabla descriptiva

skim(RESULTADOS)

#GRAFICOS

#Gráfico de dispersión: Tarjetas recibidas por minuto vs Minutos totales

ggplot(RESULTADOS, aes(x = Minutos_totales, y = tarjetas_por_minuto)) + 
  geom_point() + 
  labs(x = "Minutos totales", y = "Tarjetas recibidas por minuto") + 
  theme_classic()

#Gráfico de histograma: Distribución de tarjetas recibidas por minuto

ggplot(RESULTADOS, aes(x = tarjetas_por_minuto)) + 
  geom_histogram(bins = 30) + 
  labs(x = "Tarjetas recibidas por minuto", y = "Frecuencia") + 
  theme_classic()


#CARGAMOS LA BASE DE DATOS DE CADA EQUIPO

BASEJUDADORESBAYERN<- read.csv("BASE JUGADORES BAYERN MUNINCH.csv", sep = ";", header = TRUE)

BASEJUDADORESBENFICA<- read.csv("BASE JUGADORES BENFICA.csv", sep = ";", header = TRUE)

BASEJUDADORESCHELSEA<- read.csv("BASE JUGADORES CHELSEA.csv", sep = ";", header = TRUE)

BASEJUDADORESINTER<- read.csv("BASE JUGADORES INTER.csv", sep = ";", header = TRUE)

BASEJUDADORESCITY<- read.csv("BASE JUGADORES MANCHESTER CITY.csv", sep = ";", header = TRUE)

BASEJUDADORESMILAN<- read.csv("BASE JUGADORES MILAN.csv", sep = ";", header = TRUE)

BASEJUDADORESNAPOLI<- read.csv("BASE JUGADORES NAPOLI.csv", sep = ";", header = TRUE)

BASEJUDADORESMADRID<- read.csv("BASE JUGADORES REAL MADRID.csv", sep = ";", header = TRUE)


#REDUCIMOS A LOS CARACTERES NECESARIOS

JUGADORESBAYERN <- select(BASEJUDADORESBAYERN, Jugador, Tarjetas_amarillas, Tarjetas_rojas, Minutos_totales )

JUGADORESBENFICA <- select(BASEJUDADORESBENFICA, Jugador, Tarjetas_amarillas, Tarjetas_rojas, Minutos_totales )

JUGADORESCHELSEA <- select(BASEJUDADORESCHELSEA, Jugador, Tarjetas_amarillas, Tarjetas_rojas, Minutos_totales )

JUGADORESINTER <- select(BASEJUDADORESINTER, Jugador, Tarjetas_amarillas, Tarjetas_rojas, Minutos_totales )

JUGADORESCITY <- select(BASEJUDADORESCITY, Jugador, Tarjetas_amarillas, Tarjetas_rojas, Minutos_totales )

JUGADORESMILAN <- select(BASEJUDADORESMILAN, Jugador, Tarjetas_amarillas, Tarjetas_rojas, Minutos_totales )

JUGADORESNAPOLI <- select(BASEJUDADORESNAPOLI, Jugador, Tarjetas_amarillas, Tarjetas_rojas, Minutos_totales )

JUGADORESMADRID <- select(BASEJUDADORESMADRID, Jugador, Tarjetas_amarillas, Tarjetas_rojas, Minutos_totales )

#GENERAMOS EL DATAFRAME PARA CALCULAR LAS TARJETAS RECIBIDAS POR MINUTO
#UTILIZAMOS UN FILTRO PARA QUE TENER SOLO LOS >0

RESULTADOSBAYERN <- JUGADORESBAYERN %>%
  mutate(tarjetas_recibidas = Tarjetas_amarillas + Tarjetas_rojas) %>%
  mutate(tarjetas_por_minuto = tarjetas_recibidas / Minutos_totales) %>%
  filter(tarjetas_por_minuto > 0) %>%
  arrange(desc(tarjetas_por_minuto))

RESULTADOSBENFICA <- JUGADORESBENFICA %>%
  mutate(tarjetas_recibidas = Tarjetas_amarillas + Tarjetas_rojas) %>%
  mutate(tarjetas_por_minuto = tarjetas_recibidas / Minutos_totales) %>%
  filter(tarjetas_por_minuto > 0) %>%
  arrange(desc(tarjetas_por_minuto))

RESULTADOSCHELSEA <- JUGADORESCHELSEA %>%
  mutate(tarjetas_recibidas = Tarjetas_amarillas + Tarjetas_rojas) %>%
  mutate(tarjetas_por_minuto = tarjetas_recibidas / Minutos_totales) %>%
  filter(tarjetas_por_minuto > 0) %>%
  arrange(desc(tarjetas_por_minuto))

RESULTADOSCITY <- JUGADORESCITY %>%
  mutate(tarjetas_recibidas = Tarjetas_amarillas + Tarjetas_rojas) %>%
  mutate(tarjetas_por_minuto = tarjetas_recibidas / Minutos_totales) %>%
  filter(tarjetas_por_minuto > 0) %>%
  arrange(desc(tarjetas_por_minuto))

RESULTADOSINTER <- JUGADORESINTER %>%
  mutate(tarjetas_recibidas = Tarjetas_amarillas + Tarjetas_rojas) %>%
  mutate(tarjetas_por_minuto = tarjetas_recibidas / Minutos_totales) %>%
  filter(tarjetas_por_minuto > 0) %>%
  arrange(desc(tarjetas_por_minuto))

RESULTADOSMADRID <- JUGADORESMADRID %>%
  mutate(tarjetas_recibidas = Tarjetas_amarillas + Tarjetas_rojas) %>%
  mutate(tarjetas_por_minuto = tarjetas_recibidas / Minutos_totales) %>%
  filter(tarjetas_por_minuto > 0) %>%
  arrange(desc(tarjetas_por_minuto))

RESULTADOSMILAN <- JUGADORESMILAN %>%
  mutate(tarjetas_recibidas = Tarjetas_amarillas + Tarjetas_rojas) %>%
  mutate(tarjetas_por_minuto = tarjetas_recibidas / Minutos_totales) %>%
  filter(tarjetas_por_minuto > 0) %>%
  arrange(desc(tarjetas_por_minuto))

RESULTADOSNAPOLI <- JUGADORESNAPOLI %>%
  mutate(tarjetas_recibidas = Tarjetas_amarillas + Tarjetas_rojas) %>%
  mutate(tarjetas_por_minuto = tarjetas_recibidas / Minutos_totales) %>%
  filter(tarjetas_por_minuto > 0) %>%
  arrange(desc(tarjetas_por_minuto))

# Calculamos la suma de todos los minutos totales y la suma de las tarjetas recibidas de cada equipo 

suma_minutosBAYERN <- sum(RESULTADOSBAYERN$Minutos_totales)
suma_tarjetasBAYERN <- sum(RESULTADOSBAYERN$Tarjetas_amarillas) + sum(RESULTADOSBAYERN$Tarjetas_rojas)

suma_minutosBENFICA <- sum(RESULTADOSBENFICA$Minutos_totales)
suma_tarjetasBENFICA <- sum(RESULTADOSBENFICA$Tarjetas_amarillas) + sum(RESULTADOSBENFICA$Tarjetas_rojas)

suma_minutosCHELSEA <- sum(RESULTADOSCHELSEA$Minutos_totales)
suma_tarjetasCHELSEA <- sum(RESULTADOSCHELSEA$Tarjetas_amarillas) + sum(RESULTADOSCHELSEA$Tarjetas_rojas)

suma_minutosCITY <- sum(RESULTADOSCITY$Minutos_totales)
suma_tarjetasCITY <- sum(RESULTADOSCITY$Tarjetas_amarillas) + sum(RESULTADOSCITY$Tarjetas_rojas)

suma_minutosINTER <- sum(RESULTADOSINTER$Minutos_totales)
suma_tarjetasINTER <- sum(RESULTADOSINTER$Tarjetas_amarillas) + sum(RESULTADOSINTER$Tarjetas_rojas)

suma_minutosMADRID <- sum(RESULTADOSMADRID$Minutos_totales)
suma_tarjetasMADRID <- sum(RESULTADOSMADRID$Tarjetas_amarillas) + sum(RESULTADOSMADRID$Tarjetas_rojas)

suma_minutosMILAN <- sum(RESULTADOSMILAN$Minutos_totales)
suma_tarjetasMILAN <- sum(RESULTADOSMILAN$Tarjetas_amarillas) + sum(RESULTADOSMILAN$Tarjetas_rojas)

suma_minutosNAPOLI <- sum(RESULTADOSNAPOLI$Minutos_totales)
suma_tarjetasNAPOLI <- sum(RESULTADOSNAPOLI$Tarjetas_amarillas) + sum(RESULTADOSNAPOLI$Tarjetas_rojas)


# Calculamos el promedio por cada equipo

promedio_tarjetas_por_minutoBAYERN <- suma_tarjetasBAYERN / suma_minutosBAYERN
promedio_tarjetas_por_minutoBENFICA <- suma_tarjetasBENFICA / suma_minutosBENFICA
promedio_tarjetas_por_minutoCHELSEA <- suma_tarjetasCHELSEA / suma_minutosCHELSEA
promedio_tarjetas_por_minutoCITY <- suma_tarjetasCITY / suma_minutosCITY
promedio_tarjetas_por_minutoINTER <- suma_tarjetasINTER / suma_minutosINTER
promedio_tarjetas_por_minutoMADRID <- suma_tarjetasMADRID / suma_minutosMADRID
promedio_tarjetas_por_minutoMILAN <- suma_tarjetasMILAN / suma_minutosMILAN
promedio_tarjetas_por_minutoNAPOLI <- suma_tarjetasNAPOLI / suma_minutosNAPOLI

# Agregamos una fila adicional a la base de datos RESULTADOS por equipos

RESULTADOSBAYERN <- RESULTADOSBAYERN %>%
  add_row(
    Jugador = "Promedio",
    Tarjetas_amarillas = NA,
    Tarjetas_rojas = NA,
    Minutos_totales = suma_minutosBAYERN,
    tarjetas_recibidas = suma_tarjetasBAYERN,
    tarjetas_por_minuto = promedio_tarjetas_por_minutoBAYERN
  )

RESULTADOSBENFICA <- RESULTADOSBENFICA %>%
  add_row(
    Jugador = "Promedio",
    Tarjetas_amarillas = NA,
    Tarjetas_rojas = NA,
    Minutos_totales = suma_minutosBENFICA,
    tarjetas_recibidas = suma_tarjetasBENFICA,
    tarjetas_por_minuto = promedio_tarjetas_por_minutoBENFICA
  )

RESULTADOSCHELSEA <- RESULTADOSCHELSEA %>%
  add_row(
    Jugador = "Promedio",
    Tarjetas_amarillas = NA,
    Tarjetas_rojas = NA,
    Minutos_totales = suma_minutosCHELSEA,
    tarjetas_recibidas = suma_tarjetasCHELSEA,
    tarjetas_por_minuto = promedio_tarjetas_por_minutoCHELSEA
  )

RESULTADOSCITY <- RESULTADOSCITY %>%
  add_row(
    Jugador = "Promedio",
    Tarjetas_amarillas = NA,
    Tarjetas_rojas = NA,
    Minutos_totales = suma_minutosCITY,
    tarjetas_recibidas = suma_tarjetasCITY,
    tarjetas_por_minuto = promedio_tarjetas_por_minutoCITY
  )

RESULTADOSINTER <- RESULTADOSINTER %>%
  add_row(
    Jugador = "Promedio",
    Tarjetas_amarillas = NA,
    Tarjetas_rojas = NA,
    Minutos_totales = suma_minutosINTER,
    tarjetas_recibidas = suma_tarjetasINTER,
    tarjetas_por_minuto = promedio_tarjetas_por_minutoINTER
  )

RESULTADOSMADRID <- RESULTADOSMADRID %>%
  add_row(
    Jugador = "Promedio",
    Tarjetas_amarillas = NA,
    Tarjetas_rojas = NA,
    Minutos_totales = suma_minutosMADRID,
    tarjetas_recibidas = suma_tarjetasMADRID,
    tarjetas_por_minuto = promedio_tarjetas_por_minutoMADRID
  )

RESULTADOSMILAN <- RESULTADOSMILAN %>%
  add_row(
    Jugador = "Promedio",
    Tarjetas_amarillas = NA,
    Tarjetas_rojas = NA,
    Minutos_totales = suma_minutosMILAN,
    tarjetas_recibidas = suma_tarjetasMILAN,
    tarjetas_por_minuto = promedio_tarjetas_por_minutoMILAN
  )

RESULTADOSNAPOLI <- RESULTADOSNAPOLI %>%
  add_row(
    Jugador = "Promedio",
    Tarjetas_amarillas = NA,
    Tarjetas_rojas = NA,
    Minutos_totales = suma_minutosNAPOLI,
    tarjetas_recibidas = suma_tarjetasNAPOLI,
    tarjetas_por_minuto = promedio_tarjetas_por_minutoNAPOLI
  )

# Tabla descriptiva 

# BAYERN

skim(RESULTADOSBAYERN)

#GRAFICOS 

#Gráfico de dispersión: Tarjetas recibidas por minuto vs Minutos totales

ggplot(RESULTADOSBAYERN, aes(x = Minutos_totales, y = tarjetas_por_minuto)) + 
  geom_point() + 
  labs(x = "Minutos totales", y = "Tarjetas recibidas por minuto") + 
  theme_classic()

#Gráfico de histograma: Distribución de tarjetas recibidas por minuto

ggplot(RESULTADOSBAYERN, aes(x = tarjetas_por_minuto)) + 
  geom_histogram(bins = 30) + 
  labs(x = "Tarjetas recibidas por minuto", y = "Frecuencia") + 
  theme_classic()

# Tabla descriptiva 

# BENFICA

skim(RESULTADOSBENFICA)

#GRAFICOS 

#Gráfico de dispersión: Tarjetas recibidas por minuto vs Minutos totales

ggplot(RESULTADOSBENFICA, aes(x = Minutos_totales, y = tarjetas_por_minuto)) + 
  geom_point() + 
  labs(x = "Minutos totales", y = "Tarjetas recibidas por minuto") + 
  theme_classic()

#Gráfico de histograma: Distribución de tarjetas recibidas por minuto

ggplot(RESULTADOSBENFICA, aes(x = tarjetas_por_minuto)) + 
  geom_histogram(bins = 30) + 
  labs(x = "Tarjetas recibidas por minuto", y = "Frecuencia") + 
  theme_classic()

# Tabla descriptiva 

# CHELSEA

skim(RESULTADOSCHELSEA)

#GRAFICOS 

#Gráfico de dispersión: Tarjetas recibidas por minuto vs Minutos totales

ggplot(RESULTADOSCHELSEA, aes(x = Minutos_totales, y = tarjetas_por_minuto)) + 
  geom_point() + 
  labs(x = "Minutos totales", y = "Tarjetas recibidas por minuto") + 
  theme_classic()

#Gráfico de histograma: Distribución de tarjetas recibidas por minuto

ggplot(RESULTADOSCHELSEA, aes(x = tarjetas_por_minuto)) + 
  geom_histogram(bins = 30) + 
  labs(x = "Tarjetas recibidas por minuto", y = "Frecuencia") + 
  theme_classic()

# Tabla descriptiva 

#CITY

skim(RESULTADOSCITY)

#GRAFICOS 

#Gráfico de dispersión: Tarjetas recibidas por minuto vs Minutos totales

ggplot(RESULTADOSCITY, aes(x = Minutos_totales, y = tarjetas_por_minuto)) + 
  geom_point() + 
  labs(x = "Minutos totales", y = "Tarjetas recibidas por minuto") + 
  theme_classic()

#Gráfico de histograma: Distribución de tarjetas recibidas por minuto

ggplot(RESULTADOSCITY, aes(x = tarjetas_por_minuto)) + 
  geom_histogram(bins = 30) + 
  labs(x = "Tarjetas recibidas por minuto", y = "Frecuencia") + 
  theme_classic()

# Tabla descriptiva 

# INTER

skim(RESULTADOSINTER)

#GRAFICOS 

#Gráfico de dispersión: Tarjetas recibidas por minuto vs Minutos totales

ggplot(RESULTADOSINTER, aes(x = Minutos_totales, y = tarjetas_por_minuto)) + 
  geom_point() + 
  labs(x = "Minutos totales", y = "Tarjetas recibidas por minuto") + 
  theme_classic()

#Gráfico de histograma: Distribución de tarjetas recibidas por minuto

ggplot(RESULTADOSINTER, aes(x = tarjetas_por_minuto)) + 
  geom_histogram(bins = 30) + 
  labs(x = "Tarjetas recibidas por minuto", y = "Frecuencia") + 
  theme_classic()

# Tabla descriptiva 

# MADRID

skim(RESULTADOSMADRID)

#GRAFICOS 

#Gráfico de dispersión: Tarjetas recibidas por minuto vs Minutos totales

ggplot(RESULTADOSMADRID, aes(x = Minutos_totales, y = tarjetas_por_minuto)) + 
  geom_point() + 
  labs(x = "Minutos totales", y = "Tarjetas recibidas por minuto") + 
  theme_classic()

#Gráfico de histograma: Distribución de tarjetas recibidas por minuto

ggplot(RESULTADOSMADRID, aes(x = tarjetas_por_minuto)) + 
  geom_histogram(bins = 30) + 
  labs(x = "Tarjetas recibidas por minuto", y = "Frecuencia") + 
  theme_classic()

# Tabla descriptiva 

# MILAN

skim(RESULTADOSMILAN)

#GRAFICOS 

#Gráfico de dispersión: Tarjetas recibidas por minuto vs Minutos totales

ggplot(RESULTADOSMILAN, aes(x = Minutos_totales, y = tarjetas_por_minuto)) + 
  geom_point() + 
  labs(x = "Minutos totales", y = "Tarjetas recibidas por minuto") + 
  theme_classic()

#Gráfico de histograma: Distribución de tarjetas recibidas por minuto

ggplot(RESULTADOSMILAN, aes(x = tarjetas_por_minuto)) + 
  geom_histogram(bins = 30) + 
  labs(x = "Tarjetas recibidas por minuto", y = "Frecuencia") + 
  theme_classic()

# Tabla descriptiva 

# NAPOLI

skim(RESULTADOSNAPOLI)

#GRAFICOS 

#Gráfico de dispersión: Tarjetas recibidas por minuto vs Minutos totales

ggplot(RESULTADOSNAPOLI, aes(x = Minutos_totales, y = tarjetas_por_minuto)) + 
  geom_point() + 
  labs(x = "Minutos totales", y = "Tarjetas recibidas por minuto") + 
  theme_classic()

#Gráfico de histograma: Distribución de tarjetas recibidas por minuto

ggplot(RESULTADOSNAPOLI, aes(x = tarjetas_por_minuto)) + 
  geom_histogram(bins = 30) + 
  labs(x = "Tarjetas recibidas por minuto", y = "Frecuencia") + 
  theme_classic()


# Combinamos todos los resultados en una sola dataframe

TODOS_RESULTADOS <- bind_rows(
  RESULTADOSBAYERN, RESULTADOSBENFICA, RESULTADOSCHELSEA, 
  RESULTADOSCITY, RESULTADOSINTER, RESULTADOSMADRID, 
  RESULTADOSMILAN, RESULTADOSNAPOLI
) %>% 
  mutate(Equipo = c(rep("Bayern", nrow(RESULTADOSBAYERN)), 
                    rep("Benfica", nrow(RESULTADOSBENFICA)), 
                    rep("Chelsea", nrow(RESULTADOSCHELSEA)), 
                    rep("City", nrow(RESULTADOSCITY)), 
                    rep("Inter", nrow(RESULTADOSINTER)), 
                    rep("Madrid", nrow(RESULTADOSMADRID)), 
                    rep("Milan", nrow(RESULTADOSMILAN)), 
                    rep("Napoli", nrow(RESULTADOSNAPOLI))))
# Tabla descriptiva

skim(TODOS_RESULTADOS)

# Gráfico de barras: Promedio de tarjetas recibidas por minuto por equipo

ggplot(TODOS_RESULTADOS, aes(x = Equipo, y = tarjetas_por_minuto)) + 
  geom_col() + 
  labs(x = "Equipo", y = "Tarjetas recibidas por minuto") + 
  theme_classic()

# Gráfico de dispersión: Tarjetas recibidas por minuto vs Minutos totales por equipo

ggplot(TODOS_RESULTADOS, aes(x = Minutos_totales, y = tarjetas_por_minuto, color = Equipo)) + 
  geom_point() + 
  labs(x = "Minutos totales", y = "Tarjetas recibidas por minuto", color = "Equipo") + 
  theme_classic()

# Gráfico de histograma: Distribución de tarjetas recibidas por minuto por equipo

ggplot(TODOS_RESULTADOS, aes(x = tarjetas_por_minuto, fill = Equipo)) + 
  geom_histogram(bins = 30, position = "dodge") + 
  labs(x = "Tarjetas recibidas por minuto", y = "Frecuencia", fill = "Equipo") + 
  theme_classic()