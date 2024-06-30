library(dplyr)

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