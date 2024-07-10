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
