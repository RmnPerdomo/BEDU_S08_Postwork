#Leeemos los datos del archivo
archivo <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")
#Leemos los datos de los equipos locales
local <- archivo$FTHG
#leemos los daots de los equipos visitantes
visitante <- archivo$FTAG
#generamos la tabla de frequencias
tabla <- table(local,visitante)
#convertimos la tabla en un data frame
datos <- as.data.frame(tabla, stringsAsFactors = FALSE)

# ¿Cuántos goles tuvo el partido con mayor empate?
# 49 partidos con marcados (1-1)

# filtramos los empates
empates <- datos[datos$local == datos$visitante,]
# obtenemos el indice del empate con mas frecuencia
indice.empate <- which.max(empates$Freq)
# obtenemos el empate
empates[indice.empate,]

# ¿En cuántos partidos ambos equipos empataron 0 a 0?
# 33 partidos

#filtramos empates a cero
empates.a.cero <- datos[datos$local == 0 & datos$visitante == 0,]
# obtenemos la frecuencia
empates.a.cero$Freq

# ¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar que el equipo visitante (AG) metiera un solo gol?
# Un partido (6-1)

#filtramos con una frecuencia mayor a cero y donde el visitante no anoto
visitante.cero <- datos[datos$Freq > 0 & datos$visitante == 0,]
# buscamos el indice donde el local anoto mas goles
indice.mayor.local <- which.max(visitante.cero$local)
# obtenemos la goleada
visitante.cero[indice.mayor.local, ]
