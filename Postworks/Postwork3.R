# Utilizando el dataframe boxp.csv realiza el siguiente análisis descriptivo. 
#No olvides excluir los missing values y transformar las variables a su tipo y escala correspondiente.

	#Carga de librerías
  library(ggplot2)
	library(dplyr)
	library(DescTools)
	library(moments)
	library(RColorBrewer)

	#Lectura de datos
  df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/boxp.csv")

	#Lectura de información
  str(df)
	names(df)
	dim(df)

	#Se asignan únicamente los registros que no tengan datos faltantes
	sum(complete.cases(df))
	df <- df[complete.cases(df), ]

	#Cambio a factores
	df$Categoria <- factor(df$Categoria)
	df$Grupo <- factor(df$Grupo)

# 1. Calcula e interpreta las medidas de tendencia central de la variable Mediciones

	med.mean <- mean(df$Mediciones)
	med.median <- median(df$Mediciones)
	med.mode <- Mode(df$Mediciones)

	print(paste("Media = ", med.mean))
	print(paste("Mediana = ", med.median))
	print(paste("Moda = ", med.mode))


# 2. Con base en tu resultado anteior, ¿qué se puede concluir respecto al sesgo de Mediciones?

	sort(c(med.mean, med.median, med.mode))
	swk <- skewness(df$Mediciones)

	print(paste("Sesgo = ", swk))
	print("Moda < Mediana < Media (s > 0). La distribución presenta un sesgo hacia la derecha.")


# 3. Calcula e interpreta la desviación estándar y los cuartiles de la distribución de Mediciones

	std <- sd(df$Mediciones) 
	qrt <- quantile(df$Mediciones)

	print(paste("Desviación Estándar", std))
	print("Cuartiles: ")
	print(qrt)


# 4. Con ggplot, realiza un histograma separando la distribución de Mediciones por Categoría ¿Consideras que sólo una categoría está generando el sesgo?

	gplot <- ggplot(df, aes(x = Mediciones, fill = Categoria)) +
		geom_histogram(bins = 20, alpha = 0.5) +
		labs(title = "Data Frame Histogram") +
		theme_classic()

	ggsave("sesion03_postwork_hist.png", width = 5, height = 4, plot = gplot)

# 5. Con ggplot, realiza un boxplot separando la distribución de Mediciones por Categoría y por Grupo dentro de cada categoría. ¿Consideras que hay diferencias entre categorías? ¿Los grupos al interior de cada categoría podrían estar generando el sesgo?

	gplot <- ggplot(df, aes(x = Categoria, y = Mediciones, fill = Grupo)) +
		geom_boxplot() +
		theme_classic()

	ggsave("sesion03_postwork_boxplot.png", width = 5, height = 4, plot = gplot)

