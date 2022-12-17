# SESIÓN 8: POSTWORK. "Análisis de la Inseguridad Alimentaria en México"

# DESARROLLO

# Un centro de salud nutricional está interesado en analizar estadísticamente y 
#probabilísticamente los patrones de gasto en alimentos saludables y no saludables 
#en los hogares mexicanos con base en su nivel socioeconómico, en si el hogar tiene 
#recursos financieros extras al ingreso y en si presenta o no inseguridad alimentaria. 
#Además, está interesado en un modelo que le permita identificar los determinantes 
#socioeconómicos de la inseguridad alimentaria.

# La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) 
#levantada por el Instituto Nacional de Salud Pública en México. 

#La mayoría de las personas afirman que los hogares con menor nivel socioeconómico 
#tienden a gastar más en productos no saludables que las personas con mayores niveles 
#socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar presente 
#cierta inseguridad alimentaria.

# La base de datos contiene las siguientes variables:
# - nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto".
# - area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural".
# - numpeho (Número de personas en el hogar).
# - refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí".
# - edadjef (Edad del jefe/a de familia).
# - sexoje (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer".
# - añosedu (Años de educación del jefe de familia).
# - ln_als (Logarítmo natural del gasto en alimentos saludables).
# - ln_alns (Logarítmo natural del gasto en alimentos no saludables).
# - IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA".

#----------------------------------------------------------------------------------------------------------
# ----- 1. Plantea el problema del caso.
#----------------------------------------------------------------------------------------------------------
"
	General: Determinar los patrones que generan inseguridad alimentaria en México.
	a) Determinar el gasto en alimentos saludables y no saludables con base en su 
  	  nivel socioeconómico, recursos extras y seguridad alimentaria.
	b) Proponer un modelo para identificar los determinantes socieconómicos de la 
	    inseguridad alimentaria.
	c) Probar/desechar que a menor nivel socieconómico mayor es el consumo en 
	    alimentos no saludables.
"
	# Importar librerías
	library(ggplot2)
	library(patchwork)
	library(dplyr)
	library(moments)

	# Lectura de datos
	df <- read.csv("https://raw.githubusercontent.com/dnsmartinez/BEDU_S08_Postwork/e2b511147eabce95504b068b14b26a0845beb450/inseguridad_alimentaria_bedu.csv")

	# Limpieza de datos
	Casos.completos <- sum(complete.cases(df))
	print(paste(Casos.completos, "datos completos de", nrow(df), "datos totales."))
	df <- df[complete.cases(df), ]

	# Transformación de variables 
	df$nse5f <- factor(df$nse5f, 
			   labels = c("Bajo", "Medio bajo", "Medio", "Medio alto", "Alto"))
	df$area <- factor(df$area,
			  labels = c("Urbana", "Rural"))
	df$refin <- factor(df$refin,
			   labels = c("No", "Sí"))
	df$sexoje <- factor(df$sexoje,
			    labels = c("Hombre", "Mujer"))
	df$IA <- factor(df$IA,
			labels = c("No", "Sí"))

	str(df)

	
#----------------------------------------------------------------------------------------------------------
# ----- 2. Realiza un análisis descriptivo de la información (Sesión 3).
#----------------------------------------------------------------------------------------------------------
	
	# Tablas de frecuencias y proporciones para datos categóricos

	ia.freq <- table(df$IA)
	ia.freq <- transform(ia.freq, Freq.rel = prop.table(Freq))

	area.freq <- table(df$area)
	area.freq <- transform(area.freq, Freq.rel = prop.table(Freq))

	sexo.freq <- table(df$sexoje)
	sexo.freq <- transform(sexo.freq, Freq.rel = prop.table(Freq))

	refin.freq <- table(df$refin)
	refin.freq <- transform(refin.freq, Freq.rel = prop.table(Freq))

	nse5f.freq <- table(df$nse5f)
	nse5f.freq <- transform(nse5f.freq, Freq.rel = prop.table(Freq))

	ia.freq
	area.freq
	sexo.freq
	refin.freq
	nse5f.freq
	
	mean(df$ln_als)
	mean(df$ln_alns)
	sd(df$ln_als)
	sd(df$ln_alns)

	summary(df)

	# Histogramas y gráficas con el resumen de los datos
	# Insuficiencia alimentaria (donut chart)
	gplot_ia <- ggplot(data = ia.freq, aes(x = 1, y = Freq.rel, fill = Var1)) +
		       theme_void() +
		       geom_col() +
		       coord_polar(theta = "y") +
		       xlim(c(-0.25, 1.5)) + 
		       geom_text(aes(label = paste(round(Freq.rel, 2), "%")), 
				 position = position_stack(vjust = 0.5), 
				 show.legend = FALSE) +
		       scale_fill_brewer(name = "Inseguridad Alimentaria", palette = "Set1") +
		       theme(legend.position = "top")

	# Número de personas en el hogar (histograma)
	gplot_numpeho <- ggplot(data = df, aes(x = numpeho, y = after_stat(count/sum(count)))) +
		       theme_classic() +
		       geom_histogram(aes(color = "2", fill = "1"), binwidth = 1) +
		       geom_boxplot(aes(y = 0.42, color = "2", fill = "1"), width = 0.12) +
		       labs(x = "# Personas en el hogar", y = "% Población") +
		       scale_fill_brewer(palette = "Paired", guide = "none", aesthetics = c("color", "fill"))

	# Edad del jefe de familia (histograma)
	gplot_edad <- ggplot(data = df, aes(x = edadjef, y = after_stat(count/nrow(df)))) +
		       theme_classic() +
		       geom_histogram(aes(color = "4", fill = "3"), binwidth = 5) +
		       geom_boxplot(aes(y = 0.245, color = "4", fill = "3"), width = 0.07) +
		       labs(x = "Edad del jefe del hogar", y = "% Población") +
		       scale_fill_brewer(palette = "Paired", guide = "none", limits = factor(1:8),
			 		 aesthetics = c("color", "fill"))

	# Años de estudio del jefe de familia (histograma)
	gplot_edu <- ggplot(data = df, aes(x = añosedu, y = after_stat(count/nrow(df)))) +
		       theme_classic() +
		       geom_bar(aes(color = "6", fill = "5")) +
		       labs(x = "Años de educación", y = "% Población") +
		       scale_fill_brewer(palette = "Paired", guide = "none", limits = factor(1:8),
					 aesthetics = c("color", "fill"))

	# Zona geográfica (donut chart)
	gplot_area <- ggplot(data = area.freq, aes(x = 1, y = Freq.rel, fill = Var1)) +
		       theme_void() +
		       geom_col() +
		       coord_polar(theta = "y") +
		       xlim(c(-0.25, 1.5)) + 
		       geom_text(aes(label = paste(round(Freq.rel, 2), "%")), 
				 position = position_stack(vjust = 0.5), 
				 show.legend = FALSE) +
		       scale_fill_brewer(name = "Zona geográfica", palette = "Dark2") +
		       theme(legend.position = "top")

	# Sexo del jefe de familia (donut chart)
	gplot_sexo <- ggplot(data = sexo.freq, aes(x = 1, y = Freq.rel, fill = Var1)) +
		       theme_void() +
		       geom_col() +
		       coord_polar(theta = "y") +
		       xlim(c(-0.25, 1.5)) + 
		       geom_text(aes(label = paste(round(Freq.rel, 2), "%")), 
				 position = position_stack(vjust = 0.5), 
				 show.legend = FALSE) +
		       scale_fill_brewer(name = "Sexo", palette = "Accent") +
		       theme(legend.position = "top")

	# Recursos extras al ingreso (donut chart)
	gplot_refin <- ggplot(data = refin.freq, aes(x = 1, y = Freq.rel, fill = Var1)) +
		       theme_void() +
		       geom_col() +
		       coord_polar(theta = "y") +
		       xlim(c(-0.25, 1.5)) + 
		       geom_text(aes(label = paste(round(Freq.rel, 2), "%")), 
				 position = position_stack(vjust = 0.5), 
				 show.legend = FALSE) +
		       scale_fill_brewer(name = "Recursos extras", palette = "Set2") +
		       theme(legend.position = "top")

	# Nivel socioeconómico (barplot)
	gplot_nse5f <- ggplot(data = df, aes(x = nse5f, y = after_stat(count/nrow(df)))) +
		       theme_classic() +
		       geom_bar(aes(fill = nse5f), color = "gray") +
		       labs(x = "Nivel socioeconómico", y = "% población") +
		       scale_x_discrete(labels = NULL) +
		       scale_fill_brewer(palette = "Set2",
					 aesthetics = c("color", "fill"), name = NULL) +
		       guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
		       theme(legend.position = "right")

	# Gasto en alimentos saludables (histograma)
	(kns = ceiling(1+3.3*log10(length(df$ln_als))))
	skewness(df$ln_als) # Esto indica sesgo a la izquierda
	kurtosis(df$ln_als) # Mayor a 3 es leptocúrtica
	
	gplot_lnals <- ggplot(data = df, aes(x = ln_als, y = after_stat(count/nrow(df)))) +
		       theme_classic() +
		       geom_histogram(aes(color = "8", fill = "7"),  bins = kns) +
		       geom_boxplot(aes(y = 0.525, color = "8", fill = "7"), width = 0.15) +
		       labs(x = "Gasto en alimentos saludables", y = "% Población") +
		       scale_fill_brewer(palette = "Paired", guide = "none", limits = factor(1:8),
		 		 aesthetics = c("color", "fill"))
	
	# Gasto en alimentos no saludables (histograma)
	(kns = ceiling(1+3.3*log10(length(df$ln_alns))))
	skewness(df$ln_alns) # Esto indica que es simétrica (no hay sesgo)
	kurtosis(df$ln_alns) # Menor a 3 es platicúrtica
	
	gplot_lnalns <- ggplot(data = df, aes(x = ln_alns, y = after_stat(count/nrow(df)))) +
		       theme_classic() +
		       geom_histogram(aes(color = "10", fill = "9"), bins = kns) +
		       geom_boxplot(aes(y = 0.315, color = "10", fill = "9"), width = 0.09) +
		       labs(x = "Gasto en alimentos no saludables", y = "% Población") +
		       scale_fill_brewer(palette = "Paired", guide = "none", limits = factor(1:10),
			 		 aesthetics = c("color", "fill"))

	gplot <- (gplot_ia | gplot_area | gplot_sexo | gplot_refin) / 
		 (plot_spacer() | gplot_nse5f | gplot_edu | plot_spacer()) /
		 (gplot_numpeho | gplot_edad | gplot_lnals | gplot_lnalns) +
		 plot_annotation(title = "Inseguridad alimentaria en México",
				 subtitle = "Resumen estadístico de la muestra") +
		 plot_layout(heights = c(2, 1, 2))

	#Resumen de imagenes que se guarda en el disco duro
	ggsave(plot = gplot, file = "s08_postwork_summary.png", width = 12, height = 9)

	# Correlación entre el gasto de alimentos saludables y no saludables con base en el nivel socioeconómico, 
	#recursos extras y la inseguridad alimentaria

	df.select <- select(df, ln_als, ln_alns, edadjef, numpeho, añosedu)
	round(cor(df.select), 2)
	
	cor(df$ln_als, as.numeric(df$nse5f), method="spearman")
	cor(df$ln_als, as.numeric(df$refin), method="spearman")
	cor(df$ln_als, as.numeric(df$IA), method="spearman")

	pairs(~ ln_alns + nse5f + refin + IA, 
	      data = df, gap = 0.4, cex.labels = 1.5)

	# Gráfica de correlación

	lnals_nse5f <- ggplot(data = df, aes(x = nse5f, y = ln_als)) +
		       theme_classic() + 
		       geom_boxplot(aes(fill = nse5f, color = nse5f), alpha = 0.5, outlier.alpha = 0.3) +
		       labs(x = "", y = "Gasto en alimentos saludables") +
		       scale_fill_brewer(palette = "Set1", guide = "none", 
		       			 aesthetics = c("fill", "color"))

	lnals_refin <- ggplot(data = df, aes(x = refin, y = ln_als)) +
		       theme_classic() + 
		       geom_boxplot(aes(fill = refin, color = refin), alpha = 0.5, outlier.alpha = 0.3) +
		       labs(x = "", y = "") +
		       scale_fill_brewer(palette = "Set2", guide = "none",
		       			 aesthetics = c("fill", "color"))

	lnals_ia <- ggplot(data = df, aes(x = IA, y = ln_als)) +
		       theme_classic() + 
		       geom_boxplot(aes(fill = IA, color = IA), alpha = 0.5, outlier.alpha = 0.3) +
		       labs(x = "", y = "") +
		       scale_fill_brewer(palette = "Accent", guide = "none",
		       			 aesthetics = c("fill", "color"))

	lnalns_nse5f <- ggplot(data = df, aes(x = nse5f, y = ln_alns)) +
		       theme_classic() + 
		       geom_boxplot(aes(fill = nse5f, color = nse5f), alpha = 0.5, outlier.alpha = 0.3) +
		       labs(x = "Nivel socioeconómico", y = "Gasto en alimentos no saludables") +
		       scale_fill_brewer(palette = "Set1", guide = "none",
		       			 aesthetics = c("fill", "color"))

	lnalns_refin <- ggplot(data = df, aes(x = refin, y = ln_alns)) +
		       theme_classic() + 
		       geom_boxplot(aes(fill = refin, color = refin), alpha = 0.5, outlier.alpha = 0.3) +
		       labs(x = "Recursos extra al ingreso", y = "") +
		       scale_fill_brewer(palette = "Set2", guide = "none",
		       			 aesthetics = c("fill", "color"))

	lnalns_ia <- ggplot(data = df, aes(x = IA, y = ln_alns)) +
		       theme_classic() + 
		       geom_boxplot(aes(fill = IA, color = IA), alpha = 0.5, outlier.alpha = 0.3) +
		       labs(x = "Inseguridad alimentaria", y = "") +
		       scale_fill_brewer(palette = "Accent", guide = "none",
		       			 aesthetics = c("fill", "color"))

	gplot <- (lnals_nse5f + lnals_refin + lnals_ia) /
	         (lnalns_nse5f + lnalns_refin + lnalns_ia) +
		 plot_annotation(title = "Inseguridad alimentaria en México",
				 subtitle = "Gasto en alimentos saludables y no saludables")

	ggsave(plot = gplot, file = "s08_postwork_corr.png", width = 10, height = 7)
	

#----------------------------------------------------------------------------------------------------------
# ----- 3. Calcula probabilidades que nos permitan entender el problema en México.
#----------------------------------------------------------------------------------------------------------
	
	#OPCION 1
	# Se usara las variables nivel socioeconomico (nse5f) y el gasto en alimentos no saludables (ln_alns)
	
	# Rango de los valores de ln_alns
	media<-mean(df$ln_alns)   # 4.11
	desv_est<-sd(df$ln_alns)  # 1.041
	mayor<-max(df$ln_alns)    # 8.29
	menor<-min(df$ln_alns)    # 0
	
	# para dividir en cuatro partes CUARTILES
	grupos<-5
	
	# Construccion de los Intervalos de los gastos NO saludables	
	intervalos_alns<-cut(df$ln_alns,breaks=seq(menor,mayor, length=grupos), include.lowest=TRUE)
	
	# Se	elabora la tabla de Frecuencias	CRUZADAS entre Nivel socioEconomico y AlimentosNoSaludables
	tabla1<-table(df$nse5f, intervalos_alns)
	tabla1
	
	# Ahora la tabla de Frecuencias relativas CRUZADAS entre Nivel SocioEconomico y AlimentosNoSaludables
	tabla2<-round(prop.table(tabla1),6)
	tabla2
	#	                    intervalos_alns
	#	                A        B            C         D
	#	NIVEL       [0,2.07] (2.07,4.15] (4.15,6.22] (6.22,8.3]
	#
	#	Bajo        0.003304    0.123866    0.046696   0.001331
	#	Medio bajo  0.001923    0.119428    0.070562   0.001726
	#	Medio       0.001282    0.115680    0.083531   0.002613
	#	Medio alto  0.000888    0.106213    0.103057   0.005030
	#	Alto        0.000592    0.075296    0.124901   0.012081
	
	# Se verifica cual rango tiene mayor Probabilidad, y se observa que es el rango entre 2.07 y 4.15	
	# Se usa la distribucion normal para el gasto en alimentos no saludables
	prob_rango1<-pnorm(2.07, mean=media, sd=desv_est) - pnorm(0, mean=media, sd=desv_est)     #.0245
	prob_rango2<-pnorm(4.15, mean=media, sd=desv_est) - pnorm(2.07, mean=media, sd=desv_est)  #.4873
	prob_rango3<-pnorm(6.22, mean=media, sd=desv_est) - pnorm(4.15, mean=media, sd=desv_est)  #.4662
	prob_rango4<-pnorm(8.3, mean=media, sd=desv_est) - pnorm(6.22, mean=media, sd=desv_est)   #.0217
	
	
	# ANALISIS DE PROBABILIDAD:
	# Las tablas de frecuencias relativas se pueden usar como PROBABILIDADES cuando los datos son grandes
	# 	
	# La probabilidad de Nivel Bajo       y gasto entre 2.07 y 4.15  es de .1238
	# La probabilidad de Nivel Medio Bajo y gasto entre 2.07 y 4.15  es de .1194
	# La probabilidad de Nivel Medio      y gasto entre 2.07 y 4.15  es de .1156
	# La probabilidad de Nivel Medio Alto y gasto entre 2.07 y 4.15  es de .1062
	# La probabilidad de Nivel Alto       y gasto entre 2.07 y 4.15  es de .0752
	
	# Como se puede observar la Probabilidad de gasto en Alimentos No Saludables es mas GRANDE en personas
	#	con Nivel Socioeconomico BAJO con un valor de .1238.
	#	Esto es uno de los problemas actuales en Mexico.
	# Muy similar es el resultado del rango de gasto entre 0 y 2.7

#---------------------------------------------------------------------------------------------------------
	#OPCION 2
	
	# calculamos nueva variable que indica si el gasto es mayor en no saludables
	df.gasta.mas <- df %>% mutate(gasta.mas.en.no.saludables = (ln_alns >ln_als))
	
	# la transformamos en factor
	df.gasta.mas$gasta.mas.en.no.saludables <- factor(df.gasta.mas$gasta.mas.en.no.saludables, labels = c("No", "Sí"))
	
	# tabla de frecuencias
	tabla_variables <- table(df.gasta.mas$gasta.mas.en.no.saludables, df.gasta.mas$nse5 )
	
	tabla_variables
	
	# graficamos las frecuencias
	barplot(tabla_variables)
	
	# podemos determinar que el nivel de ingreso no determina si el gasto es mayor en alimentos no saludables
	# ya que el porcentaje es muy parecido en todos los niveles
	

#----------------------------------------------------------------------------------------------------------
# ----- 4. Plantea hipótesis estadísticas y concluye sobre ellas para entender el problema en México
#----------------------------------------------------------------------------------------------------------

	#Para todas las hipotesis se toma un nivel de confianza de 95%

	# Sesión 5.
	"Hipótesis: El promedio del gasto en alimentos saludables cuando existe un ingreso extra es igual
	al promedio de gastos en alimentos saludables cuando no existe un ingreso extra"
	# H0: mean(df$ln_als[df$refin == "Sí"]) == mean(df$ln_als[df$refin = "No"])
	# HA: mean(df$ln_als[df$refin == "Sí"]) != mean(df$ln_als[df$refin = "No"])

	var.test(df[df$refin == "Sí", "ln_als"],
		 df[df$refin == "No", "ln_als"],
		 ratio = 1, alternative = "two.sided")
	#Variables distintas
	#El p-value es menor al nivel de significancia (que es 0.05) por lo tanto 
	#se rechaza la hipotesis nula a favor de la alternativa

	#Se asigna el valor FALSE a var.equal porque se rechazó la hipotesis nula anterior
	t.test(df[df$refin == "Sí", "ln_als"],
	       df[df$refin == "No", "ln_als"],
	       mu = 0, alternative = "two.sided", var.equal = FALSE)
	# Como p-value es menor al nivel de significancia (que es 0.05)  
	# existe evidencia estadística para rechazar Ho, es decir,
	# que el gasto promedio de alimentos saludables cuando se tiene un ingreso extra es distinto
	# que cuando no se tiene un ingreso extra
	

	"Hipótesis: El promeio de gasto en alimentos saludables cuando existe inseguridad alimentaria en el hogar 
	es igual al promedio de gasto en alimentos saludables cuando no existe Inseguridad alimentaria en el hogar"
	# H0: mean(df$ln_als[df$IA == "Sí"]) == mean(df$ln_als[df$IA = "No"])
	# HA: mean(df$ln_als[df$IA == "Sí"]) != mean(df$ln_als[df$IA = "No"])

	var.test(df[df$IA == "Sí", "ln_als"],
		 df[df$IA == "No", "ln_als"],
		 ratio = 1, alternative = "two.sided")
	#Varianzas iguales
	#El p-value es mayor al nivel de significancia (que es 0.05) por lo tanto 
	#no se rechaza la hipotesis nula 

	#Se asigna el valor TRUE a var.equal porque no se rechazó la hipotesis nula anterior
	t.test(df[df$IA == "Sí", "ln_als"],
	       df[df$IA == "No", "ln_als"],
	       mu = 0, alternative = "two.sided", var.equal = TRUE)
	# Como p-value es menor al nivel de significancia (que es 0.05)  
	# existe evidencia estadística para rechazar Ho, es decir,
	# que el gasto promedio de alimentos saludables cuando hay insuficiecia alimentaria es distinto
	# que cuando no se tiene inseguridad alimentaria
	

	
	" Hipótesis: El promedio del gasto en alimentos no saludables cuando existe un ingreso extra es igual
	al promedio de gastos no saludables cuando no existe un ingreso extra"
	# H0: mean(df$ln_alns[df$refin == "Sí"]) == mean(df$ln_alns[df$refin = "No"])
	# HA: mean(df$ln_alns[df$refin == "Sí"]) != mean(df$ln_alns[df$refin = "No"])

	var.test(df[df$refin == "Sí", "ln_alns"],
		 df[df$refin == "No", "ln_alns"],
		 ratio = 1, alternative = "two.sided")
	##Varianzas iguales
	#El p-value es mayor al nivel de significancia (que es 0.05) por lo tanto 
	#no se rechaza la hipotesis nula 

	#Se asigna el valor TRUE a var.equal porque no se rechazó la hipotesis nula anterior
	t.test(df[df$refin == "Sí", "ln_alns"],
	       df[df$refin == "No", "ln_alns"],
	       mu = 0, alternative = "two.sided", var.equal = TRUE)
	# Como p-value es mayor al nivel de significancia (que es 0.05)  
	# existe evidencia estadística para no rechazar Ho, es decir,
	# que el gasto promedio de alimentos no saludables cuando se tiene un ingreso extra es igual
	# que cuando no se tiene un ingreso extra

	
	
	"Hipótesis: El promeio de gasto en alimentos no saludables cuando existe inseguridad alimentaria en el hogar 
	es igual al promedio de gasto en alimentos no saludables cuando no existe Inseguridad alimentaria en el hogar"
	# H0: mean(df$ln_alns[df$IA == "Sí"]) == mean(df$ln_alns[df$IA = "No"])
	# HA: mean(df$ln_alns[df$IA == "Sí"]) != mean(df$ln_alns[df$IA = "No"])

	var.test(df[df$IA == "Sí", "ln_alns"],
		 df[df$IA == "No", "ln_alns"],
		 ratio = 1, alternative = "two.sided")
	#Varianzas distintas
	#El p-value es menor al nivel de significancia (que es 0.05) por lo tanto 
	#se rechaza la hipotesis nula 
	
	#Se asigna el valor FALSE a var.equal porque se rechazó la hipotesis nula anterior
	t.test(df[df$IA == "Sí", "ln_alns"],
	       df[df$IA == "No", "ln_alns"],
	       mu = 0, alternative = "two.sided", var.equal = FALSE)
	# Como p-value es menor al nivel de significancia (que es 0.05)  
	# existe evidencia estadística para rechazar Ho, es decir,
	# que el gasto promedio de alimentos no saludables cuando hay insuficiecia alimentaria es distinto
	# que cuando no se tiene inseguridad alimentaria
	

	
	"Hipótesis: En promedio, los hogares con menor nivel socioeconómico gastan más en alimentos NO saludables"
	# H0: df$ln_alns[df$nse5f["Bajo"]] >= df$ln_alns[df$nse5f["Alto"]]
	# HA: df$ln_alns[df$nse5f["Bajo"]] < df$ln_alns[df$nse5f["Alto"]]

	var.test(df[df$nse5f == "Bajo", "ln_alns"],
		 df[df$nse5f == "Alto", "ln_alns"],
		 ratio = 1, alternative = "two.sided")
	#Varianzas distintas
	#El p-value es menor al nivel de significancia (que es 0.05) por lo tanto 
	#se rechaza la hipotesis nula 

	#Se asigna el valor FALSE a var.equal porque se rechazó la hipotesis nula anterior
	t.test(df[df$nse5f == "Bajo", "ln_alns"],
	       df[df$nse5f == "Alto", "ln_alns"],
	       mu = 0, alternative = "less", var.equal = FALSE)
	# Como p-value es menor al nivel de significancia (que es 0.05)  
	# existe evidencia estadística para rechazar Ho, es decir,
	# que el gasto de alimentos no saludables cuando hay un nivel socioeconomico bajo es menor
	# que el gasto de alimentos no saludables cuando hay un nivel socioeconomico mayor
	

	#Buscar si hay al menos una dierencia entre las medias de cada una de las categorías
	anova <- aov(data = df, ln_alns ~ nse5f)

	summary(anova)
	# Como p-value es menor al nivel de significancia, concluimos que 
	# si es diferente el nivel socioeconomico con respecto al gasto de los alimentos no saludables 


#----------------------------------------------------------------------------------------------------------
# ----- 5. Estima un modelo de regresión, lineal o logístico, para identificar los determinantes 
# ----- de la inseguridad alimentaria en México"
#----------------------------------------------------------------------------------------------------------

	# Sesión 6.
	#Generacion de primer modelo
	model <- glm(data = df,
		     IA ~ nse5f + area + refin + sexoje + numpeho + edadjef + añosedu + ln_als + ln_alns,
		     family = binomial)

	summary(model)

	#La variable edadjef no contribuye a mejorar sustancialmente el modelo, por lo tanto se elimina	

	#Generación de segundo modelo
	model2 <- update(model, ~. -edadjef)
	summary(model2)

	#Comparación del AIC del model y model2
	#         AIC
	#model   22143
	#model2  22142
	# Dado los criterios AIC lo tanto el model2 es el más adecuado
	
	"Probar los modelos con una matriz de confusión"
	#Creamos una muestra del 1% de los datos reales al azar
	data.test <- df[sample(nrow(df), 200),]
	
	#Predicción del modelo
	pred <- predict(model2, newdata = data.test, type = "response")
	
	# Categorizar los resultados
	#Si la probabilidad que predijo es mayor a 0.5 asignamos el valor "Si", de lo contrario asignamos "No"
	#refiriendonos a la inseguridad alimentaria
	data.pred <- ifelse(pred > 0.5, "Si", "No")
	
	#Matriz de confución (falsos negativos, falsos positivos, etc...)
	#En las filas van las predicciones y en las columnas van los valores reales.
	conf.matrix <- table(Predicción = data.pred, Real = data.test$IA)
	
	#Imprimimos la tabla
	conf.matrix
	#             Real
	#Predicción  No  Sí
	#        No  15  10
	#        Si  42 133
	
	#Cuantas predicciones fueron exitosas
	aciertos <- sum(diag(conf.matrix))/sum(conf.matrix)
	errores <- 1-aciertos
	
	aciertos; errores
	#[1] 0.74 (74%)
	#[1] 0.26 (26%)
	#El modelo tiene una probabilidad de acierto del 74%
	

#----------------------------------------------------------------------------------------------------------
# ----- 6. Escribe tu análisis en un archivo README.MD y tu código en un script de R y 
# ----- publica ambos en un repositorio de Github.
#----------------------------------------------------------------------------------------------------------
	
# NOTA: Todo tu planteamiento deberá estár correctamente desarrollado y deberás analizar e interpretar todos 
#tus resultados para poder dar una conclusión final al problema planteado.
	
	#CONCLUSION
	"Tomando como base los procedimientos estadísticos mostrados, se cuenta con evidencia suficiente 
	para afirmar que existe una relación inversa entre el nivel socioeconómico y el gasto en productos 
	no saludables, ya que entre más bajo el nivel socioeconómico mayor es el gasto en dichos productos.
El modelo generado  indica que las variables (Nivel Socioeconómico, Zona Geográfica, 
Número de personas en el hogar, Ingreso Extra, Años de educación, y el Gasto de Alimentos Saludables 
y no Saludables son buenos determinantes para la Inseguridad Alimentaria. 
El modelo tiene una probabilidad de predicción alrededor del 74%
"
	
	