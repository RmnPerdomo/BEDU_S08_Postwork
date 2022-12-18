# BEDU-Santander: Proyecto final

## Problema: "Análisis de la Inseguridad Alimentaria en México"               

Un centro de salud nutricional está interesado en analizar estadísticamente y probabilísticamente los patrones de gasto en alimentos saludables y no saludables en los hogares mexicanos con base en su nivel socioeconómico, en si el hogar tiene recursos financieros extras al ingreso y en si presenta o no inseguridad alimentaria. Además, está interesado en un modelo que le permita identificar los determinantes socioeconómicos de la inseguridad alimentaria.
        
La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) levantada por el Instituto Nacional de Salud Pública en México. 

La mayoría de las personas afirman que los hogares con menor nivel socioeconómico tienden a gastar más en productos no saludables que las personas con mayores niveles socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar presente cierta inseguridad alimentaria.
                     
La base de datos contiene las siguientes variables:
  - nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto". 
  - area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural".
  - numpeho (Número de personas en el hogar).
  - refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí".
  - edadjef (Edad del jefe/a de familia).
  - sexoje (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer".
  - añosedu (Años de educación del jefe de familia).
  - ln_als (Logarítmo natural del gasto en alimentos saludables).
  - ln_alns (Logarítmo natural del gasto en alimentos no saludables).
  - IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA".
        


### Punto 1 Plantea el problema del caso.

General: Determinar los patrones que generan inseguridad alimentaria en México.
a) Determinar el gasto en alimentos saludables y no saludables con base en su nivel socioeconómico, recursos extras y seguridad alimentaria.
b) Proponer un modelo para identificar los determinantes socioeconómicos de la inseguridad alimentaria.
c) Probar/desechar que a menor nivel socioeconómico mayor es el consumo en alimentos no saludables.

Para poder trabajar con los datos es necesario importar las librerías correspondientes. 
```r
library(ggplot2)
library(patchwork)
library(dplyr)
library(moments)
```

Después se realiza la lectura de datos y se asigna a un data frame.

```r
df <- read.csv("https://raw.githubusercontent.com/dnsmartinez/BEDU_S08_Postwork/e2b511147eabce95504b068b14b26a0845beb450/inseguridad_alimentaria_bedu.csv")
```

Se realiza la limpieza de datos. En el data frame Casos.completos se asignan únicamente los registros que no tengan datos faltantes usando la función complete.cases.

```r
Casos.completos <- sum(complete.cases(df))
print(paste(Casos.completos, "datos completos de", nrow(df), "datos totales."))
df <- df[complete.cases(df), ]
```

A pesar de que se reducen a la mitad los elementos de la muestra sigue siendo una muestra aceptable para el análisis.

#### Transformación de variables
Las variables categóricas (que son si o no, hombre o mujer, nivel socioeconómico) se transforman en factores. En este caso se transformaron las variables Nivel socieconómico, Zona geográfica, Recursos financieros distintos al ingreso laboral, Sexo del jefe/a de familia e Inseguridad alimentaria en el hogar.

```r
df$nse5f <- factor(df$nse5f, labels = c("Bajo", "Medio bajo", "Medio", "Medio alto", "Alto"))
df$area <- factor(df$area, labels = c("Urbana", "Rural"))
df$refin <- factor(df$refin, labels = c("No", "Sí"))
df$sexoje <- factor(df$sexoje, labels = c("Hombre", "Mujer"))
df$IA <- factor(df$IA, labels = c("No", "Sí"))
```

Se hace el cambio a factores para facilitar la lectura de datos a R y aplicar las herramientas estadísticas adecuadas.

Al final se muestra como quedaron los datos en el data frame.

```r
str(df)
```

### Punto 2. Realiza un análisis descriptivo de la información (Sesión 3)

Con table agrupamos las categorías y contamos cuántos datos hay de la misma. Se realiza este paso para las variables categóricas. Con transform obtenemos las frecuencias relativas. 

```r
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
```

Imprimimos las probabilidades que acabamos de calcular así como las medias y desviaciones estándar de los logaritmos naturales del gasto en alimentos saludables y no saludables.

```r
ia.freq
area.freq
sexo.freq
refin.freq
nse5f.freq

mean(df$ln_als)
mean(df$ln_alns)
sd(df$ln_als)
sd(df$ln_alns)
```

Al final mostramos los distintos estadísticos descriptivos.

```r
summary(df)
```

#### Histogramas y gráficas con el resumen de los datos

A continuación se muestran los histogramas y gráficas con el resumen de los datos. Para las variables categóricas utilizamos donut_chart y para las variables cualitativas discretas y continuas usamos bar_plots e histograms.

#### Gasto en alimentos saludables y no saludables

En la gráfica del gasto en alimentos saludables, se observa que los datos tienen un sesgo a la izquierda y que la gráfica es leptocurtica. En la gráfica del gasto en alimentos no saludables, se observa que los datos tienden a una distribución normal, simétrica y platicúrtica. Todas las paletas de colores usadas en las gráficas son amigables con las personas que padecen de algún problema de daltonismo usando las paletas de colorBrewer.

```r
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
		scale_fill_brewer(palette = "Paired", guide = "none", limits = factor(1:8), aesthetics = c("color", "fill"))
	
# Gasto en alimentos no saludables (histograma)
(kns = ceiling(1+3.3*log10(length(df$ln_alns))))
skewness(df$ln_alns) # Esto indica que es simétrica (no hay sesgo)
kurtosis(df$ln_alns) # Menor a 3 es platicúrtica
	
gplot_lnalns <- ggplot(data = df, aes(x = ln_alns, y = after_stat(count/nrow(df)))) +
		theme_classic() +
		geom_histogram(aes(color = "10", fill = "9"), bins = kns) +
		geom_boxplot(aes(y = 0.315, color = "10", fill = "9"), width = 0.09) +
		labs(x = "Gasto en alimentos no saludables", y = "% Población") +
		scale_fill_brewer(palette = "Paired", guide = "none", limits = factor(1:10), aesthetics = c("color", "fill"))

gplot <- (gplot_ia | gplot_area | gplot_sexo | gplot_refin) / 
	(plot_spacer() | gplot_nse5f | gplot_edu | plot_spacer()) /
	(gplot_numpeho | gplot_edad | gplot_lnals | gplot_lnalns) +
	plot_annotation(title = "Inseguridad alimentaria en México", subtitle = "Resumen estadístico de la muestra") +
	plot_layout(heights = c(2, 1, 2))
```

Generamos un resumen de las imágenes que se guarda en el disco duro.

```r
#Resumen de imagenes que se guarda en el disco duro
ggsave(plot = gplot, file = "s08_postwork_summary.png", width = 12, height = 9)
```

![gráfica del resumen estadístico](https://github.com/dnsmartinez/BEDU_S08_Postwork/blob/main/figs/s08_postwork_summary.png)

Calculamos la correlación entre el gasto de alimentos saludables y no saludables con base en el nivel socioeconómico, recursos extras y la inseguridad alimentaria.

```r
# Correlación entre el gasto de alimentos saludables y no saludables con base en el nivel socioeconómico, 
#recursos extras y la inseguridad alimentaria

df.select <- select(df, ln_als, ln_alns, edadjef, numpeho, añosedu)
round(cor(df.select), 2)
	
cor(df$ln_als, as.numeric(df$nse5f), method="spearman")
cor(df$ln_als, as.numeric(df$refin), method="spearman")
cor(df$ln_als, as.numeric(df$IA), method="spearman")

pairs(~ ln_alns + nse5f + refin + IA, data = df, gap = 0.4, cex.labels = 1.5)

# Gráfica de correlación

lnals_nse5f <- ggplot(data = df, aes(x = nse5f, y = ln_als)) +
		theme_classic() + 
		geom_boxplot(aes(fill = nse5f, color = nse5f), alpha = 0.5, outlier.alpha = 0.3) +
		labs(x = "", y = "Gasto en alimentos saludables") +
		scale_fill_brewer(palette = "Set1", guide = "none", aesthetics = c("fill", "color"))

lnals_refin <- ggplot(data = df, aes(x = refin, y = ln_als)) +
		theme_classic() + 
		geom_boxplot(aes(fill = refin, color = refin), alpha = 0.5, outlier.alpha = 0.3) +
		labs(x = "", y = "") +
		scale_fill_brewer(palette = "Set2", guide = "none", aesthetics = c("fill", "color"))

lnals_ia <- ggplot(data = df, aes(x = IA, y = ln_als)) +
		theme_classic() + 
		geom_boxplot(aes(fill = IA, color = IA), alpha = 0.5, outlier.alpha = 0.3) +
		labs(x = "", y = "") +
		scale_fill_brewer(palette = "Accent", guide = "none", aesthetics = c("fill", "color"))

lnalns_nse5f <- ggplot(data = df, aes(x = nse5f, y = ln_alns)) +
		theme_classic() + 
		geom_boxplot(aes(fill = nse5f, color = nse5f), alpha = 0.5, outlier.alpha = 0.3) +
		labs(x = "Nivel socioeconómico", y = "Gasto en alimentos no saludables") +
		scale_fill_brewer(palette = "Set1", guide = "none", aesthetics = c("fill", "color"))

lnalns_refin <- ggplot(data = df, aes(x = refin, y = ln_alns)) +
		theme_classic() + 
		geom_boxplot(aes(fill = refin, color = refin), alpha = 0.5, outlier.alpha = 0.3) +
		labs(x = "Recursos extra al ingreso", y = "") +
		scale_fill_brewer(palette = "Set2", guide = "none", aesthetics = c("fill", "color"))

lnalns_ia <- ggplot(data = df, aes(x = IA, y = ln_alns)) +
		       theme_classic() + 
		       geom_boxplot(aes(fill = IA, color = IA), alpha = 0.5, outlier.alpha = 0.3) +
		       labs(x = "Inseguridad alimentaria", y = "") +
		       scale_fill_brewer(palette = "Accent", guide = "none", aesthetics = c("fill", "color"))

gplot <- (lnals_nse5f + lnals_refin + lnals_ia) / (lnalns_nse5f + lnalns_refin + lnalns_ia) +
		plot_annotation(title = "Inseguridad alimentaria en México",
		subtitle = "Gasto en alimentos saludables y no saludables")

ggsave(plot = gplot, file = "s08_postwork_corr.png", width = 10, height = 7)
``` 
![gráfica del gasto en alimentos saludables y no saludables](https://github.com/dnsmartinez/BEDU_S08_Postwork/blob/main/figs/s08_postwork_corr.png)

### Punto 3. Calcula probabilidades que nos permitan entender el problema en México

Se usarán las variables nivel socioeconómico (nse5f) y el gasto en alimentos no saludables (ln_alns) para buscar la probabilidad de la relación del nivel socioeconómico bajo contra el intervalo de gastos en alimentos no saludables.

Obtenemos la media, desviación estándar, valor máximo y valor mínimo de la variable del gasto en alimentos no saludables. 
```r
	media<-mean(df$ln_alns)   # 4.11
	desv_est<-sd(df$ln_alns)  # 1.041
	mayor<-max(df$ln_alns)    # 8.29
	menor<-min(df$ln_alns)    # 0
```
Hacemos grupos de cuatro partes
```r
        grupos<-5    # para dividir en cuatro partes CUARTILES
```
Agrupamos el gasto de alimentos no saludables en los grupos previamente generados.
```r
	 intervalos_alns<-cut(df$ln_alns,breaks=seq(menor,mayor, length=grupos), include.lowest=TRUE)

```
Elaboramos la primera tabla de frecuencia de los datos agrupados en intervalos. 
```r
  tabla1<-table(df$nse5f, intervalos_alns)
```

Luego hacemos una tabla cruzada con los datos del nivel socioeconómico con los gastos ahora en intervalos.
```r
	tabla2<-round(prop.table(tabla1),6)
```

|	NIVEL       |[0,2.07] |(2.07,4.15] |(4.15,6.22] |(6.22,8.3]|
|-------------------|---------|------------|------------|----------|
|	Bajo        |0.003304 |   **0.123866** |   0.046696 |  0.001331|
|	Medio bajo  |0.001923 |   0.119428 |   0.070562 |  0.001726|
|	Medio       |0.001282 |   0.115680 |   0.083531 |  0.002613|
|	Medio alto  |0.000888 |   0.106213 |   0.103057 |  0.005030|
|	Alto        |0.000592 |   0.075296 |   0.124901 |  0.012081|

Se toman los valores de la primera fila que es el nivel bajo.

Busco el rango que me dé la mayor probabilidad de los gastos en alimentos no saludables.
```r
	prob_rango1<-pnorm(2.07, mean=media, sd=desv_est) - pnorm(0, mean=media, sd=desv_est)     #.0245
	prob_rango2<-pnorm(4.15, mean=media, sd=desv_est) - pnorm(2.07, mean=media, sd=desv_est)  #.4873   ***
	prob_rango3<-pnorm(6.22, mean=media, sd=desv_est) - pnorm(4.15, mean=media, sd=desv_est)  #.4662
	prob_rango4<-pnorm(8.3, mean=media, sd=desv_est) - pnorm(6.22, mean=media, sd=desv_est)   #.0217
```
La probabilidad mas alta es la del rango entre 2.07 y 4.15 con un valor de **.4873**

 Como se puede observar la Probabilidad de gasto en Alimentos No Saludables es Mayor en personas
con Nivel Socioeconomico **BAJO** con un valor de **.1238**.

Esto es uno de los problemas actuales en Mexico.
### Punto 4. Plantea hipótesis estadísticas y concluye sobre ellas para entender el problema en México


En este punto vamos a plantear algunas hipótesis.


Para todas las hipótesis se toma un nivel de confianza del 95%, es decir, significancia de 0.05.


La primera es “El promedio del gasto en alimentos saludables cuando existe un ingreso extra es igual al promedio de gastos en alimentos saludables cuando no existe un ingreso extra”.


Aquí lo que nos están dando es la hipótesis nula, por lo tanto, la hipótesis alternativa es que los promedios de gastos en alimentos saludables sean diferentes.


```r
"Hipótesis: El promedio del gasto en alimentos saludables cuando existe un ingreso extra es igual
al promedio de gastos en alimentos saludables cuando no existe un ingreso extra"
# H0: mean(df$ln_als[df$refin == "Sí"]) == mean(df$ln_als[df$refin = "No"])
# HA: mean(df$ln_als[df$refin == "Sí"]) != mean(df$ln_als[df$refin = "No"])
```

Primero tenemos que identificar si las varianzas son iguales o no.

```r
var.test(df[df$refin == "Sí", "ln_als"],
		 df[df$refin == "No", "ln_als"],
		 ratio = 1, alternative = "two.sided")
	#Variables distintas
	#El p-value es menor al nivel de significancia (que es 0.05) por lo tanto 
	#se rechaza la hipotesis nula a favor de la alternativa
```

El p value que obtenemos es menor al nivel de significancia, por lo tanto, para la comparación de variables, se rechaza la hipótesis nula a favor de la alternativa (dado que las variables son distintas).

Ahora calculamos el t de student agregando la opción “two-sided” porque la hipótesis se trata de una igualdad, y var.equal como “False” porque las varianzas son distintas.

```r
#Se asigna el valor FALSE a var.equal porque se rechazó la hipotesis nula anterior
	t.test(df[df$refin == "Sí", "ln_als"],
	       df[df$refin == "No", "ln_als"],
	       mu = 0, alternative = "two.sided", var.equal = FALSE)
```

Como p-value es menor al nivel de significancia, existe evidencia estadística para rechazar Ho, es decir, que el gasto promedio de alimentos saludables cuando se tiene un ingreso extra es distinto que cuando no se tiene un ingreso extra.


### Punto 5. Estima un modelo de regresión, lineal o logístico, para identificar los determinantes de la inseguridad alimentaria en México.

Para este punto, como la variable que se pretende describir es la inseguridad alimentaria (variable categórica) se propone un modelo de regresión logística con la inseguridad alimentaria como variable dependiente y el resto de las variables como variables independientes. Para ello se usa la función `glm` (generalized linear model) que viene en el paquete _stats_.

```r
        model <- glm(data = df,
                     IA ~ nse5f + area + refin + sexoje + numpeho + edadjef + añosedu + ln_als + ln_alns,
                     family = binomial)

        summary(model)
```

Con la función `summary`, observamos que la variable edad no contribuye significativamente al modelo, puesto que el _p-value_ de esta variable es mayor a la significancia así que se procede a descartarla.

```r
        model2 <- update(model, ~. - edadjef)
        
        summary(model2)
```

Como se puede observar, la variable _edad_ es la única descartable ya que el resto sí aporta información al modelo.

Posteriormente, para probar la eficacia el modelo extraemos 200 datos al azar de nuestra muestra y predecimos los resultados arrojados por  nuestro modelo con la función `predict`,

```r
        data.test <- df[sample(nrow(df), 200), ]

        pred <- predict(model2, newdata = data.test, type = "response")
```

Convertimos los resultados predichos a factores con base en si la probabilidad es mayor o menor a 0.5 y se crea una tabla comparando los resultados predichos con los datos reales,

```r
        data.pred <- ifelse(pred > 0.5, "Sí", "No")


        conf.matrix <- table(Predicción = data.pred,
                             Real = data.test$IA)

        conf.matrix
```

Y por último, contamos la proporción de los aciertos (aquellos que se encuentran en la diagonal de nuestra tabla) y los errores predichos por el modelo (falsos positivos, falsos negativos, etc).

```r
        aciertos <- sum(diag(conf.matrix))/sum(conf.matrix)
        errores <- 1 - aciertos

        aciertos; errores
```
Con esto, encontramos que nuestro modelo reproduce correctamente cerca del 74% de los datos.


