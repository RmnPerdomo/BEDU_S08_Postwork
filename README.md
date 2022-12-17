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
        

![gráfica del resumen estadístico](https://github.com/dnsmartinez/BEDU_S08_Postwork/blob/main/figs/s08_postwork_summary.png)
![gráfica del gasto en alimentos saludables y no saludables](https://github.com/dnsmartinez/BEDU_S08_Postwork/blob/main/figs/s08_postwork_corr.png)


### Punto 1 Plantea el problema del caso.

General: Determinar los patrones que generan inseguridad alimentaria en México.
a) Determinar el gasto en alimentos saludables y no saludables con base en su nivel socioeconómico, recursos extras y seguridad alimentaria.
b) Proponer un modelo para identificar los determinantes socioeconómicos de la inseguridad alimentaria.
c) Probar/desechar que a menor nivel socioeconómico mayor es el consumo en alimentos no saludables.

Para poder trabajar con los datos es necesario importar las librerías correspondientes. Después se realiza la lectura de datos y se asigna a un data frame.

Se realiza la limpieza de datos. En el data frame Casos.completos se asignan únicamente los registros que no tengan datos faltantes usando la función complete.cases.

A pesar de que se reducen a la mitad los elementos de la muestra sigue siendo una muestra aceptable para el análisis.

#### Transformación de variables
Las variables categóricas (que son si o no, hombre o mujer, nivel socioeconómico) se transforman en factores. En este caso se transformaron las variables Nivel socieconómico, Zona geográfica, Recursos financieros distintos al ingreso laboral, Sexo del jefe/a de familia e Inseguridad alimentaria en el hogar.

Se hace el cambio a factores para facilitar la lectura de datos a R y aplicar las herramientas estadísticas adecuadas.

Al final se muestra como quedaron los datos en el data frame.

### Punto 2. Realiza un análisis descriptivo de la información (Sesión 3)

Con table agrupamos las categorías y contamos cuántos datos hay de la misma. Se realiza este paso para las variables categóricas. Con el transform obtenemos las frecuencias relativas. 

Imprimimos las probabilidades que acabamos de calcular así como las medias y desviaciones estándar de los logaritmos naturales del gasto en alimentos saludables y no saludables.

Al final mostramos los distintos estadísticos descriptivos.

Histogramas y gráficas con el resumen de los datos

A continuación se muestran los histogramas y gráficas con el resumen de los datos.

Para las variables categóricas utilizamos donut_chart y para las variables cualitativas discretas y continuas usamos bar_plots e histograms.

Gasto en alimentos saludables

En la gráfica del gasto en alimentos saludables, se observa que los datos tienen un sesgo a la izquierda y que la gráfica es leptocurtica.

Gasto en alimentos no saludables

En la gráfica del gasto en alimentos no saludables, se observa que los datos tienden a una distribución normal, simétrica y platicúrtica. 

Todas las paletas de colores usadas en las gráficas son amigables con las personas que padecen de algún problema de daltonismo usando las paletas de colorBrewer

Generamos un resumen de las imágenes que se guarda en el disco duro.

Calculamos la correlación entre el gasto de alimentos saludables y no saludables con base en el nivel socioeconómico, recursos extras y la inseguridad alimentaria.

Como las variables son categóricas se transforman a datos numéricos 

### Punto 3. Calcula probabilidades que nos permitan entender el problema en México

Se usarán las variables nivel socioeconómico (nse5f) y el gasto en alimentos no saludables (ln_alns) para buscar la probabilidad de la relación del nivel socioeconómico bajo contra el intervalo de gastos en alimentos no saludables.

Obtenemos la media, desviación estándar, valor máximo y valor mínimo de la variable del gasto en alimentos no saludables. 

Hacemos grupos de cuatro partes

Agrupamos el gasto de alimentos no saludables en los grupos previamente generados.

Elaboramos la primera tabla de frecuencia de los datos agrupados en intervalos. 

Luego hacemos una tabla cruzada con los datos del nivel socioeconómico con los gastos ahora en intervalos.

Se toman los valores de la primera fila que es el nivel bajo.

Busco el rango que me dé la mayor probabilidad de los gastos en alimentos no saludables.


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


