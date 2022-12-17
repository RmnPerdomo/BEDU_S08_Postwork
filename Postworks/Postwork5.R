"Desarrollo
Requisitos
Haber desarrollado los postworks anteriores
Cubrir los temas del prework
Replicar los ejemplos de la sesión

Desarrollo
Instrucciones
El data frame iris contiene información recolectada por Anderson sobre 50 flores de 3 especies distintas 
(setosa, versicolor y virginca), incluyendo medidas en centímetros del largo y ancho del sépalo 
así como de los pétalos.

#Estudios recientes sobre las mismas especies muestran que:
# 1. En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es igual a 5.7 cm
# 2. En promedio, el ancho del pétalo de la especie virginica (Petal.Width) es menor a 2.1 cm
# 3. En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más grande que el promedio 
#    del largo del pétalo de la especie versicolor.
# 4. En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies.

# Utilizando pruebas de inferencia estadística, concluye si existe evidencia suficiente para 
#	concluir que los datos recolectados por Anderson están en línea con los nuevos estudios.

# Utiliza 99% de confianza para toda las pruebas, en cada caso realiza el planteamiento de 
#	hipótesis adecuado y concluye.

#Nivel de confianza 9.9
#Nivel de significancia 0.1

#Primero necesitamos cargar las librerías necesarias para poder hacer los gráficos"
# Cargar librerías
library(dplyr)
library(ggplot2)

#NOTA:
#Estos son los valores que puede tomar cada hipotesis
#Ho: <=   ==    >=
#Ha: >    =!    <

#-------------------------------------------------------------------------------------------------------
#I. En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es igual a 5.7 cm"
#Como nos están preguntando por una igualdad, nos están dando Ho
#Ho: prom_sepal_length_setosa == 5.7
#Ha: prom_sepal_length_setosa =! 5.7

#Se pone alternative = two.sided porque en la hipótesis se pregunta si los largos son IGUALES
t.test(iris[iris$Species == 'setosa', "Sepal.Length"], 
       alternative = 'two.sided', mu=5.1)

#RESULTADO
#	One Sample t-test

#data:  iris[iris$Species == "setosa", "Sepal.Length"]
#t = -1.8857, df = 49, p-value = 0.06527
#alternative hypothesis: true mean is not equal to 5.1
#95 percent confidence interval:
#  4.905824 5.106176
#sample estimates:
#  mean of x 
#5.006 

#COMPARACION
#p-value  > Nivel de significancia
#0.0654   > 0.01

#Toma de desición:
#Si el p-value es >= a la significancia entonces no rechazo la hipotesis nula
#Si el p-value es < a la significancia entonces rechazo la hipotesis nula

#A nivel de significancia 0.01, Existe Evidencia Estadistica para no rechazar la Ho, es decir, 
#En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es igual a 5.7 cm

#-------------------------------------------------------------------------------------------------------
"II. En promedio, el ancho del pétalo de la especie virginica (Petal.Width) es menor a 2.1 cm"
#Como nos están preguntando por algo menor que, nos están dando Ha
#Ho: prom_petal_width >= 2.1
#Ha: prom_petal_width < 2.1

#Se pone alternative = less porque en la hipótesis se pregunta si un largo es MENOR que otro
t.test(iris[iris$Species == 'virginica', "Petal.Width"], 
       alternative = 'less', mu=2.1)

#RESULTADO
#One Sample t-test

#data:  iris[iris$Species == "virginica", "Petal.Width"]
#t = -1.9052, df = 49, p-value = 0.03132
#alternative hypothesis: true mean is less than 2.1
#95 percent confidence interval:
#  -Inf 2.09112
#sample estimates:
#  mean of x 
#2.026 

#COMPARACION
#p-value  > Nivel de significancia
#0.03132  > 0.01

#Toma de desición:
#Si el p-value es >= a la significancia entonces no rechazo la hipotesis nula
#Si el p-value es < a la significancia entonces rechazo la hipotesis nula

#A nivel de significancia 0.01, Existe Evidencia Estadistica para no rechazar la Ho, es decir,
#En promedio, el ancho del pétalo de la especie virginica (Petal.Width) es menor a 2.1 cm

#-------------------------------------------------------------------------------------------------------
"III. En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más grande que el 
promedio del largo del pétalo de la especie versicolor."
#Como nos están preguntando por algo mayor que, nos están dando Ha
#Ho: Virginica.Petal.Length <= Versicolor.Petal.Length
#Ha: Virginica.Petal.Length > Versicolor.Petal.Length

#Antes de hacer la prueba de diferencia de pruebas, hay que hacer la prueba de varianzas
#para verficar si son iguales o diferentes
var.test(iris[iris$Species == "virginica", "Petal.Length"], 
         iris[iris$Species == "versicolor", "Petal.Length"], 
         ratio = 1, alternative = "two.sided")

#RESULTADO (para las varianzas)
#	F test to compare two variances

#data:  iris[iris$Species == "virginica", "Petal.Length"] and iris[iris$Species == "versicolor", "Petal.Length"]
#F = 1.3794, num df = 49, denom df = 49, p-value = 0.2637
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  0.7827605 2.4307127
#sample estimates:
#  ratio of variances 
#1.379372 

#COMPARACION (para las varianzas)
#p-value  > Nivel de significancia
#0.2637   > 0.01

#Toma de desición:
#Si el p-value es >= a la significancia entonces no rechazo la hipotesis nula
#Si el p-value es < a la significancia entonces rechazo la hipotesis nula

#A nivel de confianza 0.01, Existe Evidencia Estadistica para no rechazar la Ho (de las varianzas)

#Como no se rechazó Ho, el valor de var.equal es igual a TRUE
t.test(iris[iris$Species == "virginica", "Petal.Length"],
       iris[iris$Species == "versicolor", "Petal.Length"],
       alternative = "greater", mu = 1.1, var.equal = TRUE)

#RESULTADO
#	Two Sample t-test

#data:  iris[iris$Species == "virginica", "Petal.Length"] and iris[iris$Species == "versicolor", "Petal.Length"]
#t = 1.873, df = 98, p-value = 0.03202
#alternative hypothesis: true difference in means is greater than 1.1
#95 percent confidence interval:
#  1.121779      Inf
#sample estimates:
#  mean of x mean of y 
#      5.552     4.260 

#COMPARACION 
#p-value  > Nivel de significancia
#0.03202  > 0.01

#Toma de desición:
#Si el p-value es >= a la significancia entonces no rechazo la hipotesis nula
#Si el p-value es < a la significancia entonces rechazo la hipotesis nula

#A nivel de significancia 0.01, Existe Evidencia Estadistica para no rechazar la Ho, es decir,
#En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más grande que el 
#promedio del largo del pétalo de la especie versicolor.

#-------------------------------------------------------------------------------------------------------
"IV. En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies."
#Ho: prom_sepal_width.setosa == prom_sepal_width.versicolor == prom_sepal_width.virginica 
#Ha: por lo menos una es diferente

#Primero revisamos el promedio del ancho de las tres especies por medio de un gráfico
boxplot(Sepal.Width ~ Species, data = iris)
# Vemos en la gráfica que las medias son muy diferentes

#Ahora con ANOVA comparamos las tres medias del ancho del sépalo (una media por cada especie)
anova <- aov(Sepal.Width ~ Species,
             data = iris)

summary(anova)
#RESULTADO
#             Df Sum Sq Mean Sq F value Pr(>F)    
#Species       2  11.35   5.672   49.16 <2e-16 ***
#Residuals   147  16.96   0.115                   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#COMPARACION 
#p-value  < Nivel de significancia
#<2e-16   < 0.01

#Toma de desición:
#Si el p-value es >= a la significancia entonces no rechazo la hipotesis nula
#Si el p-value es < a la significancia entonces rechazo la hipotesis nula

#A nivel de significancia 0.01, Existe Evidencia Estadistica para rechazar la Ho, es decir,
#En existe diferencia en el ancho del sépalo entre las 3 especies.

