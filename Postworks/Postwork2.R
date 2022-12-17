install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

"
1.Inspecciona el DataSet iris disponible directamente en R. 
Identifica las variables que contiene y su tipo, 
"
iris
str(iris)
head(iris)
names(iris)
dim(iris)

"
asegúrate de que no hayan datos faltantes y que los datos se encuentran listos para usarse.
"

df <- na.omit(iris)

"
2. Crea una gráfica de puntos que contenga Sepal.Lenght en el eje horizontal, 
Sepal.Width en el eje vertical, que identifique Species por color y 
que el tamaño de la figura está representado por Petal.Width. 
Asegúrate de que la geometría contenga shape = 10 y alpha = 0.5.
"
grafica.1 <- ggplot(df, aes(x=Sepal.Length, y = Sepal.Width, color = Species, size = Petal.Width)) + geom_point(shape = 10, alpha = 0.5)
grafica.1

"
3.Crea una tabla llamada iris_mean que contenga el promedio de todas las variables agrupadas por Species.
"
iris.mean <- df %>%
  group_by(Species) %>%
  summarize(Sepal.Length = mean(Sepal.Length),
            Sepal.Width = mean(Sepal.Width),
            Petal.Length = mean(Petal.Length),
            Petal.Width = mean(Petal.Width))
iris.mean

"
Con esta tabla, agrega a tu gráfica anterior otra geometría de puntos para agregar 
los promedios en la visualización. Asegúrate que el primer argumento de la geometría 
sea el nombre de tu tabla y que los parámetros sean shape = 23, size = 4, fill = 'black' 
y stroke = 2. 
"
grafica.2 <- grafica.1 + geom_point(data = iris.mean ,shape = 23, size =4, fill = "black", stroke = 2)
grafica.2

"
También agrega etiquetas, temas y los cambios necesarios para mejorar tu visualización.
"
grafica.3 <- grafica.2 +   labs(title = "Iris Data",
                  x = "Sepal Length",
                  y = "Sepal Width") +
  scale_size("Petal Width") +
  theme_minimal()
grafica.3
