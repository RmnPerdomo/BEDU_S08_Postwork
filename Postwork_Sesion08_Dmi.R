
# Postwork sesión 8. Análisis de la Inseguridad Alimentaria en México

#### OBJETIVO
"
- Realizar un análisis estadístico completo de un caso
 - Publicar en un repositorio de Github el análisis y el código empleado 
"

#### REQUISITOS
"
- Haber realizado los works y postworks previos 
- Tener una cuenta en Github o en RStudioCloud
"

#### DESARROLLO

"
Un centro de salud nutricional está interesado en analizar estadísticamente
y probabilísticamente los patrones de gasto en alimentos saludables y no
saludables en los hogares mexicanos con base en su nivel socioeconómico,
en si el hogar tiene recursos financieros extrar al ingreso y en si presenta
o no inseguridad alimentaria. Además, está interesado en un modelo que le
permita identificar los determinantes socioeconómicos de la inseguridad
alimentaria.
"

"
La base de datos es un extracto de la Encuesta Nacional de Salud y
Nutrición (2012) levantada por el Instituto Nacional de Salud Pública en México.
La mayoría de las personas afirman que los hogares con menor nivel
socioeconómico tienden a gastar más en productos no saludables que las personas
con mayores niveles socioeconómicos y que esto, entre otros determinantes,
lleva a que un hogar presente cierta inseguridad alimentaria.
"

"
La base de datos contiene las siguientes variables:
- nse5f (Nivel socieconómico del hogar):
    1 'Bajo', 2 'Medio bajo', 3 'Medio', 4 'Medio alto', 5 'Alto'.
- area (Zona geográfica): 0 'Zona urbana', 1 'Zona rural'.
- numpeho (Número de persona en el hogar)
- refin (Recursos financieros distintos al ingreso laboral): 0 'no', 1 'sí'
- edadjef (Edad del jefe/a de familia)
- sexoje (Sexo del jefe/a de familia): 0 'Hombre', 1 'Mujer'
- añosedu (Años de educación del jefe de familia)
- ln_als (Logarítmo natural del gasto en alimentos saludables)
- ln_alns (Logarítmo natural del gasto en alimentos no saludables)
- IA (Inseguridad alimentaria en el hogar): 0 'No presenta IA', 1 'Presenta IA'
"

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
df.fact <- df

"
1) Plantea el problema del caso
"


" 2) Realiza un análisis descriptivo de la información "

# La estructura del df muestra que todas las variables son 'int' o 'num'
str(df)

# La función muestra datos faltantes en 'edadjef', 'sexojef', 'ln_als', 'ln_alns'
summary(df)

# Cambio de valores y conversión a factor para la columna nse5f
df.fact$nse5f[df.fact$nse5f == 1] <- "Bajo"
df.fact$nse5f[df.fact$nse5f == 2] <- "Medio bajo"
df.fact$nse5f[df.fact$nse5f == 3] <- "Medio"
df.fact$nse5f[df.fact$nse5f == 4] <- "Medio alto"
df.fact$nse5f[df.fact$nse5f == 5] <- "Alto"
df.fact$nse5f <- factor(x = df.fact$nse5f,
                        levels = c("Bajo", "Medio bajo", "Medio", "Medio alto",
                                   "Alto"), ordered = TRUE)
# Cambio a factor para la columna area
df.fact$area <- factor(df.fact$area, labels = c("Zona urbana", "Zona rural"))
# Cambio de valores para la columna efin
df.fact$refin <- factor(df.fact$refin, labels = c("no", "si"))
# Cambio de valores para la columna sexoje
df.fact$sexojef <- factor(df.fact$sexojef, labels = c("Hombre", "Mujer"))
# Cambio de valores para la columna IA
df.fact$IA <- factor(df.fact$IA, labels = c("No presenta IA","Presenta IA"))

summary(df.fact)



3) Calcula probabilidades que nos permitan entender el problema en México


4) Plantea hipótesis estadísticas y concluye sobre ellas para entender
el problema en México


5) Estima un modelo de regresión, lineal o logístico, para identificiar los
determinanres de la inseguridad alimentaria en México


6) Escribe tu análisis en un archivo README.MD y tu código en un script de R y
publica ambos en un repositorio de Github.

NOTA: Todo tu planteamiento deberá estár correctamente desarrollado y deberás
analizar e interpretar todos tus resultados para poder dar una conclusión final
al problema planteado.



