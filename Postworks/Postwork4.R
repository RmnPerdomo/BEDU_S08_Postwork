
# POSTWORK 04: ANALISIS PROBABILISTICO -----------------------------------------------------------------
# Llamadas internacionales -----------------------------------------------------------------------------

df_telecom <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")

summary(df_telecom)
View(df_telecom)
str(df_telecom)
#3333 datos

hist(df_telecom$total_intl_charge, prob=TRUE, main="Histograma total cargos internacionales",col = "lightblue")


# DISTRIBUCION NORMAL ----------------------------------------------------------------------------------
# Una vez que hayas seleccionado el modelo, realiza lo siguiente:


# 1 .- Grafica la distribución teórica de la variable aleatoria total_intl_charge ----------------------
media <- mean(df_telecom$total_intl_charge)
desv.std <- sd(df_telecom$total_intl_charge)

xmin <- min(df_telecom$total_intl_charge)
xmax <- max(df_telecom$total_intl_charge)

x <- seq(xmin,xmax, 0.01)
y <- dnorm(x, mean = media, sd = desv.std) 


plot(x, y, col= "red", type = "l", xlab = "X", ylab = "f(x)",
     main = "Densidad de Probabilidad Normal" )


# 2.- ¿Cuál es la probabilidad de que el total de cargos internacionales sea menor a 1.85 usd?----------
pnorm(1.85, media, desv.std, lower.tail = TRUE) 

#Por default lower.tail = TRUE, es decir, toma en cuenta el cuantil (q)

#[1] 0.1125002


# 3.- ¿Cuál es la probabilidad de que el total de cargos internacionales sea mayor a 3 usd?-------------
  
pnorm(3, media, desv.std, lower.tail = FALSE) 

#[1] 0.3773985



#4.- ¿Cuál es la probabilidad de que el total de cargos internacionales esté entre 2.35usd y 4.85 usd?-----

pnorm(4.85, mean=media, sd=desv.std) - pnorm(2.35, mean=media, sd=desv.std)

#[1] 0.7060114

# 5.- Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales más alto que podría esperar?---
qnorm(p=0.48, mean = media, desv.std)

#[1] 2.726777

#6- ¿Cuáles son los valores del total de cargos internacionales que dejan exactamente al centro el 80%  -------
#    de probabilidad?

qnorm(p = 0.1, mean = media, sd = desv.std)

qnorm(p = 0.9, mean = media, sd = desv.std)

#[1] 1.798583  Valor inferior
#[1] 3.73058   Valor superior
