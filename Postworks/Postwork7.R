# Postwork Sesión 7 
# SERIES DE TIEMPO

# Objetivo
# Estimar modelos ARIMA y realizar predicciones

# Desarrollo
# Utilizando el siguiente vector numérico, realiza lo que se indica:

url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/global.txt"

Global <- scan(url, sep="")
#indica que la separación de los datos será por espacios. 
class(Global)
# indica que tipo de clase es el objeto que tenemos. 
str(Global)
# nos da la estructura del objeto. 

# 1. Crea una objeto de serie de tiempo con los datos de Global. La serie debe ser mensual comenzado en Enero de 1856

Global.ts <- ts(Global[], start = c(1856,1), frequency = 12)
Global.ts

# 2. Realiza una gráfica de la serie de tiempo anterior de 2005
png(filename = "s07_postwork_ts.png", width = 512, height = 480)

plot(Global.ts, 
     main = "Temperatura Global", 
     xlab = "Tiempo",
     ylab = "Temperatura Global",
     sub = "Enero de 1856 - Diciembre de 2005")
dev.off()

Global.decom.A <- decompose(Global.ts, type = 'add')

plot(Global.decom.A, xlab = "Tiempo", 
     sub = "Descomposición de los datos de la temperatura global")
#se presenta un descomposición de la serie de tiempo para poder entender mejor
#su comportamiento. 

# 3. Ahora realiza una gráfica de la serie de tiempo anterior, transformando a la primera diferencia:

png(filename = "s07_postwork_diff.png", width = 512, height = 480)

plot(diff(Global.ts), type = "l", main = "Primera diferencia de Global", 
     xlab = "t", ylab = expression(Global))

dev.off()

# 4. ¿Consideras que la serie es estacionaria en niveles o en primera diferencia?

Globaldiff.decom.A <- decompose(diff(Global.ts), type = 'add')

plot(Globaldiff.decom.A, xlab = "Tiempo", 
     sub = "Descomposición de los datos de temperatura global a la primera diferencia")
# Es estacionaria en la primera diferencia ya que su varianza no es explosiva asi como su mediia


# 5. Con base en tu respuesta anterior, obten las funciones de autocorrelación y autocorrelación parcial.


acf(Global.ts)
pacf(Global.ts)

png(filename = "s07_postwork_acf.png", width = 512, height = 480)
acf(diff(Global.ts))
dev.off()

png(filename = "s07_postwork_pacf.png", width = 512, height = 480)
pacf(diff(Global.ts))
dev.off()

