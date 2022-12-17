# SESIÓN 6: POSTWORK. "Regresión lineal y clasificación"

# DESARROLLO

# Supongamos que nuestro trabajo consiste en aconsejar a un cliente sobre como mejorar las ventas de un producto particular, y el conjunto de datos con el que disponemos son datos de publicidad que consisten en las ventas de aquel producto en 200 diferentes mercados, junto con presupuestos de publicidad para el producto en cada uno de aquellos mercados para tres medios de comunicación diferentes: TV, radio, y periódico. No es posible para nuestro cliente incrementar directamente las ventas del producto. Por otro lado, ellos pueden controlar el gasto en publicidad para cada uno de los tres medios de comunicación. Por lo tanto, si determinamos que hay una asociación entre publicidad y ventas, entonces podemos instruir a nuestro cliente para que ajuste los presupuestos de publicidad, y así indirectamente incrementar las ventas. 

# En otras palabras, nuestro objetivo es desarrollar un modelo preciso que pueda ser usado para predecir las ventas sobre la base de los tres presupuestos de medios de comunicación. Ajuste modelos de regresión lineal múltiple a los datos advertisement.csv y elija el modelo más adecuado siguiendo los procedimientos vistos

# Considera:

# - Y: Sales (Ventas de un producto)
# - X1: TV (Presupuesto de publicidad en TV para el producto)
# - X2: Radio (Presupuesto de publicidad en Radio para el producto)
# - X3: Newspaper (Presupuesto de publicidad en Periódico para el producto)

# Importar librerías

	library(dplyr)
	library(ggplot2)

# Lectura de los datos

	adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")

# Explorar la estructura y tipo de datos

	names(adv)
	str(adv)
	head(adv)

# Explorar la correlación de los datos

	adv.select <- select(adv, Sales, TV, Radio, Newspaper)
	round(cor(adv.select), 2)

# Explorar gráficamente la correlación de los datos con gráficos de dispersión

	png(file = "s06_postwork_cor.png", width = 960, height = 480)
	pairs(~ Sales + TV + Radio + Newspaper, data = adv, cex.labels = 2.5)

# Modelo de regresión lineal

	attach(adv)
	model1 <- lm(Sales ~ TV + Radio + Newspaper)
	summary(model1)

# El coeficiente "Newspaper" no es significativo, por lo que se procede a descartarlo

	model2 <- update(model1, ~ . - Newspaper)
	summary(model2)

# Comparar los dos modelos con ANOVA"

	anova(model2, model1)

# El resultado da como consecuencia rechazar el modelo completo (p-value > 0.05)

# El término de error no tiene correlación significativa con las variables explicativas. En caso contrario, tendríamos un problema de endogeneidad.
# Determinar si el término de error sigue una distribución normal (prueba de Shapiro)

	StanRes2 <- rstandard(model2)
	shapiro.test(StanRes2)

# Gráfica de residuos

	png(file = "s06_postwork_res.png", width = 960, height = 480)
	par(mfrow = c(2, 2))
	plot(model2$fitted.values, Sales, xlab = "Regresión", ylab = "Valores ajustados")
	plot(TV, StanRes2, ylab = "Residuales Estandarizados")
	plot(Radio, StanRes2, ylab = "Residuales Estandarizados")

	qqnorm(StanRes2)
	qqline(StanRes2)

# Predicción: Si el presupuesto para publicidad en periódico no afecta en las ganancias, se propone usar ese presupuesto para aumentar la publicidad en los otros dos medios.

	# a.) Newspaper -> 0.5*TV + 0.5*Radio

	new.data <- data.frame("TV" = adv$TV + 0.5*adv$Newspaper,
				"Radio" = adv$Radio + 0.5*adv$Newspaper)

	predicted.sales1 <- predict(model1, newdata = new.data, interval = "confidence", level = 0.95)
	predicted.sales1 <- cbind(new.data, as.data.frame(predicted.sales1))

	# b.) Newspaper -> TV

	new.data <- data.frame("TV" = adv$TV + adv$Newspaper,
				"Radio" = adv$Radio)

	predicted.sales2 <- predict(model1, newdata = new.data, interval = "confidence", level = 0.95)
	predicted.sales2 <- cbind(new.data, as.data.frame(predicted.sales2))

	# c.) Newspaper -> Radio

	new.data <- data.frame("TV" = adv$TV,
				"Radio" = adv$Radio + adv$Newspaper)

	predicted.sales3 <- predict(model1, newdata = new.data, interval = "confidence", level = 0.95)
	predicted.sales3 <- cbind(new.data, as.data.frame(predicted.sales3))

# Gráfico de comparación entre las tres predicciones

	gplot <- ggplot(data = adv, aes(x = as.integer(rownames(adv)))) + 
		 theme_classic() +
		 geom_ribbon(data = predicted.sales1,
			    aes(ymin = lwr - adv$Sales, ymax = upr - adv$Sales, 
			    fill = "1"), alpha = 0.5, linewidth = 0.1) +
		 geom_ribbon(data = predicted.sales2, 
			    aes(ymin = lwr - adv$Sales, ymax = upr - adv$Sales, 
			    fill = "2"), alpha = 0.5, linewidth = 0.1) +
		 geom_ribbon(data = predicted.sales3, 
			    aes(ymin = lwr - adv$Sales, ymax = upr - adv$Sales, 
			    fill = "3"), alpha = 0.5, linewidth = 0.1) +
		 labs(title = "Ventas por publicidad", subtitle = "Predicciones", 
		      x = "Mercados", y = "Ventas adicionales") +
		 scale_fill_discrete(name = "Modelos", labels = c("TV + Radio", "TV", "Radio")) +
		 theme(legend.position = "bottom")

	ggsave("s06_postwork_pred.png", plot = gplot, width = 5, height = 4)

# Proyección de ganancias totales (comparadas con las reales)

	print(paste("Ventas reales = ", sum(adv$Sales)))
	print(paste("Proyección opción (a):", 
		    "inf = ", sum(predicted.sales1$lwr), "sup = ", sum(predicted.sales1$upr)))
	print(paste("Proyección opción (b):", 
		    "inf = ", sum(predicted.sales2$lwr), "sup = ", sum(predicted.sales2$upr)))
	print(paste("Proyección opción (c):", 
		    "inf = ", sum(predicted.sales3$lwr), "sup = ", sum(predicted.sales3$upr)))

# Conclusión:

# Debido a que se encuentra que estadísticamente, la inversión de publicidad en periódico no se ve reflejada en rendimientos, se sugiere reinvertir esa cantidad en otros medios, destacándose que el medio de publicidad que generaría más rendimientos es en la radio.
