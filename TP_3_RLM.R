#############################################################################
# Analisis de Regresion Lineal Multiple del dataset Estadisticas climaticas #
#                                                                           #
# Creado: 29/09/2021   version: 1.0                                         #
# Ultima Modificacion: 29/09/2021                                           #
# Autores: Franco, Maximiliano, Aguirre Maria Trinidad, Godoy Nicolas       #
#                                                                           #
#############################################################################

##### Importamos las biliotecas #####
library(corrplot)

##### Lectura y verificacion de la informacion leida #####

# Importamos la fuente de datos.
base_datos <- read.table("M:/Universidad/Explotacion de Datos/Clase 3/Unidad III/tp2-r/estadisticas.txt", sep="\t", header=TRUE, fill=TRUE, na.strings = "S/D")
#se lee el archivo. el na.strings se usa para decir que el valor S/D es null. todos son caracteres.
#https://ssl.smn.gob.ar/dpd/observaciones/estadisticas.txt

View(base_datos)

##### Preparacion de los datos cargados #####

#Cambiamos los nombres de las columnas
colnames(base_datos) <- c("Estacion", "Criterio", "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

#Eliminamos los datos nulos
base_datos_limpia <- na.omit(base_datos)

#Agrupamos los valores por los distintos criterios y obtenemos el valor promedio por cada mes

datos <- base_datos_limpia %>%
  group_by(Criterio) %>%
  summarise(Enero=mean(Enero), Febrero=mean(Febrero), Marzo=mean(Marzo), 
            Abril=mean(Abril), Mayo=mean(Mayo), Junio=mean(Junio), Julio=mean(Julio),
            Agosto=mean(Agosto), Septiembre=mean(Septiembre), Octubre=mean(Octubre),
            Noviembre=mean(Noviembre), Diciembre=mean(Diciembre))


# Trasponemos la tabla para tener los criterios como columnas y los meses como filas
data <- data.frame(t(datos[-1]))

# AÃ±adimos los nombres de las columnas
colnames(data) <- c("Precip. sup. a 0.1 mm", "Hum. relativa (%)",
                            "Nubosidad total (octavos)", "Precipitacion (mm)",
                            "Temp. (Centigr.)", "Temp. min. (Centigr.)",
                            "Temp. max. (Centigr.)", "Veloc. Viento (km/h)")

#Vemos con que variables estamos trabajando
names(data)

#PASO 1
#Realizamos un analisis visual de las variables elegidas (salario actual segun salario inicial mas los años de educacion)
#Lo primero es un diagrama de dispercion entre el salario actual y los años de estudio
plot(data$`Temp. (Centigr.)`, data$`Temp. max. (Centigr.)`)
#Se puede ver que si puede existir una correlacion, aunque no es tan facil de verificarse

#Analizamos si efectivamente existe una correlacion lineal
cor.test(data$`Temp. (Centigr.)`, data$`Temp. max. (Centigr.)`)
#Al analizar el coeficiente de correlacion de Pearson, vemos a traves del p value de que existe una
#correlacion lineal y que la fuerza de esta misma es alta ya que el coeficiente de correlacion
#es de 0.66
#Correlacion segun valor: Correlacion directa -> 0 - 0.3 (baja) | 0.3 - 0.6 (media) | 0.6 - 1.0 (alta)
#                         Correlacion inversa -> 0 - -0.3 (baja) | -0.3 - -0.6 (media) | -0.6 - -1.0 (alta)
#
# valor del p.value: >0,5 Hipotesis nula | <0,5 se rechaza Hipotesis nula

#Realizamos un modelo de regresion lineal mediante el comando lm (temperatura segun temperatura maxima mas temperatura)
modelo <- lm(data$`Temp. (Centigr.)`~data$`Temp. max. (Centigr.)` + data$`Temp. min. (Centigr.)`)
#El modelo1 es un objeto conformado por objetos

#Vemos un resumen del modelo creado
summary(modelo)

#PASO 2
#Temperatura como variable predicha, Temperatura maxima y Temperatura minima como variables predictoras
#El R studio nos muestra lo que se conoce como "la tabla de anova" y debemos observar el p.value de nuestro
#F-statistic. Si el p.value es < 0.5 rechazamos la hipotesis nula (el modelo no es valido), pero si el
#p.value > 0.5 entonces no se puede rechazar la hipotesis nula.

#PASO 3
#Debemos analizar nuestro R^2 ajustado ya que es muy riguroso a la hora de interpretar la bondad de ajuste
#del modelo
#En este caso nos indica que porcentaje de la variabilidad de la variable predicha, es explicado por las
#variables predictoras. En este ejemplo podemos ver que el modelo explica el 99.97% de la variabilidad de y

#PASO 4
#En este caso debemos analizar la significancia a traves de los coeficiente. Podemos ver que tenemos 3 
#lineas, por un lado tenemos al intercepto (por donde cruza la linea el eje y) y por el otro tenemos 
#nuestras variables predictoras. Lo que se debe tener en cuenta es que el valor que se encuentra en 
#"estimado" es el valor que ira en nuestra ecuacion, y el otro valor que se debe analizar es el p.value.
#Si nos centramos en nuestras variables predictoras, podemos ver que el p.value es < 0.5 en ambos casos
#y por lo tanto se rechaza la hipotesis nula (la variable no aporta al modelo propuesto) y 
#podemos decir que el salario inicial y los años de estudio si ayudan y si aportan al modelo de 
#regresion

#PASO 5
#Primero vemos nuestros coeficientes
modelo$coefficients
# y = a + b x -> variable dependiente = intercepto + pendiente (x1) + pendiente (x2)
# y = -2.53 + 0.65 * x1 + 0.38 * x2

#Por ultimo realizamos nuestra visualizacion con la recta de regresion lineal
plot(data$`Temp. (Centigr.)`~data$`Temp. max. (Centigr.)`)
abline(modelo, col="red")



#Construimos el modelo
formula = data$`Temp. (Centigr.)`~data$`Temp. max. (Centigr.)` + data$`Temp. min. (Centigr.)`
modelo2 <- lm(formula)
summary(modelo2)

#SUPUESTOS

#Linealidad
plot(modelo2, 1) #Plot de nuestro modelo e indicamos que nos muestre nuestro primer grafico
#Cuando lo observamos, vemos el cruce de los valores predichos o pronosticados y cada uno 
#de los residuos. Lo que se debe analizar, segun la teoria, es que la linea roja se acerque
#a la linea punteada. En este ejemplo vemos que hay un problema, debido a que la linea roja
#comienza lejos de la linea punteada, sin embargo cada vez se va acercando mas a la linea punteada.

#En este caso para verificar la linealidad, debemos ver la correlacion entre la variable dependiente
#con respecto a las variables independientes, para saber si todas cumplen con el supuesto
#de linealidad

cor.test(data$`Temp. (Centigr.)`, data$`Temp. max. (Centigr.)`)
#En este caso como el p.value es < 0.5 podemos decir que existe correlacion lineal

cor.test(data$`Temp. (Centigr.)`, data$`Temp. min. (Centigr.)`)
#En este caso como el p.value es < 0.5 podemos decir que existe correlacion lineal



#Normalidad
#Grafico
plot(modelo2, 2) #Plot de nuestro modelo e indicamos que muestre el segundo grafico (Qqplot)
#En este caso, se debe verificar que todos los puntos, o la mayoria de los puntos, sigan
#la tendencia de la linea diagonal punteada. En este ejemplo, podemos ver que si bien
#hay una gran cantidad de puntos que siguen la tendencia diagonal, tambien hay otra gran 
#cantidad de puntos que se alejan considerablemente de la linea; lo cual podria darnos indicios
#de que no se cumple el supiesto de normalidad de residuos.

#Para una verificacion mas exaustiva, podemos aplicar un test estadistico de normalidad
shapiro.test(modelo2$residuals) #Test shapiro del vector de residuos de nuestro modelo
#En este caso nuestra hipotesis nula es que tiene una distribucion normal. Como nuestro
#p.value < 0.5 podemos deducir que no existe distribucion normal.

#Homocedasticidad
#Nuestra primera forma de verificacion es a traves de un grafico
plot(modelo2, 3) #Plot de nuestro modelo e indicamos que muestre el tercer grafico
#Viendo mas detenidamente podemos identificar que claramente no estamos frente a un
#modelo homocedastico, la linea roja claramente no es una linea horizontal, sino que va
#acrecentandose conforme los valores predichos van aumentando.

#Para corroborar generamos una prueba de hipotesis que es la Breusch Pagan
library(car)
ncvTest(modelo2)
#En este test la hipotesis nula es que el modelo es homocedastico, mientras que si se rechaza
#el modelo sera heterocedastico. En este ejemplo el p.value es < 0.5 y por lo tanto, no existe
#homocedasticidad

#Multicolinealidad
#Para corroborar realizamos el test VIF
library(DescTools)
VIF(modelo)
#Tenemos que ver que los factores de inflacion de varianza sean menores a 5, segun el resultado
#tendremos un buen escenario en el que no tenedremos un factor de inflacion > 5.
#En este ejemplo, podemos ver que los factores de inflacion de varianza nos dan todos
#menores a 5. Es decir no tenemos un escenario de multicolinealidad.

#Identificacion de valores influyentes
#Tenemos 2 maneras de realizar el calculo de la distancia de Cook, y de evaluar que casos 
#podrian o no estar cumpliendo la regla
#Grafico de plot de lineas cuales son los casos que tienen mayor distancia de cook
plot(modelo, 4)
#En este ejemplo, los casos que poseen una mayor distancia de cook son el 29, el 205 y el
#160; sin embargo no solamente es necesario ver las etiquetas, sino tambien ver la escala
#de nuestro grafico. El caso 29, quien es el que posee mayor distncia de cook, tiene un valor de 0.4,
#es decir, que no cumple con la condicion de que sea >= 1 para ser un valor influyente
#Por ende, segun este grafico, podemos deducir que no tenemos valores influyentes dentro
#de nuestro modelo.

#Con este grafico, podremos explorar un poco mas
plot(modelo, 5)
#Para este tipo de grafico, nuevamente debemos poner el ojo en aquellos casos que tengan
#una distancia de cook >= 1, se debe observar en la parte superior derecha, tanto como
#en la parte inferior derecha, y aquellos puntos que pasen la linea del 1 serian los valores
#palanca, por lo tanto podemos concluir que no hay valores influyentes

#Sin embargo, podemos calcular la distancia de Cook, para realizar un analisis mas riguroso
data$cook <- cooks.distance(modelo2)
#Con esto calculamos los valores para cada una de las observaciones
#Entonces, para ser mas rigurosos, indicaremos que nos muestre solo aquellas observaciones
#que tengan una distancia de cook > 1
which(data$cook>1)
#Y podemos corroborar que no existe ninguna distancia de cook mayor a 1, por lo que podemos
#deducir que no existen valores influyentes.
#ACLARACION: la medida de cook es una medida de distancia entre muchas otras.