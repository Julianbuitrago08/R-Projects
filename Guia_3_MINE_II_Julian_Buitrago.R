#Universidad Externado de Colombia 
#MINE: Plataformas y Sistema de Información Estadística 
#Guía 3: Introducción al análisis estadístico de una base de datos en R 
#Fecha: 16/08/2021

#Integrantes: 	
# Julian Buitrago 
# mail: hector.buitrago@est.uexternado.edu.co, julian.buitrago88@gmail.com

#Guía 3: Introducción al análisis estadísstico de una base de datos en R.

library(AER)
library(DescTools)
library(Hmisc)
library(psych) 
library(tidyverse) 
install.packages("HH")
library(HH)
library(dplyr)

data("CPSSW8")
head(CPSSW8); names(CPSSW8)
summary(CPSSW8, 3)


Abstract(CPSSW8)
describe(CPSSW8)
attach(CPSSW8)
table(education)
#a) Observe el código a continuación y escriba una frase que de a conocer el 
#objetivo de tal linea de comando al ejecutarla en la consola principal de R:  

subset(CPSSW8, subset = region %in% c("Northeast","West"), select = c(earnings, gender, age, region)) 
#Se está realizando un filtrado del data-frame  CPSSW8 donde se seleccionan earnings, 	
#gender, age y region  para las regiones Northeast y West 

#Calcular la mediana
median(earnings)
quantile(earnings,0.75)

CPSSW8[earnings > median(earnings) & earnings < quantile(earnings, 0.75), ] 
#Se está extrayendo todas las columnas del data-frame  CPSSW8 cuyos ingresos sean 	
#superiores a la mediana de los ingresos (16.25 USD)  y con ingresos inferiores al cuantil 0.75 	
#(23.55769 USD) .  

CPSSW8[CPSSW8$region == "West" | CPSSW8$region== "South", c(1, 4, 3)]  
#Se está extrayendo las columnas 1 (earnings), 4 (region) y 3 (age)  cuyas filas sean iguales a 	
#la  región “West” o “South”.

?summary
summary(age)
CPSSW8[CPSSW8$region %in% c("West", "South") & age > summary(age, 3)[3], ] 
#Se está extrayendo las columnas  cuyas filas sean iguales a la  región “West” o “South" 
#y su age sea mayor mediana de la age (41 years). 

sapply(split(earnings, region), mean)
# Se esta extrayendo la media de las "earnings" segmentadas por cada "region"

tapply(earnings, list(gender, region), sd)

# Se esta extrayendo la desviacion estandar de las "earnings" segmentadas 
#por la "region" y el "gender".

# b) A partir de las funciones verbo que hacen parte de la librería dplyr3 
#siendo este paquete una dependencia de tidyverse. Emule las líneas de código anterior 
#para obtener los mismos resultados que ilustran filtrado y segmentación de información 
#de la base de datos CPSSW8. 

# b.1) subset(CPSSW8, subset = region %in% c("Northeast","West"), select = c(earnings, gender, age, region)) 

select(filter(CPSSW8,region %in% c("Northeast","West")),c(earnings, gender, age, region))

# b.2) #CPSSW8[earnings > median(earnings) & earnings < quantile(earnings, 0.75), ] 

filter(CPSSW8,earnings > median(earnings) & earnings < quantile(earnings, 0.75))

# b.3) #CPSSW8[CPSSW8$region == "West" | CPSSW8$region== "South", c(1, 4, 3)]  

select(filter(CPSSW8, region == "West" | region== "South"),c(1,4,3))

# b.4) CPSSW8[CPSSW8$region %in% c("West", "South") & age > summary(age, 3)[3], ] 

filter(CPSSW8,region %in% c("West", "South") & age > summary(age,3)[3],) 

#b.5) sapply(split(earnings, region), mean)  
# %>% concatenar múltiples dplyr operaciones
group_by(CPSSW8,region) %>% summarize(mean(earnings)) 

#b.6) tapply(earnings, list(gender, region), sd) 

group_by(CPSSW8,region,gender) %>% summarize(sd(earnings))

#C) ¿Es posible apelar a la librería data.table para realizar los ejercicios de filtrado 
#dados a conocer en los ´ítem anteriores? De ser así, dé a conocer la sintaxis en R para
#algunos ejemplos ilustrativos. 
install.packages("data.table")
library(data.table)

CPSSW8_DT = as.data.table(CPSSW8) 

# c.1) subset(CPSSW8, subset = region %in% c("Northeast","West"), select = c(earnings, gender, age, region)) 
# es de coincidencia de valores y "devuelve un vector de las posiciones de las (primeras) coincidencias 
#de su primer argumento en su segundo"

# . Columnas especificas

CPSSW8_DT[,region %in% c("Northeast","West"),  list(earnings, gender, age, region)] 


# c.2) #CPSSW8[earnings > median(earnings) & earnings < quantile(earnings, 0.75), ] 
CPSSW8_DT[earnings > median(earnings) & earnings < quantile(earnings, 0.75), , ]


# c.3) #CPSSW8[CPSSW8$region == "West" | CPSSW8$region== "South", c(1, 4, 3)]  
CPSSW8_DT[region == "West" | region == "South", c(1, 4, 3), ]


# c.4) CPSSW8[CPSSW8$region %in% c("West", "South") & age > summary(age, 3)[3], ] 
CPSSW8_DT[ region %in% c("West", "South") & age > summary(age, 3)[3] , , ]


#c.5) sapply(split(earnings, region), mean)  
CPSSW8_DT [, mean(earnings), by = region]

#c.6) tapply(earnings, list(gender, region), sd) 
CPSSW8_DT[,sd(earnings), by = list(gender, region)]


# d) Diseñe una tabla de frecuencia agrupada que permita conocer los diferentes 
#intervalos de clase y las frecuencias asociada de la variable earnings.
#Realice comentarios acerca de los resultados obtenidos en la tabla. 

install.packages("agricolae")
library(agricolae)

sturges.freq(earnings) 

hist(earnings) -> h1 

table.freq(h1) 

(table.freq(h1)) 

#e) Realice una comparación entre el histograma5 de los ingresos por hora de las 
#mujeres y hombres. ¿Existen diferencias significativas en termino de medidas de 
#centralidad y dispersión?, realice comentarios que respalden los resultados estadísticos. 


par(mfrow = c(1,2))

earnings.male <- CPSSW8[gender=="male",1];earnings.male 

earnings.female <- CPSSW8[gender=="female",1];earnings.female

hist(earnings.male, main = "Histograma de ingresos-male", freq = TRUE,
     col = heat.colors(7)); rug(earnings.male, col = "red")
lines(density(earnings.male, bw = 2), lwd = 1, col = "orange")


hist(earnings.female, main = "Histograma de ingresos-female", freq = TRUE,
     col = heat.colors(7)); rug(earnings.female, col = "red")
lines(density(earnings.female, bw = 2), lwd = 1, col = "orange")

# Se calcula las medidas de centralidad y disperción
describe(earnings.male)
describe(earnings.female)

#f) De acuerdo a la disposición de la base de datos en términos de las V.a 
#que involucra. Un análisis de ANOVA quizás sea una alternativa viable con el fin 
#de valorar los ingresos de los individuos involucrados en esta encuesta de acuerdo 
#a la región donde se encuentran. En tal caso, una tabla (1) como la que se describe 
#a continuación es pertinente con el fin de juzgar la hipótesis nula: 
  
#  ⋆ H0: µNortheast = µMidwest = µSouth = µWest.  
#  ⋆ Ha: Al menos unas de las parejas de medias no es igual. 

earnings.Northeast <- CPSSW8[region=="Northeast",1];earnings.Northeast
earnings.Midwest <- CPSSW8[region=="Midwest",1];earnings.Midwest
earnings.South <- CPSSW8[region=="South",1];earnings.South
earnings.West <- CPSSW8[region=="West",1];earnings.West 

library(DescTools)

Desc(earnings.Northeast, plotit = TRUE)
Desc(earnings.Midwest, plotit = TRUE)
Desc(earnings.South, plotit = TRUE)
Desc(earnings.West, plotit = TRUE)

#calcula la media geometrica
Gmean(earnings.Northeast)
Gmean(earnings.Midwest)
Gmean(earnings.South)
Gmean(earnings.West)

#Calculamos la media harmonica
Hmean(earnings.Northeast)
Hmean(earnings.Midwest)
Hmean(earnings.South)
Hmean(earnings.West)

Mar <- function(x, na.omit = FALSE) {
  if (na.omit)
    x <- x[!is.na(x)]
  mar = length(x)/sum((1/x))
  cat("Media armonica: ", mar) # return(mar)
}
Mar(earnings.Northeast)
Mar(earnings.Midwest)
Mar(earnings.South)
Mar(earnings.West)


#Desviación a mediana
Dme <- function(x, na.omit = FALSE) {
  if (na.omit)
    x <- x[!is.na(x)]
  n = length(x)
  dme = sum(abs(x - median(x)))/n
  cat("Desviacion a mediana: ", dme) 
  return(dme)
}
Dme(earnings.Northeast)
Dme(earnings.Midwest)
Dme(earnings.South)
Dme(earnings.West)

# Desviación a la media muestral
Dm <- function(x, na.omit = FALSE) {
if (na.omit)
  x <- x[!is.na(x)]
n = length(x)
dm = sum(abs(x - mean(x)))/n
cat("Desviacion media muestral: ", dm)
return(dm)
}
Dm(earnings.Northeast)
Dm(earnings.Midwest)
Dm(earnings.South)
Dm(earnings.West)

# ANOVA
library()

table(region)
tapply(earnings, region, mean); tapply(earnings, region, sd)
fit <- aov(earnings ~ region); fit
summary(fit)
library(gplots)
plotmeans(earnings ~ region, xlab = "Region",
          ylab = "Ingresos", main = "Mean Plot\nwith 95% CI")

TukeyHSD(fit)
par(las = 2); par(mar = c(5, 8, 4, 2))
plot(TukeyHSD(fit))

# g) Existe diferencia significativa entre los ingresos de las
#personas ubicadas en los diferentes niveles del factor region. 
#Diseñe un Box–plot con notch donde además el tamaño de la muestra
#por niveles del factor region se dé a conocer en la ilustración.  

boxplot(earnings ~ region, varwidth = TRUE, notch = T,
        col = c("deepskyblue", "rosybrown1"),main = "Boxplot Conjunto agrupado por factores y con muescas")

#Otra forma

library(psych); library(RColorBrewer)

tapply(earnings, region, mean)
index <- order(tapply(earnings, region, mean)); index
ordered <- factor(tapply(earnings, region, length))
boxplot(earnings ~ region, notch = T, names = as.character(index),
        xlab = "Region", ylab = "Earnings", col = brewer.pal(12, "Set3"))
title(main = "Boxplot Conjunto agrupado por factores y con muescas")
legend(locator(1), c("Northeast", "South", "Midwest","West" ), lty = c(1,1), col = brewer.pal(12, "Set3"), box.col = 0)


#  g.1)  Con el objetivo de brindar apoyo en este ´ítem, es pertinente diseñar un grafica del
#polígono de frecuencia de la variable earnings de acuerdo con los niveles del factor
#region. En tal caso utilice el siguiente código: 
 
# First option
ingresos <- split(x = earnings, f = region)
f1 <- density(ingresos[["Northeast"]]); f2 <- density(ingresos[["Midwest"]])
f3 <- density(ingresos[["South"]]); f4 <- density(ingresos[["West"]])
opar <- par()
par(mfrow = c(1,1), bg = "gray92", col.main = "brown",
    col.lab = "navy", col.axis = "black")
plot(f1, main = "Ingresos por region",
     las = 1, xlim = c(-2, 70), ylim = c(-0.005, 0.06),
     xlab = "Ingresos", ylab = "Densidad", lwd = 2)
abline(h = seq(0, 0.06, 0.01), col = "white")
abline(v = seq(0, 70, 10), col = "white", lty = 2)
lines(f2, lwd = 2, col = "red"); lines(f3, lwd = 2, col = "blue")
lines(f4, lwd = 2, col = "orange")
legend("topright", lwd = 2, bty = "n",
       col = c("black", "red", "blue", "orange"),
       fill = "white", legend = rownames(table(region)))
# Second option
require(ggplot2)
ggplot(CPSSW8, aes(x = earnings)) +
  geom_density(aes(group = region, fill = region), alpha = 0.3) +
  xlim(-2,70) + xlab("Ingresos por region") + ylab("Densidad")



#  g.2)  De acuerdo a lo ilustrado en las gráficas anteriores dé a conocer 
#su opinión respecto al comportamiento de las variables earnings en función 
#de los niveles del factor region. En el caso de ANOVA6 , es pertinente analizar 
#el código a continuación: 

modelAnova <- aov(earnings ~ region); summary(modelAnova)
TukeyHSD(modelAnova); plot(TukeyHSD(modelAnova), col = "blue")

#h) De a conocer un Diagrama pareto y un Fan–plot de la variable categórica 
#que desde su punto de vista sea de mayor importancia y emita comentarios acerca 
#de los porcentajes asociados.  

Desc(CPSSW8, plotit = TRUE)

par(mfrow = c(1,2))

# fan plot education
library(plotrix)

cantidad <- table(education)
trabajos <- rownames(table(education))
fan.plot(cantidad , labels = education, main = "Fan Plot by education")

# Pareto education
library(qcc)

tabla.labor <- table(education); prop.table(tabla.labor) 
pareto.chart(tabla.labor) 

# i) ¿Como interpretaría usted desde un punto de vista estadístico?, 
#las gráficas que se obtienen a partir de las siguientes líneas de código: 

library(DescTools)
Desc(gender, plotit = TRUE)
Desc(earnings ~ gender, plotit = TRUE)
Desc(gender ~ earnings, plotit = TRUE)


#j) Utilice la gráfica de comparación de factores para emitir conclusiones 
#parciales de acuerdo con las variables categóricas que hacen parte del estudio.  

par(mfrow = c(1,2))

plot(gender ~ education, data = CPSSW8,
     main = "Gráfico de comparación entre categoria", angle = 45, border = "grey",
     col = c("pink2","lightblue"))

plot(gender ~ region, data = CPSSW8,
     main = "Gráfico de comparación entre categoria", angle = 45, border = "grey",
     col = c("pink2","lightblue"))

#option 2

plot(gender ~ age, data = CPSSW8,
     main = "Gráfico de comparación entre categoria", angle = 45, border = "grey",
     col = c("pink2","lightblue"))

#k) ¿Como interpretaría la gráfica de perfil que da a conocer el código a continuación?: 
  #with(CPSSW8, interaction.plot(age, gender, earnings, col = "red")) 
par(mfrow = c(1,1))
with(CPSSW8, interaction.plot(age, gender, earnings, col = "red"))

#l) ¿Como utilizaría las gráficas que diseñan los comandos histogram(), densityplot() 
#y bwplot() que hacen parte del paquete lattice, para argumentar su posición respecto 
#a una conclusión en términos estadísticos?, brinde al menos dos ejemplos.  


par(mfrow = c(1,1))
library(lattice); trellis.par.set(col.whitebg())

histogram(~ gender | region, data = CPSSW8,
          col = c("lightblue", "pink2"))


densityplot(~ earnings | gender,
            col = c("blue", "pink", "brown"), data = CPSSW8)

bwplot(earnings ~ gender | region, col = "red")

#m) ¿Que utilidad le brinda usted la gráfica de interacción y la gráfica de parcela de 
#diseño para ilustrar características de las variables con las cuales dispone en esta base 
#de datos? Ilustre con ejemplos.  

#Se pretende es valorar las características de unavariable continua, cuando se 
#sospecha que ésta es afectada por diversos niveles de dos o más factores.

#Grafica de interacción:  
  
library(car)
head(CPSSW8); names(CPSSW8)

interaction.plot(education, gender, earnings, type = "b",
                 col = c("red", "blue"), pch = c(16, 18),
                 main = "Gráfico de interacción", xlab = "Education years")
interaction.plot(education, gender,  earnings, type = "b",
                 col = c("orange", "green"))

# Grafica de parcela:  
# permite valorar la relación entre las variables
#involucradas y la asociación de la mismas se da a conocer este código que
#permite crear una parcela de interacción

library(HH)
interaction2wt(earnings ~ gender * education)


#n) Diseñe un histograma con un Box–plot en la parte inferior, en la cual se dé a 
#conocer la tendencia de los ingresos por persona encuestadas. En tal histograma 
#superponga un polígono de frecuencia y además la distribución Normal con los parámetros 
#estimados de la muestra de ingresos.  

Desc(earnings, plotit = TRUE)


#ñ) Aunque en muchas ocasiones la base de datos con la cual se dispone cuenta 
#con muchos campos o variables para una oportuna descripción de la información. Suele 
#ser recurrente que el profesional en estadística se vea obligado a la creación de nuevas 
#variables, considerando como insumo las ya establecidas dentro de la muestra.



#  ñ.1)  Ya sea a partir de la notación $ o incluso por la función transform() diseñe 
#dos nuevos campos en el cual se dé a conocer el valor de la variable earnings en pesos
#colombianos y euros. Tales nuevos campos, adjuntarlos en una nuevo objeto llamado newCPSSW8 
#el cual contendrá estas nuevas variables y además la variable definida en el ítem a continuación 

# Consulta TRM 16/08/2021 EN https://www.bvc.com.co/pps/tibco/portalbvc/Home, consulta EUR-USD en 
# https://www.xe.com/es/currencyconverter/convert/?Amount=1&From=USD&To=EUR

newCPSSW8 <- transform(CPSSW8, earnings_cop = earnings*3830.25, earnings_eur = earnings*0.84904195) 
attach(newCPSSW8)
head(newCPSSW8); names(newCPSSW8)

#  ñ.2)     Si de ejecuta el código fivenum(education), este brinda como resultado un conjunto de 
#5 números nombrados estadísticos de no centralidad. A partir de fivenum(education)[i] siendo 
#i = 1, 2, . . . , 5 se puede acceder a cada uno de ellos. Lo que se pretende es la creación de un 
#nuevo factor en el cual los registros de años de educación entre la posición fivenum(education)[i] 
#y fivenum(education)[i+1] se les asigne un nivel lo que corresponde a definir una variable cualitativa,
#para posteriormente utilizar este nuevo factor con el fin de valorar el ingreso por hora de las personas 
#que hacen parte de la encuesta en función de los 4 rango de años de educación registrados. ¿Obtendríamos 
#los mismos resultados si se opta por incluir el factor gender en el análisis antes descrito?  


fivenum(education)

new_education <- factor(education, levels = c("6", "12" ,"13" ,"16" ,"20"),
                 labels = c("Elementary School", "Middle school", "High school", "University degree", "Master or PHD education"), order = TRUE)

newCPSSW8$educationFactor[newCPSSW8$education <= 6] <- "Elementary School"
newCPSSW8$educationFactor[newCPSSW8$education > 6 & newCPSSW8$education <= 12] <- "Middle school & High school"
newCPSSW8$educationFactor[newCPSSW8$education == 13] <- "Preparatory School"
newCPSSW8$educationFactor[newCPSSW8$education > 13 & newCPSSW8$education <= 16] <- "University degree"
newCPSSW8$educationFactor[newCPSSW8$education > 16 & newCPSSW8$education <= 20] <- "Master or PHD education"
newCPSSW8$educationFactor[newCPSSW8$education > 20] <- "More than 20 years of education"

head(newCPSSW8); names(newCPSSW8)
Desc(newCPSSW8$educationFactor, plotit = TRUE)

#  Grafica de interacción del ingreso por hora de las personas que hacen parte 
#de la encuesta en función de los 4 rango de años de educación registrado
interaction.plot(newCPSSW8$educationFactor, newCPSSW8$gender, newCPSSW8$earnings, type = "b",
                 col = c("red", "blue"), pch = c(16, 18),
                 main = "Gráfico de interacción", xlab = "Education years")

Desc(newCPSSW8$earnings ~ newCPSSW8$educationFactor, plotit = TRUE)

Desc(newCPSSW8$earnings ~ newCPSSW8$educationFactor * newCPSSW8$gender, plotit = TRUE)
library(HH)
interaction2wt(newCPSSW8$earnings ~ newCPSSW8$educationFactor * newCPSSW8$gender)



#####




