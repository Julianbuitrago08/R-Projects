#Universidad Externado de Colombia
#Facultad de Economía
#Maestría en Inteligencia de Negocios
#MINE-0002

#Taller 11 – Probabilidad

#Hector Julian Buitrago				
#06 de Septiembre 2021	

#wage salario por hora en dólares
#educ años de educación de la persona
#exper años de experiencia laboral
#tenure años de antigüedad en el empleo actual
#female 1 si es mujer, 0 si es hombre
#married 1 si está casado, 0 si no
#numdep cantidad de personas dependientes económicamente

#install.packages("readxl")
#install.packages('Rcpp')
library(readxl) # libreria para cargar archivos de excel
library(Rcpp)
library(DescTools)
library(psych) 

setwd("C:/Users/jubuv/Downloads/Taller 11 IC") #Establezco el camino donde se
#encuentra el archivo
getwd() #reviso y valido el camino al archivo

datos <- read_xlsx("wagebase.xlsx",sheet = 'datos');datos #cargo losd atos
View(datos) #veo los datos

head(datos)
summary(datos, 3) # Se usa para describir las variables del dataframe (mean, sd, median), 
#mas decimales

describe(datos) # Se usa para describir las variables del dataframe (mean, sd, median)

attach(datos) # uso esta función para seleccionar los datos directamente


#1.  Genere la variable salario que indique el salario en pesos colombianos de una 
#persona que trabaja 8 horas diarias durante 30 días por mes recibiendo la 
#remuneración por hora dada por wage. 
#Utilice una tasa de cambio para el 27 de agosto de 2021.

# TRM 2021-08-27 EN COLOMBIA 1 USD equivale a 3.870,57 COP
#https://dolarhoy.com.co/colombia/viernes-27-agosto-2021/

newdatos <- transform(datos, wage_cop = wage*3870.57)
newdatos <- transform(datos,wage_cop = wage*3870.57, salario = wage_cop*8*30) 
attach(newdatos)
head(newdatos); names(newdatos)

#En cada uno de los ejercicios siguientes:
#  • Identifique la población, la variable y sus unidades, el parámetro.
#  • Construya el intervalo adecuado 
#  • Comente en términos sencillos la afirmación 


#2. El salario medio de las personas es mayor a $4.750.000.
# P(Salario_medio>$4.750.000)=

hist(salario, main = "Histograma de salarios", freq = TRUE,
     col = heat.colors(7)); rug(salario, col = "red")
lines(density(salario, bw = 2), lwd = 1, col = "orange")

# Población = Todos los empleados 
# n = 526
# variable = Salario
# Unidades de la variable = COP, pesos colombianos 
# Parámetro = μ (la media)
# IC = 95 % de confianza
#1- α→1-0.95=0.05 

summary(salario)
describe(salario)

#salariobarra = $ 5.477.107
salariobarra <-  5477107
#desviación = $ 3.430.644
desviación_salario <- 3430644
n_1 <- nrow(datos);n_1

# P(-t_(α/2)<Z< t_(α/2)) = 0.95
α_medios_2 = (1-0.95)/2;α_medios_2

?pt
t_α_medios_2 <- qt(α_medios_2,df=(n_1-1),lower.tail = FALSE);t_α_medios_2
# 1.964493

error_2 <- t_α_medios_2*(desviación_salario / sqrt(n_1));error_2
#$293.855,1 error de estimación

lim_inf_2 <- salariobarra - error_2;lim_inf_2
#$ 5.183.252
lim_sup_2 <- salariobarra + error_2;lim_sup_2
# $5.770.962

# P($5.183.252 < Salario_medio < $5.770.962) = 0.95
# Hay una confiaza del 95% que la media de los salarios de las personas
# que trabajan 8 horas al dia, por 30 dias al mes, este entre $5.183.252 y $5.770.962,
# con lo cual, se cumple que el salario medio de las personas es mayor a $4.750.000


#3. Más de la mitad de los trabajadores están casados.
# P(proporción de casados > 0.5)=

# Población = Todos los empleados 
# n = 526
# variable = Estado civil (married)
# Unidades de la variable = Procentaje de casados
# Parámetro = π
# IC = 95 % de confianza
#1- α→1-0.95=0.05 

sum(with(newdatos,married == "0")) # Cuanto cuantas personas no estan casadas
#206
sum(with(newdatos,married == "1")) # Cuanto cuantas personas estan casadas
#320

n_1 <-  526
casados_pi_gorro <-  320/n_1;casados_pi_gorro
#0.608365

# P(-z_(α/2)<Z< z_(α/2)) = 0.95
α_medios_3 = (1-0.95)/2;α_medios_3

?pt
z_α_medios_3 <- qnorm(α_medios_3,mean=0,sd=1,lower.tail = FALSE);z_α_medios_3
# 1.959964

error_3 <- z_α_medios_3*(sqrt((casados_pi_gorro*(1-casados_pi_gorro))/n_1));error_3
#0.04171364 error de estimación

lim_inf_3 <- casados_pi_gorro - error_3;lim_inf_3
#$ 0.5666514
lim_sup_3 <- casados_pi_gorro + error_3;lim_sup_3
# 0.6500787

# P(0.5666514 < Estar casado < 0.6500787) = 0.95
# Hay una confianza del 95% de que esten casados en el intervalo de
# 56,665% al 65,007% de los trabajadores, con lo cual, se cumple que el 
# Más de la mitad de los trabajadores están casados.


# 4. La varianza de los salarios de las personas con 12 años o menos de educación es 
# igual a la varianza de los salarios de las personas con más de 12 años de educación

Abstract(newdatos)
table(educ)

# P(var_educ_<=12 = var_educ_>12)=

# Población = Personas con 12 años o menos de educación 
#             Personas con más de 12 años de educación
# n = 526
# variable =  Salario 
# Unidades de la variable = COP^2, pesos colombianos al cuadrado
# Parámetro = División de las varianzas Var1/Var"
# IC = 95 % de confianza
#1- α→1-0.95=0.05 

tapply(salario, list(educ >12), sd) # calculo con un logico cual es la desviación del salario
# de las personas que tienen una educación superior a 12 años
tapply(salario, list(educ >12), var) # calculo con un logico cual es la varianza del salario
# de las personas que tienen una educación superior a 12 años

# Selecciono los salarios que corresponden a una educación mayor a 12 años  y calculo su varianza
salario_educ_mayor_12 <- subset(newdatos,educ > 12, select = c('salario'));salario_educ_mayor_12
varianza_salario_educ_mayor_12 <- var(salario_educ_mayor_12);varianza_salario_educ_mayor_12
sqrt(varianza_salario_educ_mayor_12 )

View(salario_educ_mayor_12)
n_educ_mayor_12 = 212 # Cuenta cuantos registros corresponden a una educación mayor a 12 años

# Selecciono los salarios que corresponden una educación menor o igual a 12 años y calculo su varianza
salario_educ_menorigual_12 <- subset(newdatos,educ <= 12, select = c('salario'));salario_educ_menorigual_12 
varianza_salario_educ_menorigual_12 <- var(salario_educ_menorigual_12 );varianza_salario_educ_menorigual_12
sqrt(varianza_salario_educ_menorigual_12 )

View(salario_educ_menorigual_12)
n_educ_menorigual_12 = 314 # Cuenta cuantos registros corresponden a una educación menor o igual a 12 años

α_medios_4 = (1-0.95)/2;α_medios_4

f_α_medios_4_a <- qf(α_medios_4,df1=(n_educ_menorigual_12-1),df2=(n_educ_mayor_12-1),lower.tail = FALSE);f_α_medios_4_a
#1.284655

f_α_medios_4_b <- qf(α_medios_4,df1=(n_educ_mayor_12-1),df2=(n_educ_menorigual_12-1),lower.tail = FALSE);f_α_medios_4_b
#1.276922

lim_inf_4 <- (varianza_salario_educ_menorigual_12/varianza_salario_educ_mayor_12)*(1/f_α_medios_4);lim_inf_4
#0.3318577
lim_sup_4 <- (varianza_salario_educ_menorigual_12/varianza_salario_educ_mayor_12)*f_α_medios_4_b;lim_sup_4
# 0.5443808

# Con una confianza del 95% de que la varianza de los salarios de las personas con 12 años o menos de educación
# no sea igual a la varianza de los salarios de las personas con más de 12 años de educación en un intervalo entre
#0.3318577 y 0.5476777, con lo cual, se considera que no cumple que La varianza de los salarios de las personas 
#con 12 años o menos de educación es igual a la varianza de los salarios de las personas con más de 12 años de educación


# 5. El salario medio de las personas con 12 años o menos de educación es menor que 
#el salario medio de las personas con más de 12 años de educación

# P(xbarra_educ_<=12 < xbarra_educ_>12)=

# Población = Personas con 12 años o menos de educación 
#             Personas con más de 12 años de educación
# variable =  Salario 
# Unidades de la variable = COP, pesos colombianos
# Parámetro = La diferencia de las medias μ_1 -μ_2

# IC = 95 % de confianza
#1- α→1-0.95=0.05 


tapply(salario, list(educ >12), mean)

tapply(salario, list(educ >12), var)

xbarra_menorigual_12 <- 4538507
xbarra_mayor_12 <- 6867297

varianza_salario_educ_menorigual_12/varianza_salario_educ_mayor_12

#Como la división entre las varianzas de los salarios de las personas con 12 años 
#o menos de educación y las varianzas de los salarios de las personas con más de 12 años de educación
#no da igual a uno, sino 0.4263227, entonces no se pueden suponer que son iguales

?mean
View(salario_educ_mayor_12)
n_educ_mayor_12 = 212
View(salario_educ_menorigual_12)
n_educ_menorigual_12 = 314 

#
#
#
#

α_medios_5 = (1-0.95)/2;α_medios_5

grados_libertad_5 = n_educ_mayor_12-1
grados_libertad_5 

tα_medios_5= qt(α_medios_5,df= grados_libertad_5, lower.tail = F);tα_medios_5
#1.971271

error_5= tα_medios_5*sqrt((varianza_salario_educ_menorigual_12/n_educ_menorigual_12)+(varianza_salario_educ_mayor_12/n_educ_mayor_12));error_5
#613535 error de estimación


lim_inf_5 <- xbarra_menorigual_12 - xbarra_mayor_12 - error_5;lim_inf_5
# $ -2942325
lim_sup_5 <- xbarra_menorigual_12 - xbarra_mayor_12 + error_5;lim_sup_5
# $ 1715255

# P(-2942325<(xbarra_educ_<=12 - xbarra_educ_>12)<1715255)= 0.95

# Hay una confianza del 95% que la media de los salarios de las personas con 12 años 
# o menos de educación y la media de los salarios de las personas con más de 12 años de educación
# se encuentren en un intervalo de $ -2942325 y $ 1715255, con lo cual, 
# se cumple que la media de los salarios de las personas con 12 años o menos de 
# educación es menor a la media de los salarios de las personas con más de 12 años


# 6. La proporción casadas dentro de las personas con más de 12 años de educación es 
#mayor que la proporción de casadas dentro de las personas con 12 años o menos 
#de educación.

# P(a > (pi_casado_educ_>12 > pi_casado_educ_<=12) > b)= 

# Población = Personas con más de 12 años de educación
#             Personas con 12 años o menos de educació

# variable =  Married 
# Unidades de la variable = Proporción de casados (%)
# Parámetro = Diferencia de proporciones
# IC = 95 % de confianza
#1- α→1-0.95=0.05 

#realizo el filtro para tener las proporciones en una tabla
m1 <- newdatos[educ >12,6];m1
m2 <- newdatos[educ <=12,6];m2
table(m1)
table(m2)

pi_gorro_casados_educ_mayor_12 <-  137/212;pi_gorro_casados_educ_mayor_12
# 0.6462264
n_educ_mayor_12 = 212 

pi_gorro_casados_educ_menorigual_12 <- 183/314;pi_gorro_casados_educ_menorigual_12
#0.5828025
n_educ_menorigual_12 = 314 

α_medios_6 = (1-0.95)/2;α_medios_6

?pt
z_α_medios_6 <- qnorm(α_medios_6,mean=0,sd=1,lower.tail = FALSE);z_α_medios_6
# 1.959964

ee_6 <- sqrt(((pi_gorro_casados_educ_mayor_12*(1-pi_gorro_casados_educ_mayor_12))/n_educ_mayor_12)+
  ((pi_gorro_casados_educ_menorigual_12*(1-pi_gorro_casados_educ_menorigual_12))/n_educ_menorigual_12))
ee_6
#ee = 0.04304334 

error_6= ee_6*z_α_medios_6;error_6
#0.0843634 error de estimación


lim_inf_6 <- (pi_gorro_casados_educ_mayor_12-pi_gorro_casados_educ_menorigual_12) - error_6;lim_inf_6
# -0.02093953
lim_sup_6 <- (pi_gorro_casados_educ_mayor_12-pi_gorro_casados_educ_menorigual_12) + error_6;lim_sup_6
# 0.1477873

# P(-0.02093953 > (pi_casado_educ_>12 - pi_casado_educ_<=12) > 0.1477873)= 0.95

# Hay una confianza del 95% que la proporción casados dentro de las personas con 
#más de 12 años de educación es mayor que la proporción de casados dentro de las 
#personas con 12 años o menos de educación en un intervalo de -0.02093953 a 0.1477873, 
# con lo cual, cumple debido a que lo incluye, es mayor.
