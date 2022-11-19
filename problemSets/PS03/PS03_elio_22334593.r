library(readr)
library(tidyr)
library(dplyr)

datos <- read.csv("incumbents_subset.csv")

# creamos las variables respectivas
x  = datos$difflog
y  = datos$voteshare
# aplicamos una regresion lineal
regresion1 <- lm(y ~ x)


# graficamos para ver los resultados
plot(x,y,col= "blue",main = "difflog - voteshare", xlab = "difflog", ylab = "voteshare",pch = 19)
abline(regresion1,col="red")
legend(x = "topright", legend = c("datos", "Regresion lineal"), fill = c("blue", "red"), 
       title = "Leyenda")
grid()

# guardamos los residuos
r1 = residuals( regresion1 ) 


resultados = summary(regresion1)
coeficentes = resultados$coefficients
print(paste("Recta de regresion lineal: y = ",coeficentes[2],"x + ",coeficentes[1]))

coeficentes[2]

# creamos las variables respectivas
x  = datos$difflog
y  = datos$presvote
# aplicamos una regresion lineal
regresion2 <- lm(y ~ x)


# graficamos para ver los resultados
plot(x,y,col= "blue",main = "difflog - presvote", xlab = "difflog", ylab = "presvote",pch = 19)
abline(regresion2,col="red")
legend(x = "topright", legend = c("datos", "Regresion lineal"), fill = c("blue", "red"), 
       title = "Leyenda")
grid()

# guardamos los residuos
r2 = residuals( regresion2 ) 


# guardamos las estadisticas de la regresion
resultados = summary(regresion2)
coeficentes = resultados$coefficients
print(paste("Recta de regresion lineal: y = ",coeficentes[2],"x + ",coeficentes[1]))

# creamos las variables respectivas
x  = datos$presvote
y  = datos$voteshare
# aplicamos una regresion lineal
regresion3 <- lm(y ~ x)

# graficamos para ver los resultados
plot(x,y,col= "blue",main = "presvote - voteshare", xlab = "presvote", ylab = "voteshare",pch = 19)
abline(regresion3,col="red")
legend(x = "topright", legend = c("datos", "Regresion lineal"), fill = c("blue", "red"), 
       title = "Leyenda")
grid()

# guardamos las estadisticas de la regresion
resultados = summary(regresion3)
coeficentes = resultados$coefficients
print(paste("Recta de regresion lineal: y = ",coeficentes[2],"x + ",coeficentes[1]))

# creamos las variables respectivas
x  = r2
y  = r1
# aplicamos una regresion lineal
regresion4 <- lm(y ~ x)

# graficamos para ver los resultados
plot(x,y,col= "blue",main = "r2 - r1", xlab = "r2", ylab = "r1",pch = 19)
abline(regresion4,col="red")
legend(x = "topright", legend = c("datos", "Regresion lineal"), fill = c("blue", "red"), 
       title = "Leyenda")
grid()

# guardamos las estadisticas de la regresion
resultados = summary(regresion4)
coeficentes = resultados$coefficients
print(paste("Recta de regresion lineal: y = ",coeficentes[2],"x + ",coeficentes[1]))

# creamos las variables respectivas
x  = datos$difflog
x2 = datos$presvote
y  = datos$voteshare
# aplicamos una regresion lineal
multiple = lm(formula = y ~ x + x2, data = datos)

# guardamos las estadisticas de la regresion
estadisticas = summary(multiple)
c = estadisticas$coefficients
print(paste("Recta de regresion lineal: y = ",c[2],"x1 + ",c[3],"x2 + ",c[1]))


