install.packages("car")
library("car")

data(Prestige)
help(Prestige)

# borramos los datos NAN
Prestige <- na.omit(Prestige)

# creamos una lista vacia
professional <- c()
# creamos un ciclo for para añadir elementos a la lista
# 1 profesionales
# 0 caso contrario
# creamos un ciclo para rellenar la lista
for (i in 1:length(Prestige$type)) 
{
    # agregamos los valores respectivos a la variable
    if (Prestige$type[i] == "prof")
    {
    professional <- c(professional,1)
    }
    else
    {
    professional <- c(professional,0)
    }
}

# creamos el modelo de regresion lineal multiple
model <- lm(Prestige$prestige ~ Prestige$income + professional)

# imprimimos la ecuacion
print(paste("Equation: prestige = ",coef(model)[2],"*income + ",coef(model)[3],"*professional + ",coef(model)[1]))

# creamos una nueva lista para aumenta 1000 al ingreso de los profesionales
pro_100 <- c()
for (i in 1:length(Prestige$type)) 
{
    # en caso de ser profesional aumentamos 1000
    if (Prestige$type[i] == "prof")
    {
        pro_100 <- c(pro_100,Prestige$income[i] + 1000)
    }
    else
    {
        pro_100 <- c(pro_100,Prestige$income[i])
    }    

}

# volvemos a realizar el modelo con la nueva lista
model2 <- lm(Prestige$prestige ~ pro_100 + professional)
# resultados
print(paste("Equation: prestige = ",coef(model2)[2],"*income + ",coef(model2)[3],"*professional + ",coef(model2)[1]))
print(paste("the value of the professional coefficient decreases in:",coef(model)[3]-coef(model2)[3]))

# volvemos a calcular la variable profesional con los datos dados
professional2 <- c()

for (i in 1:length(Prestige$type)) 
{
    # agregamos los valores respectivos a la variable
    if (Prestige$type[i] == "prof" ||  Prestige$income[i]>= 6000)
    {
    professional2 <- c(professional2,1)
    }
    else
    {
    professional2 <- c(professional2,0)
    }
}

# creamos la ecuacion
model3 <- lm(Prestige$prestige ~ Prestige$income + professional2)
print(paste("Equation: prestige = ",coef(model3)[2],"*income + ",coef(model3)[3],"*professional + ",coef(model3)[1]))

print(paste("change income :",coef(model3)[2]-coef(model)[2]))
print(paste("change professional :",coef(model3)[3]-coef(model)[3]))
print(paste("change b :",coef(model3)[1]-coef(model)[1]))

# guardamos los valores respectivos de los coeficientes
B1 =  0.042
seB1  = 0.016
# having these yard signs in a precinct affects vote share?

# hypothesis

# H0   B1 = 0   no afecta el procentaje de votos
# H1   B1 > 0   si afecta el porcentaje de votos

# Test statistic
t = (B1-0)/seB1

# p-value
n = 131
df = n-3
p = 2*pt(q = t , df = df, lower.tail = F)

alfa = 0.05

if (p < alfa) 
{
    print(paste("B1 = 0 was not rejected!, which is why have these yard signs in a precinct does not affect the percentage of votes"))
}else
{
    print(paste("B1 = 0 rejected!, which is why have these yard signs in a precinct if it affects the percentage of votes"))
}    

# guardamos los valores respectivos de los coeficientes
B2 =  0.042
seB2  = 0.013
# having these yard signs in a precinct affects vote share?

# hypothesis

# H0   B1 = 0   no afecta el procentaje de votos
# H1   B1 > 0   si afecta el porcentaje de votos

# Test statistic
t2 = (B2-0)/seB2

# p-value
n = 131
df = n-3
p2 = 2*pt(q = t2 , df = df, lower.tail = F)

alfa = 0.05

if (p2 < alfa) 
{
    print(paste("B2 = 0 was not rejected!, does not affect the percentage of votes"))
}else
{
    print(paste("B2 = 0 rejected!, if it affects the percentage of votes"))
}    


