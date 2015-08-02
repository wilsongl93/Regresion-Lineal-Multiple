#REGRESION LINEAL MULTIPLE
#EJERCICIO 2.1
library(readxl)
library(ggplot2)
install.packages("DT",depencies=TRUE)
data1 <- read_excel("poblacion1.xlsx",sheet=1,na="")
data2 <- read_excel("poblacion2.xlsx",sheet=1,na="")
dim(data1)
dim(data2)
#En la data poblacion1 existen 44 observaciones con 4 variables
#En la data poblacion2 existen 40 observaciones con 7 variables


#EJERCICIO 2.2

poblacion <- merge(x = data1 ,y = data2, by = "identificador", suffixes = c("","")) 
poblacion <- poblacion[,-1]
poblacion
clase_variable <-function(A)
{
  clase<-sapply(A,class)
  for(i in 1:ncol(A)) 
  {
    if(is.numeric(A[,i]))
    {
      boxplot(A[,i], xlab = "", ylab=names(A)[i], main=paste("Diag. de cajas de ",names(poblacion)[i]),
              col="green", border="gray1") 
    }
    if(is.character(A[,i]))
    {
      barplot(table(A[,i]), xlab = names(A)[i], ylab="Frecuencia", main="Diagrama de barras",
              col="green", border="gray1")  
    }
  }
  clase
}

clase_variable(poblacion)


#EJERCICIO 2.3
Funcion2.3<-function(A)
{
  for(i in 1:ncol(A))
  {
    if(is.numeric(A[,i]))
    {
      print( paste("Variable:",names(A)[i],"-> Minimo:",min(A[,i]),
                   "-> Maximo:",max(A[,i]),"-> Media",mean(A[,i]),
                   "->Desviacion estandar:", sd(A[,i]),"-> Cuartil:",
                   quantile(A[,i], probs = seq(0, 1, 0.25), na.rm = FALSE)))
    }
    if(is.character(A[,i]))
    {
      print(paste("Frecuencias de ",names(A)[i]))
      print(table(A[,i]))
    }
  }
  
}

Funcion2.3(poblacion)


#EJERCICIO 2.4

correlacion<-numeric()
for(i in 1:ncol(poblacion)){
  if(is.numeric(poblacion[,i]))
  {
  correlacion[i]=cor(poblacion[,1],poblacion[,i])
  print(paste("La correlacion entre ",names(poblacion)[1]," y ",names(poblacion)[i]," es ", correlacion[i]))
  }
}

#EJERCICIO 2.5
p<-subset(data2, subset=data2[,"serv.bas.compl"]=="NO")
q<-subset(data2, subset=data2[,"serv.bas.compl"]=="SI")

t.test(p[,1],q[,1], alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.90)
#La diferencia de medias es distintas para las dos poblaciones
#EJERCICIO 2.6

clase<-sapply(poblacion,class)
(poblacion_reg<-poblacion[,clase=="numeric"])
regresion<-lm(poblacion~.,poblacion_reg)
summary(regresion)
#escojemos los mejores regresores
regre<-step(regresion,direction="backward")
summary(regre)
#Como podemos ver el modelo puede ser representado con un simple
#regresor que determina la mejor aproximacion de la poblacion
#este regresor es tasa.crimen
#Esto quiere decir que simplemente se puede realizar una regresion simple
#entre la variable poblacion y la variable tasa.crimen
#Interpretando estos resultados si la variable tasa.crimen aumenta 1 unidad
#la variable poblacion disminuira en 0.012469
#El intercepto es 8.617002 


#EJERCICIO 2.7
#Como ya vimos, el modelo se redujo a un modelo de regresion simple
#el Rcuadrado es 14.06  es el porcentaje de variabilidad explicada
#por la regresion  con respecto a la variabilidad total

#EJERCICIO 2.8
#El valor de la significancia de la regresion es 0.01714 
#Este valor es menor a 0.5 por tanto aceptamos el modelo
#Ahora para el intercepto, este es aceptado al 100%
#Para el regresor tasa.crimen este es significado al 90% de confiabilidad

#EJERCICIO 2.9
### Gráficos residuales
residuo <- regre[["residuals"]]
prediccion <- regre[["fitted.values"]]

hist(residuo,15)
mean(residuo)
qqnorm(residuo)
qqline(residuo,col="red")
plot(residuo,prediccion)

##como podemos ver la media es practicamente cero y los residuos
#estan dentro de  una franja por lo que se puede decir que este modelo 
#no viola las hipotesis de normalidad
