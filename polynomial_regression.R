#Regresion polinomica. 

#Jones and colleagues.
#British Medical Journal.
#21 muestras de niños con caries y perdidas dentales en cientos.
#Concentracion de fluoruro por partes en millon en el agua que beben los niños. 

rm(list = ls())
setwd("...")
PerdDent<-read.csv(file="FlourideDMF.csv",header=TRUE)
View(PerdDent)
attach(PerdDent)


y<-DMFper100
x<-FlouridePPM
x2<-x^2
x3<-x^3
x4<-x^4

plot(x,y,ylab="Perdidas dentales",xlab = "Concentracion flour agua",pch=19,
     main = "Comparacion regresion polinomial")

#Modelo lineal de grado 1.
ft1<-lm(y~x)
ft1
anova(ft1)
abline(ft1,col="red")

#Modelo de segundo grado.
ft2<-lm(y~x+x2)
ft2
anova(ft2)
xv2<-seq(min(x),max(x),0.01)
yv2<-predict(ft2, list(x=xv2,x2=xv2^2))
lines(xv2,yv2,col="blue")

#Modelo de tercer grado (Significant variability)
ft3<-lm(y~x+x2+x3)
ft3
anova(ft3)
xv3<-seq(min(x),max(x),0.01)
yv3<-predict(ft3, list(x=xv3,x2=xv3^2,x3=xv3^3))
lines(xv3,yv3,col="green")

#Modelo de cuardo grado.
ft4<-lm(y~x+x2+x3+x4)
ft4
anova(ft4)
xv4<-seq(min(x),max(x),0.01)
yv4<-predict(ft4, list(x=xv4,x2=xv4^2,x3=xv4^3,x4=xv4^4))
lines(xv4,yv4,col="gold")

legend("topright", "top", legend=c("Informacion", "Lineal", "Cuadratica", "Cubica", "Cuarta"),col=c("black", "red","blue","green","gold"), lty=1:2, cex=0.8 )


#De acuerdo a los valores de anova para cada modelo, el modelo de segundo grado es
#el que mejor se ajusta a las variables sin que la complejidad del modelo se incremente.



