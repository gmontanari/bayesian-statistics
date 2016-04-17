theta0<-.6  #prob inicial
n<-100   #muestra de la bin
x<-rbinom(n,1,theta0)

hist(x,freq=FALSE)

#DISTRIBUCION INICIAL PARA THETA
a<-1
b<-1
theta<-seq(0,1,,100) #yo quiero 100 puntos en el intervalo 0 y 1
plot(theta,dbeta(theta,a,b),type="l") #secuencia versus una beta(1,1)

#DISTRIBUCION FINAL BETA
a1<-a+sum(x)
b1<-b+n-sum(x)
plot(theta,dbeta(theta,a1,b1),type="l")

#ambas
theta<-seq(0,1,,100)
#cuando quieres graficar muchas encimadas hay que agarrar la y m??xima
plot(theta,dbeta(theta,a,b),type="l",ylim=c(0,8))
lines(theta,dbeta(theta,a1,b1),col=2)
abline(v=theta0,col=4)

#aproximacion normal asintotica
mu<-(a1-1)/(a1+b1-2) #es la media despues de la transformacion
sig2<-(a1-1)*(b1-1)/(a1+b1-2)^3


