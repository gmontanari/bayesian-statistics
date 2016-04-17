
#--------------------------Ejercicio 2------------------------------
#doy un directorio
setwd("~/Desktop/Reg_Avanzada")
library(rjags)
library(R2jags)
#leyendo datos (este es el n??mero de observaciones, cada X_i)
utilidad<-c(212, 207, 210, 196, 223, 193, 196, 210, 202, 221)
n<-length(utilidad)

#la utilidad son las equis, sonde cada equis se distribuye normal miu y sigma cuadrada

#definimos los datos
data<-list("n"=n,"x"=utilidad)

#definiendo inits donde va a comenzar la cadena
inits<-function(){list(mu=0,sig=1,x1=0)} #aqui es donde se puede cambiar a 

#seleccionar parametros al monitor
parameters<-c('mu','sig','x1')

model<-function(){
  #likelihood
  for (i in 1:n) {
    x[i] ~ dnorm(mu,tau)
  }
  tau<- 1/(sig*sig)
  #priors
  mu ~ dnorm(200,0.025)
  sig ~ dgamma(10,1)

  x1 ~ dnorm(mu,tau)
  }

ej2<-jags(data,inits,parameters,model.file=model,n.iter=10000,n.chains=1,n.burnin=100) #se descarta el .10 en el calentamiento



traceplot(ej2) #traza de la cadena

out<-ej2$BUGSoutput$sims.list #jags
traceplot(ej2.sim) mu

out.mat<-ej2$BUGSoutput$sims.matrix
out.sum<-ej2$BUGSoutput$summary #estimaciones en esta salen los resultadaos
out.dic<-ej2$BUGSoutput$DIC #dic 76.71 para el b, 77.09649 para el a

#despues de los dos ejercicios vemos que las estimaciobnes no sesgan mucho, 
#en este caso si me sirve por q me da informaci??n

z<-out$x1
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z)
acf(z)



names(ej1$BUGSoutput) #lo que tiene