
#--------------------------Ejercicio 1------------------------------
#doy un directorio
setwd("E:/Users/B146322/Desktop/R_work/regresion_av")
library(rjags)
library(R2jags)
#leyendo datos (este es el n??mero de observaciones, cada X_i)
n<-100
#n<-20 #para el inciso d hacemos el mismo ejemplo pero con menos muestra
#credito<-c(rep(1,50),rep(0,50)) #repite los numeros 
credito<-c(rep(1,n/2),rep(0,n/2))

#definimos los datos
data<-list("n"=n,"x"=credito)

#------definiendo inits donde va a comenzar la cadena (son las funciones iniciales)
#--el chiste es elegir bien el par??metro inicial, en este caso fue beta por que e una tasa
#inits<-function(){list(theta=0.5)} 
#inits<-function(){list(lambda=0)} 
#inits<-function(){list(theta=0.5)} 
#inits<-function(){list(theta=0.5,eta=1)}
inits<-function(){list(theta=0.5,x1=rep(1,2))} #para el ejemplo de la predictiva


#------seleccionar parametros al monitor
#parameters<-c('theta')
#parameters<-c('lambda')
#parameters<-c('theta')
#parameters<-c('theta','eta')
parameters<-c('theta','x1')

ej1<-jags(data,inits,parameters,model.file='E1.txt',n.iter=20000,n.chains=1,n.burnin=500, jags.seed = 123) 
#se descarta el .10 en el calentamiento, el n??mero de iteraciones es por cadena, inits es lista o funci??n
#funcion jags


traceplot(ej1) #traza de la cadena, (grafica los par??metros monitoreados y la devianza)

names(ej1$BUGSoutput) #lo que tiene
out<-ej1$BUGSoutput #jags
out.mat<-ej1$BUGSoutput$sims.matrix #matriz de las simulaciones, cada valor estimado de la devianza y los par??metros
out.sum<-ej1$BUGSoutput$summary #estimaciones, resumen de la DIC y parametros
out.dic<-ej1$BUGSoutput$DIC #dic   

#entonces el c es el que mejor se ajusta a los dtos, la dist inicial no esta dando informaci??n adidionl es la mejor

# a) se nota que la theta estimada es muy parecida a la observaci??n del a??o en pasado
# b) se nota que la lambda estimada (normal transformada) es muy parecida a la observaci??n del a??o en pasado
# c) la inicial de referencia es la nueva theta
# d) (verificar cual es la dis final, las gr??ficas de mezcla de betas es este inciso)
# e) (verificar la estimacion)

#combinaci??n de betas
z<-out$theta
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z)
acf(z)

w<-seq(0.01,0.99,,100)
pp<-.3
fw<-pp*dbeta(w,10,10)+(1-pp)*dbeta(w,6,0.01)
par(mfrow=c(1,1))
plot(w,fw,type="l")
