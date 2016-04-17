#simulaci??n montecarlo

#Sea ??? la tasa de cr??ditos hipotecarios otorgados por un banco. Durante
#el 2004 la tasa promedio fue de 60% y la desviaci??n est??ndar de la tasa fue de 0.04. En lo que va del a??o 2005 se han solicitado 100 cr??ditos, de los cuales se han otorgado ??nicamente 50.
#a) Usando la informaci??n del a??o pasado, encuentra la distribuci??n beta
#que mejor describe el conocimiento inicial.
#b) Usando la informaci??n del a??o pasado, encuentra la distribuci??n
#normal transformada que mejor describa el conocimiento inicial.
#c) Determina la distribuci??n inicial de referencia.
#d) Usando los datos del a??o 2005 encuentra la distribuci??n final para
#cada una de las distribuciones iniciales de los incisos (a) ??? (c).
#e) Estima la tasa de cr??ditos otorgados, usando las 3 distribuciones
#finales del inciso (d).
#f) Estima el momio de otorgar un cr??dito, i.e., ??? ??? ???/(1??????), usando las
#3 distribuciones finales del inciso (d).

#variable de interes theta
#distribucion inicial tiene una media y varianza
#tenemos las variables aleatorias X_i (una muestra de 100)
#donde cada v.a. se distribuye Bernoulli X|theta 
#se estiman los parametros de la beta conociendo miu y varianza
#para bugs debe tener un formato de lista (tiene un nombre interno y una variable asignada)
#la theta inicial (el valor inicial de la cadena es arbitrario, se elige .5)
#un momio es la representaci??n real de la beta
#4 graficas, traza, promedios ergodicos, histograma, correlaciones
##el promedio ergodico es el promedio acumulado de la traza, cuando se deja de mover se estabiliza la media, en este caso, despues de 30 podemos considerar que viene de la distribucion final
#en este caso la grafica de autocorrelaciones no dice nada
#el comando jags.model
#los parametros para el comando bugs, datos,funcion, parametros a monitorear, modelo que yo lo doy en un txt en el directorio
#la grafica se llama traza


#--------------------------Ejercicio 1------------------------------
#doy un directorio
setwd("~/Desktop/Reg_Avanzada")
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
#inits<-function(){list(theta=0.5)} #MODELO theta ~ dbeta(89.4,59.6)
#inits<-function(){list(lambda=0)} #MODELO lambda ~ dnorm(0.4054,33.33)
#inits<-function(){list(theta=0.5,eta=1)}
#inits<-function(){list(theta=0.5)} 
inits<-function(){list(theta=0.5,x1=rep(1,2))} #para el ejemplo de la predictiva

                      
#------seleccionar parametros al monitor
#parameters<-c('theta')
#parameters<-c('lambda')
#parameters<-c('theta','eta')
#parameters<-c('theta')
parameters<-c('theta','x1')

ej1<-jags(data,inits,parameters,model.file='E1.txt',n.iter=20000,n.chains=1,n.burnin=500) 
#se descarta el .10 en el calentamiento, el n??mero de iteraciones es por cadena, inits es lista o funci??n
#funcion jags
jags(data, inits, parameters.to.save, model.file="model.bug",
     n.chains=3, n.iter=2000, n.burnin=floor(n.iter/2),
     n.thin=max(1, floor((n.iter - n.burnin) / 1000)),
     DIC=TRUE, working.directory=NULL, jags.seed = 123,
     refresh = n.iter/50, progress.bar = "text", digits=5,
     RNGname = c("Wichmann-Hill", "Marsaglia-Multicarry",
                 "Super-Duper", "Mersenne-Twister"),
     jags.module = c("glm","dic")
)


traceplot(ej1) #traza de la cadena, (grafica los par??metros monitoreados y la devianza)

names(ej1$BUGSoutput) #lo que tiene
out<-ej1$BUGSoutput #jags
out.mat<-ej1$BUGSoutput$sims.matrix #matriz de las simulaciones, cada valor estimado de la devianza y los par??metros
out.sum<-ej1$BUGSoutput$summary #estimaciones, resumen de la DIC y parametros
out.dic<-ej1$BUGSoutput$DIC #dic   
# a) 141.9001 
# b) 141.72 
# c) 140.68 
#entonces el c es el que mejor se ajusta a los dtos, la dist inicial no esta dando informaci??n adidionl es la mejor

# a) se nota que la theta estimada es muy parecida a la observaci??n del a??o en pasado
# b) se nota que la lambda estimada (normal transformada) es muy parecida a la observaci??n del a??o en pasado
# c) la inicial de referencia es la nueva theta**(confirmar)
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
inits<-function(){list(mu=0,sig=1)} #aqui es donde se puede cambiar a 

#seleccionar parametros al monitor
parameters<-c('mu','sig')

ej2<-jags(data,inits,parameters,model.file='E2.txt',n.iter=10000,n.chains=1,n.burnin=100) #se descarta el .10 en el calentamiento

#funcion jags
jags(data, inits, parameters.to.save, model.file="model.bug",
     n.chains=3, n.iter=2000, n.burnin=floor(n.iter/2),
     n.thin=max(1, floor((n.iter - n.burnin) / 1000)),
     DIC=TRUE, working.directory=NULL, jags.seed = 123,
     refresh = n.iter/50, progress.bar = "text", digits=5,
     RNGname = c("Wichmann-Hill", "Marsaglia-Multicarry",
                 "Super-Duper", "Mersenne-Twister"),
     jags.module = c("glm","dic")
)




traceplot(ej2) #traza de la cadena

out<-ej2$BUGSoutput$sims.list #jags
traceplot(ej2.sim) mu

out.mat<-ej2$BUGSoutput$sims.matrix
out.sum<-ej2$BUGSoutput$summary #estimaciones en esta salen los resultadaos
out.dic<-ej2$BUGSoutput$DIC #dic 76.71 para el b, 77.09649 para el a

#despues de los dos ejercicios vemos que las estimaciobnes no sesgan mucho, 
#en este caso si me sirve por q me da informaci??n

z<-out$theta
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z)
acf(z)



names(ej1$BUGSoutput) #lo que tiene