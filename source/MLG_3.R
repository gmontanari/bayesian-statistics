##A continuaci??n se presenta una base de datos de calificaciones de 20 empresas financieras 
# hechas por las dos compa????as calificadores m??s importantes S&P y Moody???s. 
# Realiza un an??lisis Bayesiano completo de los datos, ajustando un modelo de regresi??n lineal, 
# tomando como variable respuesta las calificaciones de S&P y como variable explicativa 
# las calificaciones de Moody???s
#--------------------------Ejercicio 3------------------------------

library(rjags)
library(R2jags)
calif<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/calificaciones.txt",header = TRUE)
n<-nrow(calif)
plot(calif) #se ve que es un modelo de regresi??n 
vec<-seq(1,n,1)
table<-data.frame(vec,calif$MO,calif$SP)
colnames(table)<- c("Empresa", "Moodys", "SP")
p<-ggplot(data=table, aes(x=Empresa,y=Moodys),colour('red')) +geom_line() 
p + geom_line(data=table, aes(y=SP),colour='brown')
##MO MOoddy's van a ser las X, y SP van a ser las Y

#------los datos siempre se enlistan (en este caso tenemos variable explicativa y respuesta)
data<-list("n"=n, "y"=calif$SP, "x"=calif$MO) 

#------definiendo inits donde va a comenzar la cadena (son las funciones iniciales)
#inits<-function(){list(beta=rep(0,2),tau=1,yf=rep(0,n))} 
inits<-function(){list(beta0=0, beta1=0, tau=1, yf=rep(0,n))} 

#------seleccionar parametros al monitor
#parameters<-c('beta','tau','yf')
parameters<-c('beta0','beta1','tau','yf')

model <- function(){
#Likelihood
for ( i in 1:n ) {
  y[ i ] ~ dnorm(mu[i],tau) #cada y_i se distribuye normal mu tau
  #mu[i]<- beta[1]+ beta[2]*x[i]  #mu es el estimador lineal
  mu[i] <- beta0+ beta1*x[i]
}
#priors
#for (j in 1:2) { beta[j] ~ dnorm(0,0.001) }
beta0 ~ dnorm(0,0.001)
beta1 ~ dnorm(0,0.001)
tau ~ dgamma (0.001, 0.001)
#PREDICTION
for (i in 1:n) { yf[i] ~ dnorm (mu[i], tau)}
}

#correr primero sin calentamiento y ver la autocorrelacion y graficar, despues quitar 
ej3<-jags(data,
          inits,
          parameters,
          model.file=model, #'E3.txt',
          n.chains=1,
          n.iter=10000,
          #n.burnin = 0,
          n.burnin=4000,
          #cworking.directory = "~/Desktop/Reg_Avanzada",
          DIC = TRUE,
          jags.seed = 159549) 
#se descarta el .10 en el calentamiento, el n??mero de iteraciones es por cadena, inits es lista o funci??n
#en este caso despues de ver las graficas calentamos 400 iteraciones

#--------------------Monitoring chain----------------------#

#Traza de la cadena
traceplot(ej3)

#Cadena
out<-ej3$BUGSoutput$sims.list

#z<-out$beta[,2]
b0<-out$beta0
b1<-out$beta1
DIC<-out$deviance
par(mfrow=c(3,2)) 
plot(b0,type="l")
plot(cumsum(b0)/(1:length(b0)),type="l") #estabilizacion 
plot(b1,type="l")
plot(cumsum(b1)/(1:length(b1)),type="l") #estabilizacion 
plot(DIC,type="l")
plot(cumsum(DIC)/(1:length(DIC)),type="l") #estabilizacion 
#pendientes por asignar
hist(b0,freq=FALSE) #histograma 
acf(b0) #autocorrelograma

#Resumen (estimadores)
out.sum<-ej3$BUGSoutput$summary

print(out.sum)
head(out.sum,4) #numero de parametros

#DIC
out.dic<-ej3$BUGSoutput$DIC
print(out.dic)

#Predictions
out.yf<-out.sum[grep("yf",rownames(out.sum)),]
or<-order(calif$MO)
# ymin<-min(calif$SP,out.yf[,c(1,3,7)]) #selecciona la media, y los cuantiles 2.5 y 97.5
# ymax<-max(calif$SP,out.yf[,c(1,3,7)])
par(mfrow=c(1,1))

tabla<-data.frame(calif$MO,calif$SP,calif$MO[or],out.yf[or,1],out.yf[or,3],out.yf[or,7])
colnames(tabla)<- c("Moodys", "SP", "Order", "Media", "lim_inf","lim_sup")
p<-ggplot(data=tabla, aes(x=Moodys,y=SP)) + geom_point(shape=1) 
p + geom_line(data=tabla, aes(x=Order, y=Media),colour='red') +
  geom_line(data=tabla, aes(x=Order, y=lim_inf),colour='red') +
  geom_line(data=tabla, aes(x=Order, y=lim_sup),colour='red')
  

# plot(calif$MO,calif$SP,ylim=c(ymin,ymax))
# lines(calif$MO[or],out.yf[or,1],lwd=2,col=2)
# lines(calif$MO[or],out.yf[or,3],lty=2,col=2)
# lines(calif$MO[or],out.yf[or,7],lty=2,col=2)
