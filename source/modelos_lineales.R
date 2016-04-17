##-----------------------MODELOS LINEALES--------------------------##

##A continuaci??n se presenta una base de datos de calificaciones de 20 empresas financieras 
# hechas por las dos compa????as calificadores m??s importantes S&P y Moody???s. 
# Realiza un an??lisis Bayesiano completo de los datos, ajustando un modelo de regresi??n lineal, 
# tomando como variable respuesta las calificaciones de S&P y como variable explicativa 
# las calificaciones de Moody???s


#--- My functions ---
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

#--------------------------Ejercicio 3------------------------------
setwd("~/Desktop/Reg_Avanzada")
library(rjags)
library(R2jags)
calif<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/calificaciones.txt",header = TRUE)
n<-nrow(calif)
plot(calif) #se ve que es un modelo de regresi??n 
##MO MOoddy's van a ser las X, y SP van a ser las Y

#------los datos siempre se enlistan (en este caso tenemos variable explicativa y respuesta)
data<-list("n"=n, "y"=calif$SP, "x"=calif$MO) 

#------definiendo inits donde va a comenzar la cadena (son las funciones iniciales)
inits<-function(){list(beta=rep(0,2),tau=1,yf=rep(0,n))} 

#------seleccionar parametros al monitor
parameters<-c('beta','tau','yf')

ej3<-jags(data,inits,parameters,model.file='Ex3_1.txt',n.iter=10000,n.chains=1,n.burnin=1000) 
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

#--------------------Monitoring chain----------------------#

#Traza de la cadena
traceplot(ej3)

#Cadena
out<-ej3$BUGSoutput$sims.list

z<-out$beta[,2]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
out.sum<-ej3$BUGSoutput$summary

print(out.sum)
head(out.sum)

#DIC
out.dic<-ej3$BUGSoutput$DIC
print(out.dic)

#Predictions
out.yf<-out.sum[grep("yf",rownames(out.sum)),]
or<-order(calif$MO)
ymin<-min(calif$SP,out.yf[,c(1,3,7)])
ymax<-max(calif$SP,out.yf[,c(1,3,7)])
par(mfrow=c(1,1))
plot(calif$MO,calif$SP,ylim=c(ymin,ymax))
lines(calif$MO[or],out.yf[or,1],lwd=2,col=2)
lines(calif$MO[or],out.yf[or,3],lty=2,col=2)
lines(calif$MO[or],out.yf[or,7],lty=2,col=2)





#--------------------------Ejercicio 4------------------------------

# Un investigador desea evaluar la relaci??n entre el salario anual de trabajadores de una compa????a de 
# nivel medio y alto (Y, en miles de d??lares) y el ??ndice de calidad de trabajo (X1), n??mero de a??os 
# de experiencia (X2) y el ??ndice de ??xito en publicaciones (X3). La muestra consiste de 
# 24 trabajadores. Realiza un an??lisis Bayesiano completo de los datos y obt??n las predicciones 
# de salarios para 3 nuevos empleados con variables explicativas:

    
setwd("~/Desktop/Reg_Avanzada")
library(rjags)
library(R2jags)
#salarios<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/salarios.txt",header = TRUE)
n<-nrow(salarios)
plot(salarios) #se ve que es un modelo de regresi??n 

#------los datos siempre se enlistan (en este caso tenemos 3 variables explicativas)
data<-list("n"=n, "y"=salarios$Y, "x1"=salarios$X1, "x2"=salarios$X2, "x3"=salarios$X3) 
# ??ndice de calidad de trabajo (X1)
# n??mero de a??os de experiencia (X2)
# el ??ndice de ??xito en publicaciones (X3)

predictors <- c(salarios$X1, salarios$X2, salarios$X3)
x= as.matrix(salarios[,-1]) #agarra la tabla salarios y le quita la Y

#Valores para predicci??n X's

m<-3
dataF <- list(x1F=c(5.4, 6.2, 6.4),  x2F=c(17, 12, 21),  x3F=c(6.0, 5.8, 6.1))
xF=as.data.frame(dataF)
dataPred <- list('x'=x,'y'=salarios$Y,'n'=n, 'xF'=xF, 'm'=m)

#------definiendo inits donde va a comenzar la cadena (son las funciones iniciales)
# inits<-function(){list(beta=rep(0,4),tau=1,yf=rep(0,n))} 
inits<-function(){list(b0=0,beta=rep(1,m),tau=1,yf=rep(0,n),yf2=rep(0,m))} ##prediccion

#------seleccionar parametros al monitor
parameters<-c('b0','beta','tau','yf','yf2')

# ej4<-jags(data,inits,parameters,model.file='E4.txt',n.iter=10000,n.chains=1,n.burnin=1000)
ej4<-jags(dataPred,inits,parameters,model.file='E4.txt',n.iter=5000,n.chains=1,n.burnin=500) 
#se descarta el .10 en el calentamiento, el n??mero de iteraciones es por cadena, inits es lista o funci??n


#--------------------Monitoring chain----------------------#
#probabilidad de que mi beta1
prob <- function(x){
  out <- min(length(x[x>0])/length(x), type="l")
  out
}

#Traza de la cadena
traceplot(ej4)

#Cadena
out<-ej4$BUGSoutput$sims.list

z<-out$beta[,1]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
out.sum<-ej4$BUGSoutput$summary
print(out.sum)
head(out.sum)

#DIC
out.dic<-ej4$BUGSoutput$DIC
print(out.dic)

#Predictions
out.yf <- out.sum[grep("yf", rownames(out.sum)),]
yfs<- (out.yf[(1:n),1])
or <- order(yfs)
ymin <- min (salarios$Y, out.yf[,c(1,3,7)])
ymax <- max (salarios$Y, out.yf[,c(1,3,7)])
par(mfrow=c(1,1))

plot(yfs, salarios$Y)
points(yfs, yfs ,col=2)

plot(yfs[or], salarios$Y[or])
lines(yfs[or], out.yf[or ,1],lwd=2,col=2)
lines(yfs[or], out.yf[or ,3],lty=2,col=2)
lines(yfs[or], out.yf[or ,7],lty=2,col=2)




#--------------------------Ejercicio 5------------------------------

# Tasas de mortalidad (Congdon, 2001). Una compa????a de seguros quiere lanzar un nuevo seguro m??dico 
# para mineros. Para ello desea estimar la probabilidad de muerte (????i), con base en el tiempo d
# e exposici??n al mineral (xi en horas). Se cuenta con informaci??n de las muertes registradas 
# entre 1950 y 1959, junto con el tiempo de exposici??n al mineral y el n??mero de mineros expuestos. 
# Realiza un an??lisis Bayesiano de los datos y obt??n la distribuci??n predictiva del n??mero de muertes 
# suponiendo que hay 100 mineros con un tiempo de exposici??n de 200 horas. El modelo es el siguiente: 
#   Para i????1,...,N

#tenemos 
# pi_i=probabilidad de muerte
# x_i= tiempo de exposicion al mineral
# y_i= muertes registradas (y estan en naturales)

1)vemos la piosson como primera opcion
y~poi(mu)
mu=riesgo(tasa de muerte)
------ligas canonicas
a) log(mu)=beta0 + beta1*x1
b) log_10(mu)=beta0 + beta1*x1
------para los estimadores
a)beta0~N(0,0.0001), beta1 tambien
b)beta0~st(0,0.001,2)

2) opcion 2 la geometrica
y~geo(p_i)
m_i=(1-p)/p = n_i*theta_i
entonces
p_i=1/(1+n_i*theta_i) theta_i vive en reales positivos
---ligas canonicas
a)log(theta_i)=beta0+beta1*x_i
b)log(-log(theta/(1+theta_i)))

3)y~
bin(n,p)
m=np
--canonicas
a)log(p/(1-p))=beta0 + beta1*x1
b)log(-log(p))
c)log(-log(1-p))

4)geometrica
mu=n*theta
mu=(1-p)/p
p=1/(1+n*theta)
log(theta)=beta0+beta1*x

##hay que considerar graficar la liga
#########aqui empieza

help(jags)
setwd("~/Desktop/Reg_Avanzada")
library(rjags)
library(R2jags)
#--- Ejemplo 5 ---
#-Reading data-
mortality<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/mortality.txt",header=TRUE)
n<-nrow(mortality)
plot(mortality)
m<-1
nef<-c(100)
xf<-c(200)

#-Defining data-
data<-list("n"=n,"ne"=mortality$n,"y"=mortality$y,"x"=mortality$x,"m"=m,"nef"=nef,"xf"=xf)

#-Defining inits-
inits<-function(){list(beta=rep(0,2),yf1=rep(1,n),yf2=1)}

#-Selecting parameters to monitor-
parameters<-c("beta","yf1","yf2")

#-Running code-
ej5.sim<-jags(data,inits,parameters,model.file="E5.txt",
              n.iter=10000,n.chains=1,n.burnin=1000)
#-Monitoring chain-
#Traza de la cadena
traceplot(ej5.sim)
#JAGS
out<-ej5.sim$BUGSoutput$sims.list

z<-out$beta[,2]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
out.sum<-ej5.sim$BUGSoutput$summary

print(out.sum)
head(out.sum)
#la beta0 es como el promedio sin exposicion
#con 100 obreros expuestos equivale a lambda=exp(beta0)=.02
#cuando se aumentan las horas de exposicion al mneral x=100 entonces
#lambda=exp(beta0+beta1*100)=.07
#con x=200 entonces lambda=.2
#calcular la tasa relativa lamnda2/lambda1 ->exp(beta1(x2-x1))

#notar que es una acumulacion exponencial 


#Probabilidades
z<-out$beta[,1]
prob(z)

#DIC
out.dic<-ej5.sim$DIC
out.dic<-ej5.sim$BUGSoutput$DIC
print(out.dic)

#Predictions
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
or<-order(mortality$x)
ymin<-min(mortality$y,out.yf[,c(1,3,7)])
ymax<-max(mortality$y,out.yf[,c(1,3,7)])
par(mfrow=c(1,1))
plot(mortality$x,mortality$y,ylim=c(ymin,ymax))
lines(mortality$x[or],out.yf[or,1],lwd=2,col=2)
lines(mortality$x[or],out.yf[or,3],lty=2,col=2)
lines(mortality$x[or],out.yf[or,7],lty=2,col=2)

##reportar la correlacion
#reportar la comparacion de las variables predichas con su equivalente original


#----------------------------------- Ejemplo 6 -----------------------------------------------
#-Reading data-
##es una serie de tiempo
##lo que cambia es la media de los datos (datos dsicretos positivos) conteos 
#through the eyes of statisian (video)
#ara este ejemplo podemos tomar 2 tipos de verosimilitudes
1)y_poi(mu) con canonica log(mu) =b0+b1t+b2t^2
2)y_binneg(r,pt) con canonica log(pt/(1-pt) = b0 + b1t = b0+b1I(t>tau)

desastres<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/desastres.txt",header=TRUE)
n<-nrow(desastres)
plot(desastres,type='l')

#-Defining data-
data<-list("n"=n,"ne"=mortality$n,"y"=mortality$y,"x"=mortality$x,"m"=m,"nef"=nef,"xf"=xf)

#-Defining inits-
inits<-function(){list(beta=rep(0,2),yf1=rep(1,n),yf2=1)}

#-Selecting parameters to monitor-
parameters<-c("beta","yf1","yf2")

#-Running code-
ej5.sim<-jags(data,inits,parameters,model.file="E5.txt",
              n.iter=10000,n.chains=1,n.burnin=1000)
#-Monitoring chain-
#Traza de la cadena
traceplot(ej5.sim)
#JAGS
out<-ej5.sim$BUGSoutput$sims.list

z<-out$beta[,2]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
out.sum<-ej5.sim$BUGSoutput$summary

print(out.sum)
head(out.sum)
#la beta0 es como el promedio sin exposicion
#con 100 obreros expuestos equivale a lambda=exp(beta0)=.02
#cuando se aumentan las horas de exposicion al mneral x=100 entonces
#lambda=exp(beta0+beta1*100)=.07
#con x=200 entonces lambda=.2
#calcular la tasa relativa lamnda2/lambda1 ->exp(beta1(x2-x1))

#notar que es una acumulacion exponencial 


#Probabilidades
z<-out$beta[,1]
prob(z)

#DIC
out.dic<-ej5.sim$DIC
out.dic<-ej5.sim$BUGSoutput$DIC
print(out.dic)

#Predictions
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
or<-order(mortality$x)
ymin<-min(mortality$y,out.yf[,c(1,3,7)])
ymax<-max(mortality$y,out.yf[,c(1,3,7)])
par(mfrow=c(1,1))
plot(mortality$x,mortality$y,ylim=c(ymin,ymax))
lines(mortality$x[or],out.yf[or,1],lwd=2,col=2)
lines(mortality$x[or],out.yf[or,3],lty=2,col=2)
lines(mortality$x[or],out.yf[or,7],lty=2,col=2)

##reportar la correlacion
#reportar la comparacion de las variables predichas con su equivalente original
