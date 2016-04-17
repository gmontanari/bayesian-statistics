###--- Ejercicios de clase ---###
options(repos="cran.itam.mx")

#--- My functions ---
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

#- Ilustraci???n del proceso de inferencia -

#Simulacion de datos Bernoulli
theta0 <- 0.6
n <- 10
x<-rbinom(n,1,theta0)

hist(x,freq=FALSE)

#Distribucion inicial para theta
a <- 1
b <- 1
theta<-seq(0,1,,100)
plot(theta,dbeta(theta,a,b),type="l")

#Distribucion final
a1 <- a + sum(x)
b1 <- b + n - sum(x)
plot(theta,dbeta(theta,a1,b1),type="l")

#Ambas
theta<-seq(0,1,,100)
ymax <- max(dbeta(theta,a,b),dbeta(theta,a1,b1))
plot(theta,dbeta(theta,a,b),type="l",ylim=c(0,ymax))
lines(theta,dbeta(theta,a1,b1),col=2)
abline(v=theta0,col=4)

#Aproximacion normal asintotica
mu <- (a1-1)/(a1+b1-2)
sig2 <- (a1-1)*(b1-1)/(a1+b1-2)^3
lines(theta,dnorm(theta,mu,sqrt(sig2)),col=3)


#### --- Estimacion Monte Carlo --- ####
x<-seq(-2,4,,100)
f<-function(x){
  out <- 5-(x-1)^2
  out <- ifelse (x < -1 | x>3,0,out)
  out
}
plot(x,f(x)*3/44,type="l",ylim=c(0,0.5))
lines(x,dnorm(x,0,1),lty=2,col=2)
lines(x,dnorm(x,1,2/3),lty=3,col=3)
lines(x,dnorm(x,1,1),lty=4,col=4)

#Caso 1: S=Normal est???ndar
mu<-0
sig<-1
N<-100000
x<-rnorm(N,mu,sig)
I1<-mean(f(x)/dnorm(x,mu,sig))
I1

#Caso 2: S=Normal no est???ndar
mu<-1
sig<-2/3
N<-100000
x<-rnorm(N,mu,sig)
I2<-mean(f(x)/dnorm(x,mu,sig))
I2

#Caso 3: S=Normal no est???ndar
mu<-1
sig<-1
N<-100000
x<-rnorm(N,mu,sig)
I3<-mean(f(x)/dnorm(x,mu,sig))
I3


#Algoritmo de Gibbs
install.packages("bayesm")
library(bayesm)
out<-rbiNormGibbs(rho=.95)
out<-rbiNormGibbs(rho=-0.5)


###########################################

install.packages("R2OpenBUGS")
install.packages("rjags")
install.packages("R2jags")
library(R2OpenBUGS)
#ibrary(rjags)
library(R2jags)

#-Working directory-
wdir<-"c:/Temp/"
setwd(wdir)

#--- Ejemplo 1---
#-Reading data-
n<-100
credito<-c(rep(1,n/2),rep(0,n/2))

#-Defining data-
data<-list("n"=n,"x"=credito)

#-Defining inits-
inits<-function(){list(theta=0.5,x1=rep(1,2))}
inits<-function(){list(lambda=0)}
#inits<-function(){list(theta=0.5,eta=1)}

#-Selecting parameters to monitor-
parameters<-c("theta","x1")
#parameters<-c("theta","eta")

#-Running code-
#OpenBUGS
ej1.sim<-bugs(data,inits,parameters,model.file="Ej1.txt",
              n.iter=5000,n.chains=1,n.burnin=500)
#JAGS
ej1.sim<-jags(data,inits,parameters,model.file="Ej1.txt",
              n.iter=5000,n.chains=1,n.burnin=500)

#-Monitoring chain-

#Traza de la cadena
traceplot(ej1.sim)

#Cadena

#OpenBUGS
out<-ej1.sim$sims.list

#JAGS
out<-ej1.sim$BUGSoutput$sims.list

z<-out$theta
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
#OpenBUGS
out.sum<-ej1.sim$summary

#JAGS
out.sum<-ej1.sim$BUGSoutput$summary

print(out.sum)

#DIC
out.dic<-ej1.sim$BUGSoutput$DIC
print(out.dic)

#---------------------#
#Mezcla de betas
w<-seq(0.01,0.99,,100)
pp<-0.3
fw<-pp*dbeta(w,10,10)+(1-pp)*dbeta(w,6,0.01)
par(mfrow=c(1,1))
plot(w,fw,type="l")

#--- Ejemplo 2---
#-Reading data-
utilidad<-c(212, 207, 210,
            196, 223, 193,
            196, 210, 202, 221)
n<-length(utilidad)

#-Defining data-
data<-list("n"=n,"x"=utilidad)

#-Defining inits-
inits<-function(){list(mu=0,sig=1,x1=0)}

#-Selecting parameters to monitor-
parameters<-c("mu","sig","x1")

#-Running code-
#OpenBUGS
ej2.sim<-bugs(data,inits,parameters,model.file="Ej2.txt",
              n.iter=5000,n.chains=1,n.burnin=500)
#JAGS
ej2.sim<-jags(data,inits,parameters,model.file="Ej2.txt",
              n.iter=5000,n.chains=1,n.burnin=500)

#-Monitoring chain-

#Traza de la cadena
traceplot(ej2.sim)

#Cadena

#OpenBUGS
out<-ej2.sim$sims.list

#JAGS
out<-ej2.sim$BUGSoutput$sims.list

z<-out$x1
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
#OpenBUGS
out.sum<-ej2.sim$summary

#JAGS
out.sum<-ej2.sim$BUGSoutput$summary

print(out.sum)

#DIC
out.dic<-ej2.sim$BUGSoutput$DIC


#--- Ejemplo 3 ---
#-Reading data-
calif<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/calificaciones.txt",header=TRUE)
n<-nrow(calif)
plot(calif)

#-Defining data-
data<-list("n"=n,"y"=calif$SP,"x"=calif$MO)

#-Defining inits-
inits<-function(){list(beta=rep(0,2),tau=1,yf=rep(0,n))}

#-Selecting parameters to monitor-
parameters<-c("beta","tau","yf")

#-Running code-
#OpenBUGS
ej3.sim<-bugs(data,inits,parameters,model.file="Ej3.txt",
              n.iter=10000,n.chains=1,n.burnin=1000)
#JAGS
ej3.sim<-jags(data,inits,parameters,model.file="Ej3.txt",
              n.iter=10000,n.chains=1,n.burnin=1000)

#-Monitoring chain-

#Traza de la cadena
traceplot(ej3.sim)

#Cadena

#OpenBUGS
out<-ej3.sim$sims.list

#JAGS
out<-ej3.sim$BUGSoutput$sims.list

z<-out$beta[,2]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
#OpenBUGS
out.sum<-ej3.sim$summary

#JAGS
out.sum<-ej3.sim$BUGSoutput$summary

print(out.sum)
head(out.sum)

#Probabilidades
z<-out$beta[,1]
prob(z)

#DIC
out.dic<-ej3.sim$DIC
out.dic<-ej3.sim$BUGSoutput$DIC
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


#--- Ejemplo 4 ---
#TAREA


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
#OpenBUGS
ej5a.sim<-bugs(data,inits,parameters,model.file="Ej5a.txt",
               n.iter=50000,n.chains=1,n.burnin=5000)
ej5b.sim<-bugs(data,inits,parameters,model.file="Ej5b.txt",
               n.iter=50000,n.chains=1,n.burnin=5000)
ej5c.sim<-bugs(data,inits,parameters,model.file="Ej5c.txt",
               n.iter=50000,n.chains=1,n.burnin=5000,debug=TRUE)
#JAGS
ej5.sim<-jags(data,inits,parameters,model.file="Ej5c.txt",
              n.iter=50000,n.chains=1,n.burnin=5000)

#-Monitoring chain-

#Traza de la cadena
traceplot(ej5.sim)

#Cadena

#OpenBUGS
out<-ej5.sim$sims.list

#JAGS
out<-ej5.sim$BUGSoutput$sims.list

z<-out$beta[,2]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
#OpenBUGS
out.sum<-ej5c.sim$summary

#JAGS
out.sum<-ej5.sim$BUGSoutput$summary

print(out.sum)
head(out.sum)

#Probabilidades
z<-out$beta[,1]
prob(z)

#DIC
out.dic<-ej5c.sim$DIC
out.dic<-ej5.sim$BUGSoutput$DIC
print(out.dic)

#Predictions
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
or<-order(mortality$x)
ymin<-min(mortality$y,out.yf[,c(1,3,7)])
ymax<-max(mortality$y,out.yf[,c(1,3,7)])

par(mfrow=c(1,1))
plot(mortality$x,mortality$y,ylim=c(ymin,ymax))
#Modelo 1
lines(mortality$x[or],out.yf[or,1],lwd=2,col=2)
lines(mortality$x[or],out.yf[or,3],lty=2,col=2)
lines(mortality$x[or],out.yf[or,7],lty=2,col=2)
#Modelo 2
lines(mortality$x[or],out.yf[or,1],lwd=2,col=3)
lines(mortality$x[or],out.yf[or,3],lty=2,col=3)
lines(mortality$x[or],out.yf[or,7],lty=2,col=3)

plot(mortality$y,out.yf[,1])
abline(a=0,b=1)
cor(mortality$y,out.yf[,1])


#--- Ejemplo 6 ---
#-Reading data-
desastres<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/desastres.txt",header=TRUE)
n<-nrow(desastres)
plot(desastres,type="l")
plot(desastres[2:n,2]-desastres[1:(n-1),2],type="l")
plot(log(desastres[2:n,2])-log(desastres[1:(n-1),2]),type="l")


#-Defining data-
data<-list("n"=n,"y"=desastres$No.Desastres,"x"=desastres$Anho)

#-Defining inits-
inits<-function(){list(beta=rep(0,2),aux=1,aux2=1,yf1=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("beta","r","tau","yf1","mu")

#-Running code-
#OpenBUGS
ej6a.sim<-bugs(data,inits,parameters,model.file="Ej6a.txt",
               n.iter=50000,n.chains=1,n.burnin=5000)
ej6b.sim<-bugs(data,inits,parameters,model.file="Ej6b.txt",
               n.iter=50000,n.chains=1,n.burnin=5000,debug=TRUE)
#JAGS
ej6.sim<-jags(data,inits,parameters,model.file="Ej5c.txt",
              n.iter=50000,n.chains=1,n.burnin=5000)

#-Monitoring chain-

#Traza de la cadena
traceplot(ej6.sim)

#Cadena

#OpenBUGS
out<-ej6b.sim$sims.list

#JAGS
out<-ej6a.sim$BUGSoutput$sims.list

z<-out$tau
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
#OpenBUGS
out.sum<-ej6b.sim$summary

#JAGS
out.sum<-ej6a.sim$BUGSoutput$summary

print(out.sum)
head(out.sum)

#Probabilidades
z<-out$beta[,1]
prob(z)

#DIC
out.dic<-ej6b.sim$DIC
out.dic<-ej6a.sim$BUGSoutput$DIC
print(out.dic)

#Predictions
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
ymin<-min(desastres[,2],out.yf[,c(1,3,7)])
ymax<-max(desastres[,2],out.yf[,c(1,3,7)])

par(mfrow=c(1,1))
plot(desastres,type="l",col="grey80",ylim=c(ymin,ymax))
lines(desastres[,1],out.yf[,1],lwd=2,col=2)
lines(desastres[,1],out.yf[,3],lty=2,col=2)
lines(desastres[,1],out.yf[,7],lty=2,col=2)

#Medias
out.mu<-out.sum[grep("mu",rownames(out.sum)),]
par(mfrow=c(1,1))
plot(desastres,type="l",col="grey80")
lines(desastres[,1],out.mu[,1],lwd=2,col=2)
lines(desastres[,1],out.mu[,3],lty=2,col=2)
lines(desastres[,1],out.mu[,7],lty=2,col=2)