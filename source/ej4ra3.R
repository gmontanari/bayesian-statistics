#EJEMPLO 4
# Load auxiliary functions for plotting the MCMC chain:
library(rjags)
library(R2jags)
#-----------------------------------------------

setwd("~/Desktop/Reg_Avanzada")
#setwd("/Volumes/EHZ/ITAM/REGRESION AVANZADA/R EJERCICIOS")

#DATA
######

wage <- read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/salarios.txt", header=TRUE)
n <- nrow(wage)
plot(wage)

#-Defining data-
data<-list("n"=n,"y"=wage$Y, "x1"=wage$X1, "x2"=wage$X2, "x3"=wage$X3)
n=n
predictors <- c(wage$X1, wage$X2, wage$X3)
y= as.matrix(wage$Y)
x= as.matrix(wage[,-1])
predictorNames=colnames(wage)[-1]
nPred=ncol(x)

#Valores para predicci??n X's
x1F <- c(5.4, 6.2, 6.4) 
x2F <- c(17, 12, 21)
x3F <- c(6.0, 5.8, 6.1)

datapred <- list(
  x1F=x1F,
  x2F=x2F,
  x3F=x3F
)
xF= as.data.frame(datapred)
nF= nrow(xF)

dataList <- list(
  x=x,
  y=as.vector(y),
  n=n,
  nPred=nPred,
  xF=xF,
  nF=nF
)

#-Defining inits-
inits<-function(){list(b=rep(1:nPred),tau=1, yf=rep(0,n), yPred=rep(0,nF),b0=0)}

#-Selecting parameters to monitor-
parameters<-c("b","yf","tau","yPred","b0")

#JAGS
######

#-Running code-

ej4_1.sim<-jags(dataList,inits,parameters,model.file="Ex4_1.txt", n.iter=5000,n.chains=1,n.burnin=500)

#-Monitoring chain-

#Traza de la cadena
traceplot(ej4_1.sim)

#MY FUNCTIONS

#probabilidad de que mi beta1
prob <- function(x){
  out <- min(length(x[x>0])/length(x), type="l")
  out
}
#Cadena

#JAGS
out<-ej4_1.sim$BUGSoutput$sims.list

z<-out$b[,1]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)



#JAGS
out.sum<-ej4_1.sim$BUGSoutput$summary

print(out.sum)
head(out.sum,10L)


#DIC
out.dic<-ej4_1.sim$BUGSoutput$DIC
print(out.dic)
yfs<- (out.yf[,1])

#wagex <- ((wage$X1+wage$X2+wage$X3)/3)
#PREDICCION
out.yf <- out.sum[grep("yf", rownames(out.sum)),]
or <- order(yfs)
ymin <- min (wage$Y, out.yf[,c(1,3,7)])
ymax <- max (wage$Y, out.yf[,c(1,3,7)])
par(mfrow=c(1,1))

plot(yfs, wage$Y)
points(yfs, out.yf[,1],col=2)

plot(yfs[or], wage$Y[or])
lines(yfs[or], out.yf[or ,1],lwd=2,col=2)
lines(yfs[or], out.yf[or ,3],lty=2,col=2)
lines(yfs[or], out.yf[or ,7],lty=2,col=2)
