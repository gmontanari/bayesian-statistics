
# Un investigador desea evaluar la relación entre el salario
# anual de trabajadores de una compañía de nivel medio y alto (Y, en miles
# de dólares) y el índice de calidad de trabajo (X1), número de años de
# experiencia (X2) y el índice de éxito en publicaciones (X3). La muestra
# consiste de 24 trabajadores. Realiza un análisis Bayesiano completo de los
# datos y obtén las predicciones de salarios para 3 nuevos empleados con
# variables explicativas

#--------------------------Ejercicio 4------------------------------

library(rjags)
library(R2jags)
library(ggplot2)
salarios<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/salarios.txt",header=TRUE)
#datos conocidos
n<-nrow(salarios)
x<-data.frame(salarios$X1,salarios$X2,salarios$X3)
colnames(x)<-c('X1','X2','X3')
#datos a predecir
m<-3
xf<-data.frame(matrix(c(5.4,6.2,6.4,17.0,12.0,21.0,6.0,5.8,6.1),3,3))

#-Data visualization
p <- ggplot(salarios, aes(x=Y, y=X1)) + geom_point( colour='red', size=3)
p + geom_point(data=salarios, aes(y=X2), colour='blue', size=3) +
   geom_point(data=salarios, aes(y=X3), size=3)


#-Defining data-
data <- list('n'=n,'x'=x,'y'=salarios$Y,'m'=m,'xf'=xf)

#-Defining inits-
inits<-function(){list(beta0=0,beta=rep(0,m),tau=1,y_hat=rep(0,n),yf=rep(0,m))}

#-Selecting parameters to monitor-
parameters<-c('beta0','beta','tau','y_hat','yf')

#-Defining model-
norm_model <- function(){
  #Likelihood
  for (i in 1:n) {
    y[i] ~ dnorm(mu[i],tau)
    mu[i] <- beta0+beta[1]*x[i,1]+beta[2]*x[i,2]+beta[3]*x[i,3]
  }
  #Priors 
  beta0 ~ dnorm(0,0.001)
  for (i in 1:m) {
    beta[i] ~ dnorm(0,0.001)
  }
  tau ~ dgamma(0.001,0.001)
  #Estimacion y_hat
  for (i in 1:n) { 
    y_hat[i] ~ dnorm(mu[i],tau)
  }
  #Prediction
  for (i in 1:m) {
    yf[i] ~ dnorm(muf[i],tau)
    muf[i] <- beta0+beta[1]*xf[i,1]+beta[2]*xf[i,2]+beta[3]*xf[i,3]
   }
}



#-Running jags-
fit<-jags(data,
          inits,
          parameters,
          model.file=
            norm_model,
          n.chains=1,
          n.iter=10000,
          #n.burnin = 0,
          n.burnin=1000,
          DIC = TRUE,
          jags.seed = 159549) 


#--------------------Monitoring chain----------------------#
#Cadena
out<-fit$BUGSoutput$sims.list

par(mfrow=c(5,3)) 
for (i in 1:m) {
  betas<-out$beta[,i]
  plot(betas,type="l",col='brown')
  plot(cumsum(betas)/(1:length(betas)),type="l", ylab = 'Estabilizacion')
  acf(betas)
}
b0<-out$beta0
DIC<-out$deviance
plot(b0,type="l",col='brown')
plot(cumsum(b0)/(1:length(b0)),type="l", ylab = 'Estabilizacion')
acf(b0)
plot(DIC,type="l",col='brown')
plot(cumsum(DIC)/(1:length(DIC)),type="l", ylab = 'Estabilizacion') 
acf(DIC)

#Resumen (estimadores)
out.sum<-fit$BUGSoutput$summary
out.sum

#--------------------Comentarios del modelo----------------------#

#----------opcion normal-identidad    DIC=96.228307
out.yf<-out.sum[grep("yf",rownames(out.sum)),]
out.beta<-out.sum[grep("beta",rownames(out.sum)),]
out.DIC<-out.sum[grep("deviance",rownames(out.sum)),]
out.y_hat<-out.sum[grep("y_hat",rownames(out.sum)),]
out.yf[,1] #prediccion
out.DIC[1]

#ir comparando las DIC y las probabilidades y compararlas en un cuadro

or.y_hat<-sort(out.y_hat[,1])
or.y<-sort(salarios$Y)

#estimaciones vs reales
tab_est<-data.frame(or.y,or.y_hat)
colnames(tab_est)<- c("Reales", "Estimados")
p2 <-ggplot(tab_est, aes(Reales,Estimados)) + geom_point(size=3)
p2 + geom_abline(intercept=0,slope=1,colour='#CC6633') +
  ggtitle("Comparativo de ajuste del modelo")



