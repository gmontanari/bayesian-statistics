

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

#el problema quiere la prob de muerte, por lo que se debe encontrar la predictiva

#--------------------------Ejercicio 5------------------------------

library(rjags)
library(R2jags)
library(ggplot2)
mortality<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/mortality.txt",header=TRUE)
n<-nrow(mortality)
nef<-c(100) #numero de expuestos final
xf<-c(200) #horas de exposicion final

#en este caso ya es un dataframe
table <- mortality
colnames(table)<- c("Exposicion", "Muertes", "Mineros")
p <- ggplot(data=table, aes(x=Exposicion, y=Muertes)) +geom_line() 
p + geom_line(data=table, aes(y=Mineros), colour='brown')

#-Defining data-
data<-list("n"=n,"ne"=mortality$n,"y"=mortality$y,"x"=mortality$x,"nef"=nef,"xf"=xf) #"m"=m,nuevas muertes

#-Defining inits-
inits<-function(){list(beta0=0, beta1=0, y_hat=rep(1,n), yf=1)}

#-Selecting parameters to monitor-
parameters<-c("beta0", "beta1", "y_hat", "yf")

#-Defining model-
bin_model <- function(){
  #Likelihood
  for (i in 1:n) {
    y[i] ~ dbin(p[i],ne[i])
#    mu[i] <- p[i]*ne[i]
     logit(p[i]) <- beta0+beta1*x[i]     #Liga logistica
#     eta[i]<-beta0+beta1*x[i]            #Liga loglog
#     p[i]<-exp(-exp(eta[i]))   
#     cloglog(p[i])<-beta0+beta1*x[i]     #liga cloglog
  }
  #Priors 
  beta0 ~ dnorm(0,0.001)
  beta1 ~ dnorm(0,0.001)
  #Estimacion y_hat
  for (i in 1:n) {
    y_hat[i] ~ dbin(p[i],ne[i]) 
  }
  #Prediction
    yf ~ dbin(pf,nef) 
#    muf <- pf*nef
     logit(pf)<-beta0+beta1*xf
#     eta1<-beta0+beta1*xf
#     pf<-exp(-exp(eta1))
#     cloglog(pf)<-beta0+beta1*xf
}

poi_model <- function(){
  #Likelihood
  for (i in 1:n) {
    y[i] ~ dpois(lambda[i])
    lambda[i] <- ne[i]*p[i]
    log(p[i]) <- beta0 + beta1*x[i]       #Liga log
    }
  #Priors 
  beta0 ~ dnorm(0,0.001)
  beta1 ~ dnorm(0,0.001)
  #Estimacion y_hat
  for (i in 1:n) { 
    y_hat[i] ~ dpois(lambda[i]) 
  }
  #Prediction
  yf ~ dpois(lambdaf) 
  lambdaf <- nef*pf
  log(pf) <- beta0 + beta1*xf
  }


#-Running code-
fit<-jags(data,
          inits,
          parameters,
          model.file=
            bin_model,
            #poi_model,
          n.chains=1,
          n.iter=10000,
          #n.burnin = 0,
          n.burnin=4000,
          #working.directory = "~/Desktop/Reg_Avanzada",
          DIC = TRUE,
          jags.seed = 159549) 


#--------------------Monitoring chain----------------------#
#Traza de la cadena
traceplot(fit)
#Cadena
out<-fit$BUGSoutput$sims.list
out.DIC<-fit$BUGSoutput$DIC


b0<-out$beta0
b1<-out$beta1
DIC<-fit$BUGSoutput$DIC
par(mfrow=c(3,3)) 
plot(b0,type="l",col='brown')
plot(cumsum(b0)/(1:length(b0)),type="l", ylab = 'Estabilizacion')
acf(b0)
plot(b1,type="l",col='brown')
plot(cumsum(b1)/(1:length(b1)),type="l", ylab = 'Estabilizacion')
acf(b1)
plot(DIC,type="l",col='brown')
plot(cumsum(DIC)/(1:length(DIC)),type="l", ylab = 'Estabilizacion') 
acf(DIC)

#Resumen (estimadores)
out.sum<-fit$BUGSoutput$summary
out.sum
print(out.sum[1:3,1:3])


#--------------------Comentarios del modelo----------------------#

#---------opcion binomial-logit    DIC=25.63507186
beta0<- out.sum[1,1]
beta1<- out.sum[2,1]
pi0<-exp(beta0)/(1+exp(beta0)) #la beta0 es como el promedio sin exposicion (aqui no hay exposiciones)
pi<-exp(beta0+beta1*xf)/(1+exp(beta0+beta1*xf))
# 0.2231457 prob muerte
out.sum[10,1]

#---------opcion binomial-loglog    DIC=26.020220961
beta0<- out.sum[1,1]
beta1<- out.sum[2,1]
pi0<exp(-exp(beta0))
pi<exp(-exp(beta0+beta1*xf))
# 0.3228225 prob muerte

#---------opcion binomial-cloglog    DIC=24.45384737
beta0<- out.sum[1,1]
beta1<- out.sum[2,1]
pi0<- 1-exp(-exp(beta0))
pi<- 1-exp(-exp(beta0+beta1*xf))
# 0.213065 prob muerte

#----------opcion poisson-log    DIC=25.28557861
beta0<- out.sum[1,1]
beta1<- out.sum[2,1]
pi0<- exp(beta0)
pi<- exp(beta0+beta1*xf)
# 0.2046498 prob muerte
out.sum[10,1]


#calcular la tasa relativa lamnda2/lambda1 ->exp(beta1(x2-x1))

#notar que es una acumulacion exponencial 

#ir comparando las DIC y las probabilidades y compararlas en un cuadro

#reportar la comparacion de las variables predichas con su equivalente original

out.yf<-out.sum[grep("y_hat",rownames(out.sum)),]
or<-order(mortality$x)
par(mfrow=c(2,2))

#ajuste del modelo
tabla<-data.frame(mortality$x,mortality$y,mortality$x[or],out.yf[or,1],out.yf[or,3],out.yf[or,7])
colnames(tabla)<- c("Exposicion", "Muertes", "Order", "Media", "lim_inf","lim_sup")
p1 <-ggplot(data=tabla, aes(x=Exposicion,y=Muertes)) + geom_point(size=3) 
p1 + geom_line(data=tabla, aes(x=Order, y=Media),colour='#CC6633',size=1) +
  geom_freqpoly(data=tabla, aes(x=Order, y=lim_inf),colour='#CC6633',linetype="dashed") +
  geom_line(data=tabla, aes(x=Order, y=lim_sup),colour='#CC6633',linetype="dashed") +
  geom_point(aes(x=xf,y=out.sum[10,1]), colour='#3366CC',size=4) +
  ggtitle("Numero de muertes explicado por la exposicion")

 help(geom_freqpoly)
#estimaciones vs reales
tab_est<-data.frame(mortality$y,out.yf[,1])
colnames(tab_est)<- c("Reales", "Estimados")
p2 <-ggplot(tab_est, aes(Reales,Estimados)) + geom_point(size=3)
p2 + geom_abline(intercept=0,slope=1,colour='#CC6633') +
  ggtitle("Comparativo de ajuste del modelo")



