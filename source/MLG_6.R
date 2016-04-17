
# 
# En el mismo contexto del problema anterior, supongamos
# ahora que la compañía de seguros está interesada en modelar el número
# total de desastres (Yt) que ocurren en la mina. Se cuenta con N???112
# observaciones durante los años 1851 a 1962. Se proponen tres modelos: 
#   a) Modelo con tasa variable en función del tiempo:
#   ??? ??? Yt ???t ??? Po ???t
# log??? ??? t ???t ??? ???0 ??? ???1
# con N???0,0.001??? ???0 ??? y N???0,0.001??? ???1 ???
# b) Modelo con tasa constante en dos períodos: Se cree que la tasa
# promedio de desastres es constante, pero que en el siglo XX la tasa ha
# disminuido. Esto se traduce en el siguiente modelo:
#   ??? ??? Yt ???t ??? Po ???t
# log?????? ??? ??? ??? ??? ??? I???t ??? ?????? t 0 1
# con N???0,0.001??? ???0 ??? , N???0,0.001??? ???1 ??? y ??? ??? U???1,???,N???. . 
# Queremos ver cual será el mejor modelo

#--------------------------Ejercicio 6------------------------------

library(rjags)
library(R2jags)
library(ggplot2)
desastres<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/desastres.txt",header=TRUE)
n<-nrow(desastres)
min_anio<-min(desastres$Anho-1)
max_anio<-max(desastres$Anho+1)
xf<-c(1988) #tiempo hipotetico a predecir

#-Data visualization
p <- ggplot(data=desastres, aes(x=Anho, y=No.Desastres))+ geom_line(colour='brown') 
p + ggtitle("Numero de desastres de 1851 a 1963")

#-Defining data-
data<-list("n"=n,"y"=desastres$No.Desastres,"x"=desastres$Anho,"xf"=xf) 

#-Defining inits-
inits<-function(){list(beta0=0, beta1=0, y_hat=rep(1,n), yf=1)}

#-Selecting parameters to monitor-
parameters<-c("beta0", "beta1", "y_hat", "yf")

#-Defining model-
poi_model <- function(){
  #Likelihood
  for (i in 1:n) {
    y[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta0 + beta1*x[i]       #Liga log
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
  log(lambdaf) <- beta0 + beta1*xf
}

poi2_model <- function(){
  #Likelihood
  for (i in 1:n) {
    y[i] ~ dpois(lambda[i])
    logit(lambda[i])<-beta0+beta1*step(x[i]-tau)       #Liga log
  }
  #Priors 
  beta0 ~ dnorm(0,0.001)
  beta1 ~ dnorm(0,0.001)
  aux ~ dcat(a[])
  tau <- aux + 1850
  for (j in 1:112) { a[j]<- 1/112}
  #Estimacion y_hat
  for (i in 1:n) { 
    y_hat[i] ~ dpois(lambda[i]) 
  }
  #Prediction
  yf ~ dpois(lambdaf) 
  log(lambdaf) <- beta0 + beta1*xf
}

binneg_model <- function(){
  #Likelihood
  for (i in 1:n) {
    y[i] ~ dnegbin(p[i],r)
    mu[i]<-r*(1-p[i])/p[i]
    logit(p[i]) <- beta0 + beta1*x[i]       #Liga logit
  }
  #Priors 
  beta0 ~ dnorm(0,0.001)
  beta1 ~ dnorm(0,0.001)
  aux ~ dpois(5)
  r <- aux + 1
  #Estimacion y_hat
  for (i in 1:n) { 
    y_hat[i] ~ dnegbin(p[i],r) 
  }
  #Prediction
  yf ~ dpois(pf) 
  log(pf) <- beta0 + beta1*xf
}


#-Running jags-
fit<-jags(data,
          inits,
          parameters,
          model.file=
            #poi_model,
            #poi2_model,
            binneg_model,
          n.chains=1,
          n.iter=10000,
          #n.burnin = 0,
          n.burnin=4000,
          DIC = TRUE,
          jags.seed = 159549) 


#--------------------Monitoring chain----------------------#
#Cadena
out<-fit$BUGSoutput$sims.list

#z<-out$beta[,2]
b0<-out$beta0
b1<-out$beta1
DIC<-out$deviance
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
#al parecer aqui las predicciones no tienen sentido

#--------------------Comentarios del modelo----------------------#

#----------opcion poisson-log    DIC=344.8052
beta0<- out.sum[1,1]
beta1<- out.sum[2,1]
lambda <- exp(beta0+beta1*xf)
round(lambda,0) 

DIC <- out.sum[3,1]

#----------opcion poisson-logit  (modelo tasa constante 2 periodos)   
                #DIC=349.27497707
beta0<- out.sum[1,1]
beta1<- out.sum[2,1]
lambda <- exp(beta0+beta1*xf)/(1+exp(beta0+beta1*xf))
round(lambda,0) 
#1 es la prediccion para xf
DIC <- out.sum[3,1]

#----------opcion binneg-logit   DIC=348.4394
beta0<- out.sum[1,1]
beta1<- out.sum[2,1]
lambda <- exp(beta0+beta1*xf)/(1+exp(beta0+beta1*xf))
round(lambda,0) 
#1 es la prediccion para xf
DIC <- out.sum[3,1]


#notar que es una acumulacion exponencial 

#ir comparando las DIC y las probabilidades y compararlas en un cuadro

#reportar la comparacion de las variables predichas con su equivalente original

out.yf<-out.sum[grep("y_hat",rownames(out.sum)),]
or<-order(desastres$Anho)
par(mfrow=c(2,2))

#ajuste del modelo
tabla<-data.frame(desastres$Anho,desastres$No.Desastres,desastres$Anho[or],out.yf[or,1],out.yf[or,3],out.yf[or,7])
colnames(tabla)<- c("Año", "Desastres", "Order", "Media", "lim_inf","lim_sup")
p1 <-ggplot(data=tabla, aes(x=Año,y=Desastres)) + geom_point(size=3) 
p1 + geom_line(data=tabla, aes(x=Order, y=Media),colour='#CC6633',size=2) +
  geom_line(data=tabla, aes(x=Order, y=lim_inf),colour='#CC6633',linetype="F1") +
  geom_line(data=tabla, aes(x=Order, y=lim_sup),colour='#CC6633',linetype="F1") +
  ggtitle("Prediccion del numero de desastres")


#estimaciones vs reales
tab_est<-data.frame(desastres$No.Desastres,out.yf[,1])
colnames(tab_est)<- c("Reales", "Estimados")
p2 <-ggplot(tab_est, aes(Reales,Estimados)) + geom_point(size=3)
p2 + geom_abline(intercept=0,slope=1,colour='#CC6633') +
  ggtitle("Comparativo de ajuste del modelo")



