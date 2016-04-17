
# Producci??n de leche (Congdon, 2001). Se tienen los datos
# anuales de producci??n de leche (Yt) en lbs???109, y n??mero de vacas (xt) en unidades???106, en el per??odo de 1970 a 1982.
# El modelo sugerido para estos datos es: para t???1,2,...,13
##es un ejemplo de espacio de estados
# Observaci??n: Yt ??????txt ??????t, ???t ???N???0,V???1??? ??? Yt ???N??????txt,V???1??? Evoluci??n: ???t ??????t???1 ??????t, ???t ???N???0,W???1?????? ???t ???N??????t???1,W???1???
# Adem??s sugieren una varianza constante V???1 y W???0.05

#de los modelos que hay, tenemos que hacer otras propuestas
#Y tiene cara de continua no negativa
#podemos probar con 

#--------------------------Ejercicio 7------------------------------

library(rjags)
library(R2jags)
library(ggplot2)
milk<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/milk.txt",header=TRUE)
milk$t<-1970:1982
n<-nrow(milk)
m<-2
or<-order(milk$x)
plot(milk$x[or],milk$y[or],type="l")
text(milk$x[or],milk$y[or],labels=milk$t[or],cex=0.5,col=2)
plot(milk$t,milk$y,type="l")
plot(milk$t,milk$x,type="l")

#-Data visualization
#p <- ggplot(data=desastres, aes(x=Anho, y=No.Desastres))+ geom_line(colour='brown') 
#p + ggtitle("Numero de desastres de 1851 a 1963")

#-Defining data-
#data<-list("n"=n,"m"=m,"y"=milk$y,"x"=milk$x/max(milk$x),"t"=milk$t/max(milk$t))
#data<-list("n"=n,"m"=m,"y"=c(milk$y[1:(n-2)],NA,NA),"x"=milk$x/max(milk$x),"t"=milk$t/max(milk$t))
data<-list("n"=n, "y"=c(milk$y[1:(n-2)],NA,NA),"x"=milk$x/max(milk$x))
#tecnica para predecir ya que el modelo los acompleta (kinging) interpolaci??n
#pero ya no se pueden comparar los DIC

#-Defining inits-
#inits<-function(){list(beta=rep(0,5),tau=1,yf1=rep(1,n))}
#inits<-function(){list(beta=rep(0,n+m),tau.y=1,tau.b=1,yf1=rep(0,n+m))}
#inits<-function(){list(beta=rep(0,n+m),tau.y=1,yf1=rep(0,n+m))}
#inits<-function(){list(beta=rep(0,n+m),tau.y=1,tau.b=1,yf1=rep(0,n+m),g=0)}
inits<-function(){list(beta=rep(0,n),tau.y=1,tau.b=1,yf1=rep(0,n))}

#-Selecting parameters to monitor-
#parameters<-c("beta","tau","yf1")
#parameters<-c("beta","tau.y","tau.b","yf1","g")
parameters<-c("beta","tau.y","tau.b","yf1")

#-Defining model-
#(a) Modelo est??tico (x[t])
estatico <- function(){
  #Likelihood
  for (i in 1:n) {
    y[i] ~ dnorm(mu[i],tau)
    mu[i]<-beta[1]+beta[2]*x[i]+beta[3]*pow(x[i],2)+beta[4]*t[i]+beta[5]*pow(t[i],2)
  }
  #Priors 
  for (j in 1:5) { beta[j] ~ dnorm(0,0.001) }
  tau ~ dgamma(0.001,0.001)
  
  #Prediction 1
  for (i in 1:n) { yf1[i] ~ dnorm(mu[i],tau) }
}

estatico2 <- function(){
  #Likelihood
  #Space eq.
  for (i in 1:n) {
    y[i] ~ dnorm(mu[i],tau.y)
    mu[i]<-beta[i]
  }
  #State eq.
  for (i in 2:n) {
    beta[i] ~ dnorm(mu.b[i],tau.b)
    mu.b[i] <- g*beta[i-1]
  }
  #Priors 
  beta[1] ~ dnorm(0,0.001)
  #tau.b<- lam*tau.y
  #lam<-0.5
  tau.y ~ dgamma(0.001,0.001)
  tau.b ~ dgamma(0.001,0.001)
  g ~ dnorm(0,0.001)
  
  #Prediction 1
  for (i in 1:n) { yf1[i] ~ dnorm(mu[i],tau.y) }
  
  #Prediction 2
  for (i in (n+1):(n+m)) { 
    yf1[i] ~ dnorm(mu[i],tau.y)
    mu[i] <- beta[i]
    beta[i] ~ dnorm(mu.b[i],tau.b)
    mu.b[i] <- g*beta[i-1]
  }
}

estatico3 <- function(){ #
  #Likelihood
  #Space eq.
  for (i in 1:n) {
    y[i] ~ dnorm(mu[i],tau.y)
    mu[i]<-beta[i]*x[i]
  }
  #State eq.
  for (i in 2:n) {
    beta[i] ~ dnorm(mu.b[i],tau.b)
  }
  #Priors 
  beta[1] ~ dnorm(0,0.001)
  tau.y ~ dgamma(0.001,0.001)
  tau.b ~ dgamma(0.001,0.001)
  #probar la "vaguidad de las distribuciones" quitandole ceros
  #Prediction 1
  for (i in 1:n) { yf1[i] ~ dnorm(mu[i],tau.y) }
}

#uno de los usos del modelo dinamico es para filtrar

#-Running jags-
fit<-jags(data,
          inits,
          parameters,
          model.file=
            estatico3,
            #estatico2,
            #estatico,
          n.chains=1,
          n.iter=50000,
          #n.burnin = 0,
          n.burnin=5000,
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
out<-fit$BUGSoutput$summary
head(out)

#Probabilidades
z<-out$beta[,1]
prob(z)

#DIC
out.dic<-fit$DIC
out.dic<-fit$BUGSoutput$DIC
print(out.dic)
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
colnames(tabla)<- c("A?o", "Desastres", "Order", "Media", "lim_inf","lim_sup")
p1 <-ggplot(data=tabla, aes(x=A?o,y=Desastres)) + geom_point(size=3) 
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


#######estas son las graficas

#Predictions
out.yf<-out[grep("yf1",rownames(out)),]
ymin<-min(milk$y,out.yf[,c(1,3,7)])
ymax<-max(milk$y,out.yf[,c(1,3,7)])
xmin<-min(milk$t)
xmax<-max(milk$t+m)

#x vs. y
par(mfrow=c(1,1))
plot(milk$x,milk$y,type="p",col="grey80")
points(milk$x,out.yf[,1],col=2)

#t vs y
par(mfrow=c(1,1))
plot(milk$t,milk$y,type="b",col="grey80",ylim=c(ymin,ymax),xlim=c(xmin,xmax))
lines(milk$t,out.yf[1:n,1],col=2)
lines(milk$t,out.yf[1:n,3],col=2,lty=2)
lines(milk$t,out.yf[1:n,7],col=2,lty=2)
lines(milk$t[n]:(milk$t[n]+m),out.yf[n:(n+m),1],col=4)
lines(milk$t[n]:(milk$t[n]+m),out.yf[n:(n+m),3],col=4,lty=2)
lines(milk$t[n]:(milk$t[n]+m),out.yf[n:(n+m),7],col=4,lty=2)

####################
######################################################################
#PROBAR CON ESTOS MODELOS

#---------------------------------------------------------------------
#(a) Modelo est??tico (x[t])

model  {
  tau.y  <- 1
  for (t in 1:13){y[t] ~ dnorm(mu[t],tau.y);
    mu[t] <-  alpha + x[t]*beta;}
  # forecast
  for (t in 1:13){  mu.new[t] <- alpha + x[t]*beta;
  y.new[t] ~ dnorm(mu.new[t],tau.y) }
  # initials
  alpha ~ dnorm(10,0.001);
  beta ~ dnorm(10,0.001) 
}


list(
  y=c(117., 118.6, 120., 115.5, 115.6, 115.4, 120.2, 122.7, 121.5, 123.4, 128.5, 
      130., 135.8), 
  x=c(12., 11.8, 11.7, 11.4, 11.2, 11.1, 11., 11., 10.8, 10.7, 10.8, 10.9, 11.)) 


list(alpha=0, beta=10)

#---------------------------------------------------------------------
#(b) Modelo est??tico (Tiempo)

model  {
  tau.y  <- 1
  for (t in 1:13){y[t] ~ dnorm(mu[t],tau.y);
    mu[t] <-  alpha + tiempo[t]*beta;}
  # forecast
  for (t in 1:13){  mu.new[t] <- alpha + tiempo[t]*beta;
  y.new[t] ~ dnorm(mu.new[t],tau.y) }
  # initials
  alpha ~ dnorm(0,0.001);
  beta ~ dnorm(0,0.001) 
}


list(
  y=c(117., 118.6, 120., 115.5, 115.6, 115.4, 120.2, 122.7, 121.5, 123.4, 128.5, 
      130., 135.8), 
  tiempo=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)) 


list(alpha=0, beta=0)

#---------------------------------------------------------------------
#(c) Modelo est??tico (Tiempo + x[t])

model  {
  tau.y  <- 1
  for (t in 1:13){y[t] ~ dnorm(mu[t],tau.y);
    mu[t] <-  alpha + beta[1]*tiempo[t] + beta[2]*x[t];}
  # forecast
  for (t in 1:13){  mu.new[t] <- alpha + beta[1]*tiempo[t] + beta[2]*x[t];
  y.new[t] ~ dnorm(mu.new[t],tau.y) }
  # initials
  alpha ~ dnorm(0,0.001);
  beta[1] ~ dnorm(0,0.001);
  beta[2] ~ dnorm(0,0.001)
}


list(
  y=c(117., 118.6, 120., 115.5, 115.6, 115.4, 120.2, 122.7, 121.5, 123.4, 128.5, 
      130., 135.8), 
  tiempo=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
  x=c(12., 11.8, 11.7, 11.4, 11.2, 11.1, 11., 11., 10.8, 10.7, 10.8, 10.9, 11.)) 


list(alpha=0, beta=c(0,0))

#---------------------------------------------------------------------
#(d) Modelo din??mico (con variable explicativa)

model  {# fixed precisions, as in Harrison and West, Ch 3
  W <- 0.05; W.inv <- 1/W
  tau.y  <- 1
  #  W.inv ~ dgamma(0.01,0.01);
  #  tau.y ~ dgamma(0.01,0.01);
  
  # observation model 
  for (t in 1:13){y[t] ~ dnorm(mu[t],tau.y);
    mu[t] <-  x[t]*beta[t];}
  # state model
  for (t in 2:13){ beta[t]  ~ dnorm(beta[t-1],W.inv);}
  # one-step ahead forecasts
  for (t in 2:13){  beta.new[t] ~ dnorm(beta[t-1],W.inv);
    mu.new[t] <- beta.new[t]*x[t];
    y.new[t] ~ dnorm(mu.new[t],tau.y) }
  # settings for year 1
  beta[1] ~ dnorm(10,0.001) 
}


list(
  y=c(117., 118.6, 120., 115.5, 115.6, 115.4, 120.2, 122.7, 121.5, 123.4, 128.5, 
      130., 135.8), 
  x=c(12., 11.8, 11.7, 11.4, 11.2, 11.1, 11., 11., 10.8, 10.7, 10.8, 10.9, 11.)) 


list(beta=c(10,10,10,10,10,10,10,10,10,10,10,10,10))

#---------------------------------------------------------------------
#(e) Modelo din??mico (sin variables explicativas)

model  {# fixed precisions, as in Harrison and West, Ch 3
  #   W <- 10; W.inv <- 1/W
  #   tau.y  <- 0.01 
  W.inv ~ dgamma(0.01,0.01);
  tau.y ~ dgamma(0.01,0.01);
  
  # observation model 
  for (t in 1:13){y[t] ~ dnorm(mu[t],tau.y);
    mu[t] <- beta[t]}
  # state model
  for (t in 2:13){ beta[t] ~ dnorm(beta[t-1],W.inv);}
  # one-step ahead forecasts
  for (t in 2:13){  beta.new[t] ~ dnorm(beta[t-1],W.inv);
    mu.new[t] <- beta.new[t];
    y.new[t] ~ dnorm(mu.new[t],tau.y);}
  # settings for year 1
  beta[1] ~ dnorm(0,0.001) 
}


list(
  y=c(117., 118.6, 120., 115.5, 115.6, 115.4, 120.2, 122.7, 121.5, 123.4, 128.5, 
      130., 135.8))

list(beta=c(0,0,0,0,0,0,0,0,0,0,0,0,0))

