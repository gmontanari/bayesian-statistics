
#queremos modelar el numero de 
# Y_i animales muertos
# log(x_i) cantidad de droga
# ne_i numero de expuestos

#--------------------------Examen------------------------------

library(rjags)
library(R2jags)
library(ggplot2)
bioassay<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/Bioassay.txt",header=TRUE)
n<-nrow(bioassay)

table <- bioassay
table['Dose']<-exp(bioassay[,1])
colnames(table)<- c("LogDosis", "Expuestos", "Muertes","Dosis")
p <- ggplot(data=table, aes(x=Dosis, y=Muertes)) +geom_line(color='red') 
#p + geom_point(data=table, aes(y=Expuestos), colour='brown')

#-Defining data-
data<-list("n"=n,"ne"=bioassay$No.Animales,"y"=bioassay$No.Muertes,"x"=bioassay$LogDose)

#-Defining inits-
inits<-function(){list(beta0=0, beta1=0, y_hat=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("beta0", "beta1", "y_hat")

#-Defining model-
bin_model1 <- function(){
  #Likelihood
  for (i in 1:n) {
    y[i] ~ dbin(p[i],ne[i])
    logit(p[i]) <- beta0+beta1*x[i]
  }
  #Priors vagas
  beta0 ~ dnorm(0,0.001)
  beta1 ~ dnorm(0,0.001)
  #Estimacion y_hat
  for (i in 1:n) {
    y_hat[i] ~ dbin(p[i],ne[i]) 
  }
}

bin_model2 <- function(){
  #Likelihood
  for (i in 1:n) {
    y[i] ~ dbin(p[i],ne[i])
    logit(p[i]) <- beta0+beta1*x[i]
  }
  #Priors informativas (conocimiento previo)
  beta0 ~ dnorm(-17.31,0.0009490187)
  beta1 ~ dnorm(2.57,0.04302926)
  #Estimacion y_hat
  for (i in 1:n) {
    y_hat[i] ~ dbin(p[i],ne[i]) 
  }
}

probit_model1 <- function(){
  #Likelihood
  for (i in 1:n) {
    y[i] ~ dbin(p[i],ne[i])
    p[i] <- phi(beta0+beta1*x[i])
  }
  #Priors vagas
  beta0 ~ dnorm(0,0.001)
  beta1 ~ dnorm(0,0.001)
  #Estimacion y_hat
  for (i in 1:n) {
    y_hat[i] ~ dbin(p[i],ne[i]) 
  }
}

probit_model2 <- function(){
  #Likelihood
  for (i in 1:n) {
    y[i] ~ dbin(p[i],ne[i])
    p[i] <- phi(beta0+beta1*x[i])
  }
  #Priors informativas (conocimiento previo)
  beta0 ~ dnorm(-17.31,0.0009490187)
  beta1 ~ dnorm(2.57,0.04302926)
  #Estimacion y_hat
  for (i in 1:n) {
    y_hat[i] ~ dbin(p[i],ne[i]) 
  }
}

#-Running code-
fit<-jags(data,
          inits,
          parameters,
          model.file=
            #bin_model1,
            #bin_model2,
            #probit_model1,
            probit_model2,
          n.chains=1,
          n.iter=50000,
          #n.burnin = 0,
          n.burnin=5000,
          jags.seed = 159549) 


#--------------------Monitoring chain----------------------#
#Cadena
out<-fit$BUGSoutput$sims.list

b0<-out$beta0
b1<-out$beta1
dev<-out$deviance
par(mfrow=c(3,3)) 
plot(b0,type="l",col='brown')
plot(cumsum(b0)/(1:length(b0)),type="l", ylab = 'Estabilizacion')
acf(b0)
plot(b1,type="l",col='brown')
plot(cumsum(b1)/(1:length(b1)),type="l", ylab = 'Estabilizacion')
acf(b1)
plot(dev,type="l",col='brown')
plot(cumsum(dev)/(1:length(dev)),type="l", ylab = 'Estabilizacion') 
acf(dev)

#Resumen (estimadores)
out.sum<-fit$BUGSoutput$summary
out.sum

#--------------------Comentarios del modelo----------------------#

out.beta<-out.sum[grep("beta",rownames(out.sum)),]
out.y_hat<-out.sum[grep("y_hat",rownames(out.sum)),]
out.DIC<-fit$BUGSoutput$DIC 
out.beta
out.y_hat[,1:2]
out.DIC
beta0<- out.beta[1,1]
beta1<- out.beta[2,1]

#---------opcion binomial-logit-vaga    DIC=7.29722
#como usamos logit
#p=exp(alpha + beta*LD50)/(1+exp(alpha + beta*LD50))
#liga_p<-log(0.5/(1-0.5))
#como es cero entonces
#eta=beta0+beta1*LD50=0
#de aqui que 
LD50 <- -beta0/beta1
LD50 # es la dosis a aplicar
# -0.1066526
exp(-0.1066526)
0.8988379

#---------opcion binomial-logit-informativa   DIC=7.057167
#como usamos logit
#p=exp(alpha + beta*LD50)/(1+exp(alpha + beta*LD50))
#liga_p<-log(0.5/(1-0.5))
#como es cero entonces
#eta=beta0+beta1*LD50=0
#de aqui que 
LD50 <- -beta0/beta1
LD50 # es la dosis a aplicar
# -0.09988585

#---------opcion binomial-probit-vaga  DIC=6.938515
LD50 <- -beta0/beta1
LD50 # es la dosis a aplicar
# -0.1208439

#---------opcion binomial-probit-informativa  DIC=6.922653
LD50 <- -beta0/beta1
LD50 # es la dosis a aplicar
# -0.1122966




#--------------------Graficas Finales----------------------#

out.yf<-out.sum[grep("y_hat",rownames(out.sum)),]
or<-order(bioassay$LogDose)

#ajuste del modelo
tabla<-data.frame(bioassay$LogDose,bioassay$No.Muertes,bioassay$LogDose[or],out.yf[or,1],out.yf[or,3],out.yf[or,7])
colnames(tabla)<- c("LogDosis", "Muertes", "Order", "Media", "lim_inf","lim_sup")
p1 <-ggplot(data=tabla, aes(x=LogDosis,y=Muertes)) + geom_bar(fill='red',stat="identity") 
p1 + geom_errorbar(aes(ymin=Muertes-lim_inf, ymax=Muertes+lim_sup),width=.1)

p1 <-ggplot(data=tabla, aes(x=LogDosis,y=Muertes)) + geom_point(size=3) 
p1 + geom_line(data=tabla, aes(x=Order, y=Media),colour='#CC6633',size=1) +
  geom_line(data=tabla, aes(x=Order, y=lim_inf),colour='#CC6633',linetype="dashed") +
  geom_line(data=tabla, aes(x=Order, y=lim_sup),colour='#CC6633',linetype="dashed")
  ggtitle("Numero de muertes explicado por la LogDosis")


#estimaciones vs reales
tab_est<-data.frame(bioassay$No.Muertes,out.yf[,1])
colnames(tab_est)<- c("Reales", "Estimados")
p2 <-ggplot(tab_est, aes(Reales,Estimados)) + geom_point(size=3)
p2 + geom_abline(intercept=0,slope=1,colour='#CC6633') +
  ggtitle("Comparativo de ajuste del modelo")



