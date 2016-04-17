
##----------------M??todo de la aproximaci??n lineal asint??tica

f<-function(x){
  out<- 5-(x-1)^2
  out<- ifelse(x>3 | x>3,0,out)
  out
}

x<-seq(-2,4,100)
plot(x,f(x),type="l")
abline(h=0)
abline(v=c(-1,3))

lines(x,dnorm(x,0,1),lty=2,col=2) #caso 1
lines(x,dnorm(x,1,2/3),lty=2,col=3) #caso 2

#caso 1 s=normal estandar
mu<-0
sig<-1
N<-1000
x<-rnorm(N,mu,sig)
I1=mean(f(x)/dnorm(x,mu,sig))
I1

#caso 2 s=normal 1,2/3
mu<-1
sig<-2/3
N<-1000
x<-rnorm(N,mu,sig)
I1<-mean(f(x)/dnorm(x,mu,sig))
I1

#caso 3
lines(x,dnorm(x,1,1),lty=2,col=4)
#caso 3 s=normal 1,1
mu<-1
sig<-1
N<-1000
x<-rnorm(N,mu,sig)
I1=mean(f(x)/dnorm(x,mu,sig))
I1

#########################
#de la paqueter??a bayesm
#hay bugs ilustrativos
out<-rbiNormGibbs(rho=.80)

