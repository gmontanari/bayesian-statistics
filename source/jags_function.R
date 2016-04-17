#funcion jags
jags(data, #vector or list
     inits, #a list with n.chains elements; each element of the list is itself a list of starting values for the BUGS model
     parameters.to.save, #character vector of the names of the parameters to save which should be monitored
     model.file="model.bug", #txt with the model
     n.chains=1, #we use 1, default 3
     n.iter=2000, #we use, 10000
     n.burnin=floor(n.iter/2), #we use 10 % or what appears
     n.thin=max(1, floor((n.iter - n.burnin) / 1000)), #we dont use
     DIC=TRUE, #we always use true
     working.directory=NULL, #where model.txt belongs
     jags.seed = 123, #for personal seed 159549
     refresh = n.iter/50, #we dont use
     progress.bar = "text", #we dont use
     digits=5,
     RNGname = c("Wichmann-Hill", "Marsaglia-Multicarry",
                 "Super-Duper", "Mersenne-Twister"), #has always a default we dont specificate
     jags.module = c("glm","dic") #null if u dont want to load
)
