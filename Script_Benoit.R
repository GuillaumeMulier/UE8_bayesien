library(R2jags)
library(lattice)
# Données
x = c(2, 14, 2, 9, 15, 7, 3, 14, 5, 2) 
y = c(3, 50, 7, 20, 44, 3, 1, 58, 8, 7)
data <- data.frame(x, y)

summary(data)
hist(data$y)
xyplot(data$y~data$x, main = "Nombre de panne en fonction age de la machine")

dataD = as.list(data)

# Modèle 1 ------------------------------------------------

bayes.mod <- function(){
  for (t in 1:length(y)) {
    y[t]~dpois(exp(a))
  }
  a~dnorm(0,1.0E-3) 
  
}

# paramètres
bayes.mod.params <- c("a")

# Inits
inits1<- list("a"=3)

bayes.mod.inits<-list(inits1)

#nbre d'iterations

nb = 1000 # burn-in
m = 30000 #nbre total d'iterations
nt = 8 #thin

bayes.mod.fit <-jags(
  data = dataD,
  inits = bayes.mod.inits,
  parameters.to.save = bayes.mod.params,
  n.chains = 1,
  n.iter = m,
  n.burnin = nb,
  n.thin =nt,
  model.file = bayes.mod)

traceplot(bayes.mod.fit, varname = "a", ask = F)
bayes.mod.fit.mcmc = as.mcmc(bayes.mod.fit)
summary(bayes.mod.fit.mcmc)

autocorr.plot(bayes.mod.fit.mcmc)
effectiveSize(bayes.mod.fit.mcmc)
print(bayes.mod.fit)


# graph du mod poisson
res = summary(bayes.mod.fit.mcmc)
resmoy = as.data.frame(res[[1]])
resq = as.data.frame(res[[2]])

densityplot(bayes.mod.fit.mcmc[,1:2], layout=c(2,1), aspect = "fill")
t=1:10

lambda = exp(resmoy$Mean[1])

aaq2.5 = resq$'2.5%'[1]
lambdainf = exp(aaq2.5)

aaq97.5 = resq$'97.5%'[1]
thetasup = exp(aaq97.5)

xyplot(data$y~data$x, main = "Nombre de panne en fonction age de la machine")
lines(data$y, lambda)
lines(tableau$age, thetainf, col='red')
lines(tableau$age, thetasup, col='red')


#### Modèle 2 ----------------------------------------------------------------------------

bayes.mod2 <- function(){
  for (t in 1:length(y)) {
    y[t]~dpois(lam[t])
    log(lam[t]) = a+b_x*x[t]
  }
  a ~ dnorm(0,1.0E-3) 
  b_x ~ dnorm(0,1.0E-3) 
}

# paramètres
bayes.mod2.params <- c("a","b_x")

# Inits
inits2<- list("a"=3, "b_x"= 1)

bayes.mod.inits2<-list(inits2)

#nbre d'iterations

nb = 1000 # burn-in
m = 30000 #nbre total d'iterations
nt = 10 #thin

bayes.mod.fit2 <-jags(
  data = dataD,
  inits = bayes.mod.inits2,
  parameters.to.save = bayes.mod2.params,
  n.chains = 1,
  n.iter = m,
  n.burnin = nb,
  n.thin =nt,
  model.file = bayes.mod2)

traceplot(bayes.mod.fit2, varname = "a", ask = F)
traceplot(bayes.mod.fit2, varname = "b", ask = F)
bayes.mod.fit.mcmc2 = as.mcmc(bayes.mod.fit2)
summary(bayes.mod.fit.mcmc2)

autocorr.plot(bayes.mod.fit.mcmc2)
effectiveSize(bayes.mod.fit.mcmc2)
print(bayes.mod.fit2)

# graph du mod poisson
res = summary(bayes.mod.fit.mcmc)
resmoy = as.data.frame(res[[1]])
resq = as.data.frame(res[[2]])

densityplot(bayes.mod.fit.mcmc[,1:2], layout=c(2,1), aspect = "fill")
t=1:10

lambda = exp(resmoy$Mean[1])

aaq2.5 = resq$'2.5%'[1]
lambdainf = exp(aaq2.5)

aaq97.5 = resq$'97.5%'[1]
thetasup = exp(aaq97.5)

xyplot(data$y~data$x, main = "Nombre de panne en fonction age de la machine")
lines(data$y, lambda)
lines(tableau$age, thetainf, col='red')
lines(tableau$age, thetasup, col='red')


## Modèles 3 -----------------------------------------------
bayes.mod3 <- function(){
  for (t in 1:length(y)) {
    y[t]~dpois(lam[t])
    log(lam[t]) = a+b_x*x[t] + Eps
  }
  a ~ dnorm(0,1.0E-3) 
  b_x ~ dnorm(0,1.0E-3) 
  Eps ~ dnorm(0,sigma2)
  sigma2 ~ dgamma(0.01,0.01) 
}

# paramètres
bayes.mod3.params <- c("a","b_x","Eps", "sigma2")

# Inits
inits3<- list("a"=3, "b_x"= 1, "Eps" = 1, "sigma2" = 1)

bayes.mod.inits3<-list(inits3)

#nbre d'iterations

nb = 1000 # burn-in
m = 300000 #nbre total d'iterations
nt = 10 #thin

bayes.mod.fit3 <-jags(
  data = dataD,
  inits = bayes.mod.inits3,
  parameters.to.save = bayes.mod3.params,
  n.chains = 1,
  n.iter = m,
  n.burnin = nb,
  n.thin =nt,
  model.file = bayes.mod3)

traceplot(bayes.mod.fit3, varname = "a", ask = F)
traceplot(bayes.mod.fit3, varname = "b_x", ask = F)
traceplot(bayes.mod.fit3, varname = "Eps", ask = F)
traceplot(bayes.mod.fit3, varname = "sigma2", ask = F)

bayes.mod.fit.mcmc3 = as.mcmc(bayes.mod.fit3)
summary(bayes.mod.fit.mcmc3)

autocorr.plot(bayes.mod.fit.mcmc3)
effectiveSize(bayes.mod.fit.mcmc3)
print(bayes.mod.fit3)
