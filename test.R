#Opgave 4
set.seed(20)
U <-runif(10000)

?uniroot
beta <- 0.07
lambda <- 0.003
eta <- 0.01
x0 <- 20
f <- function (x) lambda*x+eta*(exp(beta*x)-1)-(lambda*x0+eta*(exp(beta*x0)-1))+log(1-U[1])
uniroot(f, c(20,100))


Alder <- c()
for (i in 1:10000) {
  f <- function (x) lambda*x+eta*(exp(beta*x)-1)-(lambda*x0+eta*(exp(beta*x0)-1))+log(1-U[i])
  uniroot(f, c(20,200))
  Alder[i] <- uniroot(f, c(20,200))$root
}
Alder

#Tæthed herunder
ff <- function(x) (lambda+beta*eta*exp(beta*x))*exp(lambda*x0+eta*(exp(beta*x0)-1)-(lambda*x+eta*(exp(beta*x)-1)))

hist(Alder,breaks=20,probability=TRUE) #Histogram
curve(ff,lwd=2,add=TRUE) #Plot af tætheden





#2. plot
beta <- 0.07
lambda <- 0.007
eta <- 0.03
x0 <- 20
f <- function (x) lambda*x+eta*(exp(beta*x)-1)-(lambda*x0+eta*(exp(beta*x0)-1))+log(1-U[1])
uniroot(f, c(20,100))


Alder <- c()
for (i in 1:10000) {
  f <- function (x) lambda*x+eta*(exp(beta*x)-1)-(lambda*x0+eta*(exp(beta*x0)-1))+log(1-U[i])
  uniroot(f, c(20,100))
  Alder[i] <- uniroot(f, c(20,100))$root
}
Værdi


#Tæthed herunder
ff <- function(x) (lambda+beta*eta*exp(beta*x))*exp(lambda*x0+eta*(exp(beta*x0)-1)-(lambda*x+eta*(exp(beta*x)-1)))

hist(Alder,breaks=20,probability=TRUE) #Histogram
curve(ff,lwd=2,add=TRUE) #Plot af tætheden






#3. plot
beta <- 0.07
lambda <- 0.003
eta <- 0.01
x0 <- 20
f <- function (x) lambda*x+eta*(exp(beta*x)-1)-(lambda*x0+eta*(exp(beta*x0)-1))+log(1-U[1])
uniroot(f, c(20,100))


Alder <- c()
for (i in 1:10000) {
  f <- function (x) lambda*x+eta*(exp(beta*x)-1)-(lambda*x0+eta*(exp(beta*x0)-1))+log(1-U[i])
  uniroot(f, c(20,100))
  Alder[i] <- uniroot(f, c(20,100))$root
}
Værdi


#Tæthed herunder
ff <- function(x) (lambda+beta*eta*exp(beta*x))*exp(lambda*x0+eta*(exp(beta*x0)-1)-(lambda*x+eta*(exp(beta*x)-1)))

hist(Alder,breaks=20,probability=TRUE) #Histogram
curve(ff,lwd=2,add=TRUE) #Plot af tætheden




#4. plot
beta <- 0.07
lambda <- 0.003
eta <- 0.005
x0 <- 20
f <- function (x) lambda*x+eta*(exp(beta*x)-1)-(lambda*x0+eta*(exp(beta*x0)-1))+log(1-U[1])
uniroot(f, c(20,100))


Alder <- c()
for (i in 1:10000) {
  f <- function (x) lambda*x+eta*(exp(beta*x)-1)-(lambda*x0+eta*(exp(beta*x0)-1))+log(1-U[i])
  uniroot(f, c(20,100))
  Alder[i] <- uniroot(f, c(20,100))$root
}
Værdi


#Tæthed herunder
ff <- function(x) (lambda+beta*eta*exp(beta*x))*exp(lambda*x0+eta*(exp(beta*x0)-1)-(lambda*x+eta*(exp(beta*x)-1)))

hist(Alder,breaks=20,probability=TRUE) #Histogram
curve(ff,lwd=2,add=TRUE) #Plot af tætheden
?curve


?optim



#Opgave 10
beta <- 0.05
eta <- 0.02
lambda <- 0.002
x0 <- 20

MLE <- function(x) {
  nlx <- function(theta) {
    b <- exp(theta[1])
    e <- exp(theta[2])
    l <- exp(theta[3])
    
    loglik <- log(l + e*b*exp(b*x)) -
      l*(x - x0) -
      e*(exp(b*x) - exp(b*x0))
    
    -sum(loglik)
  }
  
  start <- log(c(0.05, 0.02, 0.001))
  optim(start, nlx, method="BFGS")
}

res <- MLE(Alder)
exp(res$par)






#Opgave 13
library(tidyverse)
library(infer)
library(skimr)
library(HH)
library(vcd)
library(moderndive)

data <- kohorte1910

#Her er to plots over hinanden, hvor fordelingerne kan sammenlignes
ggplot(kohorte1910, aes(x=Age,y=nx)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ Sex, ncol = 1, scales = "free_y") +
  labs(x = "Alder", y = "Døde", title = "Døde pr. år")


#Her er et samlet plot, hvor de to fordelinger sammenlignes (dette er bedst)
ggplot(kohorte1910, aes(x=Age,y=nx,fill=Sex)) +
  geom_col(position = "dodge") +
  labs(x = "Alder", y = "Døde", title = "Døde pr. år")

#De gennemsnitlige levealdre for mænd og kvinder
Mænd <- subset(kohorte1910, Sex == "Male")
weighted.mean(Mænd$Age, Mænd$nx)

Kvinder <- subset(kohorte1910, Sex == "Female")
weighted.mean(Kvinder$Age, Kvinder$nx)


#Opgave 14
#De gennemsnitlige levealdre for mænd og kvinder
Mænd <- subset(kohorte1910, Sex == "Male" & Age >= 20)
weighted.mean(Mænd$Age, Mænd$nx)

Kvinder <- subset(kohorte1910, Sex == "Female" & Age >= 20)
weighted.mean(Kvinder$Age, Kvinder$nx)

vm <- rep(Mænd$Age, Mænd$nx)
vk <- rep(Kvinder$Age, Kvinder$nx)

# Mænd c(0.07, 0.02, 0.002) --------------------------------------------------------------------
MLE <- function(x, x0) {
  nlx <- function(theta) {
    b <- exp(theta[1])
    e <- exp(theta[2])
    l <- exp(theta[3])
    
    loglik <- log(l + e*b*exp(b*x)) -
      l*(x - x0) -
      e*(exp(b*x) - exp(b*x0))
    
    -sum(loglik)
  }
  
  start <- log(c(0.07, 0.02, 0.002))
  optim(start, nlx, method="BFGS")
}
res <- MLE(vm, x0)
exp(res$par)

beta <- 0.0932572278
eta <- 0.0006048876
lambda <- 0.0012394866

#Tæthed herunder
ff <- function(x) (lambda+beta*eta*exp(beta*x))*exp(lambda*x0+eta*(exp(beta*x0)-1)-(lambda*x+eta*(exp(beta*x)-1)))

hist(vm,breaks=20,probability=TRUE) #Histogram
curve(ff,lwd=2,add=TRUE) #Plot af tætheden

# Kvinder c(0.08, 0.002, 0.003) --------------------------------------------------------------------
MLE <- function(x, x0) {
  nlx <- function(theta) {
    b <- exp(theta[1])
    e <- exp(theta[2])
    l <- exp(theta[3])
    
    loglik <- log(l + e*b*exp(b*x)) -
      l*(x - x0) -
      e*(exp(b*x) - exp(b*x0))
    
    -sum(loglik)
  }
  
  start <- log(c(0.08, 0.002, 0.003))
  optim(start, nlx, method="BFGS")
}
res <- MLE(vk, x0)
exp(res$par)

beta <- 0.1053773623
eta <- 0.0001206993
lambda <- 0.0019034375

#Tæthed herunder
ff <- function(x) (lambda+beta*eta*exp(beta*x))*exp(lambda*x0+eta*(exp(beta*x0)-1)-(lambda*x+eta*(exp(beta*x)-1)))

hist(vk,breaks=20,probability=TRUE) #Histogram
curve(ff,lwd=2,add=TRUE) #Plot af tætheden


#Opgave 15





