#Opgave 4
set.seed(11)
U <-runif(10000)

?uniroot
beta <- 0.1
lambda <- 0.01
eta <- 0.05
x0 <- 20
f <- function (x) lambda*x+eta*(exp(beta*x)-1)-(lambda*x0+eta*(exp(beta*x0)-1))+log(1-U[1])
uniroot(f, c(20,100))


Alder <- c()
for (i in 1:10000) {
  f <- function (x) lambda*x+eta*(exp(beta*x)-1)-(lambda*x0+eta*(exp(beta*x0)-1))+log(1-U[i])
  uniroot(f, c(20,100))
  Alder[i] <- uniroot(f, c(20,100))$root
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
beta <- 0.1
lambda <- 0.01
eta <- 0.05
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

MLE <- function(x) {
  nlx <- function(par) {
    b <- par[1]
    e <- par[2]
    l <- par[3]
    loglik <- log(l+e*b*exp(b*x))-l*(x-x0)-e*(exp(b*x)-exp(b*x0))
    -sum(loglik)
  }
  optim(c(0.11,0.051,0.011), nlx,method ="L-BFGS-B",lower=c(exp(-8),exp(-8),exp(-8)))
}

MLE(Værdi)
Alder

?optim






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


