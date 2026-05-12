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
beta <- 0.07
eta <- 0.01
lambda <- 0.003
x0 <- 20

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
  
  start <- log(c(0.07, 0.008, 0.003))
  optim(start, nlx, method="BFGS")
}

res <- MLE(Alder, x0)
exp(res$par)



#Plot
loglik_fun <- function(beta, eta, lambda, x, x0) {
  log(lambda + eta * beta * exp(beta * x)) -
    lambda * (x - x0) -
    eta * (exp(beta * x) - exp(beta * x0))
}
beta_seq <- seq(0.05, 0.15, length.out = 50)
eta_seq  <- seq(0.02, 0.08, length.out = 50)

LL <- matrix(0, length(beta_seq), length(eta_seq))

for (i in 1:length(beta_seq)) {
  for (j in 1:length(eta_seq)) {
    LL[i, j] <- sum(
      loglik_fun(beta_seq[i], eta_seq[j], lambda = 0.01, x = Alder, x0 = x0)
    )
  }
}
contour(beta_seq, eta_seq, LL,
        xlab = "beta", ylab = "eta",
        main = "Log-likelihood overflade")
persp(beta_seq, eta_seq, LL,
      theta = 40, phi = 30,
      xlab = "beta", ylab = "eta", zlab = "log-likelihood",
      main = "3D log-likelihood overflade")





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


