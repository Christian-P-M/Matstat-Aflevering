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
Værdi


#Tæthed herunder
ff <- function(x) (lambda+beta*eta*exp(beta*x))*exp(lambda*x0+eta*(exp(beta*x0)-1)-(lambda*x+eta*(exp(beta*x)-1)))

hist(Alder,breaks=20,probability=TRUE) #Histogram
curve(ff,lwd=2,add=TRUE) #Plot af tætheden


x


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







