## Matematisk Statistik, 7/5, 2026, Niels Richard Hansen
## Code illustrating the numerical computation of estimates as
## either solutions of moment equations or as minimizers of 
## the negative log-likelihood. 

## The package nleqslv implements algorithms for solving non-linear equations

library(nleqslv)

## Simulating data from gamma model

alpha <- 3
beta <- 2
n <- 100
x <- rgamma(n, shape = alpha, scale = beta)

## Analytic solution for the moment estimator, FoMS Ex. 4.17

x_bar <- mean(x)
s_sq <- mean((x - x_bar)^2)

par_hat <- c(x_bar^2 / s_sq, s_sq / x_bar)
par_hat

## Using nleqslv() to solve the moment equation

m <- function(par) {
  alpha <- par[1]
  beta <- par[2]
  c(alpha * beta, alpha * (alpha + 1) * beta^2)
}

t_bar <- c(mean(x), mean(x^2))

par_hat_slv <- nleqslv(
  c(alpha, beta), 
  function(par) m(par) - t_bar
)

par_hat_slv$x

## Using nleqslv() with explicit jacobian 

Dm <- function(par) {
  alpha <- par[1]
  beta <- par[2]
  matrix(
    c(
      beta,                       alpha, 
      (2 * alpha + 1) * beta^2,   2 * alpha * (alpha + 1) * beta
      ),
    2, 2,
    byrow = TRUE
  )
}

par_hat_slv2 <- nleqslv(
  c(alpha, beta), 
  function(par) m(par) - t_bar, 
  Dm
)
par_hat_slv2$x

## Using optim()

g <- function(par) {
  mtheta <- m(par)
  (mtheta[1] - t_bar[1])^2 + (mtheta[2] - t_bar[2])^2
}

par_hat_opt <- optim(
  c(alpha, beta), 
  g, 
  method = "BFGS"
)
par_hat_opt$par

## Using optim() with explicit gradient 

grad_g <- function(par) {
  mtheta <- m(par)
  Dmtheta <- Dm(par) 
  2 * Dmtheta[1, ] * (mtheta[1] - t_bar[1]) + 
    2 * Dmtheta[2, ] * (mtheta[2] - t_bar[2])
}

par_hat_opt2 <- optim(c(alpha, beta), g, grad_g, method = "BFGS")
par_hat_opt2$par

## Optimizing the log-likelihood using optim(), FoMS Ex. 4.26
## Note that this implementation is in terms of the 
## canonical parameters: theta1 = alpha, theta2 = 1/beta.

tt_bar <- c(mean(log(x)), - mean(x))

psi <- function(theta)
  lgamma(theta[1]) - theta[1] * log(theta[2])

tau <- function(theta) 
  c(digamma(theta[1]) - log(theta[2]), - theta[1]/theta[2])

neg_loglik <- function(theta) 
  psi(theta) - theta[1] * tt_bar[1] - theta[2] * tt_bar[2]

# With numerical gradient

par_hat_mle_opt <- optim(
  c(alpha, 1/beta), 
  neg_loglik, 
  method = "BFGS"
)
par_hat_mle_opt$par

# With analytic gradient

par_hat_mle_opt <- optim(
  c(alpha, 1/beta), 
  neg_loglik, 
  function(theta) tau(theta) - tt_bar, 
  method = "BFGS"
)
par_hat_mle_opt$par

## Using nleqslv() to solve the score equation (with numerical Jacobian)

par_hat_mle_slv <- nleqslv(
  c(alpha, 1/beta), 
  function(theta) tau(theta) - tt_bar
)
par_hat_mle_slv$x







