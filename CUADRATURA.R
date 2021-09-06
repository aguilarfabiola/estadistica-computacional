# LIBRERIA

library(gaussquad)

# POLINOMIO DE LAGUERRE

laguerre <- laguerre.quadrature.rules(n = 100)
x <- laguerre[[100]]$x
w <- laguerre[[100]]$w

# PRIMER MOMENTO

m1 <- function(x){
  exp(x)*((((1.5*x)^3)*exp(-1.5*x))/2)
}

# SEGUNDO MOMENTO

m2 <- function(x){
  exp(x)*((x*((1.5*x)^3)*exp(-1.5*x))/2)
}

# VECTORES

f_x1 <- c()
f_x2 <- c()

# INCREMENTO

i <- 1

# VALORES

while(i <= 100){
  f_x1 <- c(f_x1,m1(x[i]))
  f_x2 <- c(f_x2,m2(x[i]))
  i <- i + 1
}

# VARIANZA APROXIMADA POR CUADRATURA

var_aprox <- sum(w*f_x2)-(sum(w*f_x1))^2
var_aprox

# VARIANZA EXACTA 

var_exacta <- 3/((1.5)^2)
var_exacta

# DIFERENCIA DE VARIANZAS

var_exacta - var_aprox








