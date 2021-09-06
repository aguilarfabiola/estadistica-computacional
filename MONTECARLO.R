# FUNCIÓN DE DENSIDAD DE DISTRIBUCIÓN ERLANG

erlang <- function(x){
  (1.5*((1.5*x)^2)*exp(-1.5*x))/2
}

# GENERACIÓN DE NÚMEROS PSEUDOALEATORIOS

u  <- runif(100000000,0,1)

# CALCULAR PRIMER MOMENTO

numbs <- c()
m1 <- function(u){
      h      <- -((u^(-1))-1)*erlang((u^(-1))-1)*(-u^-2)
      numbs <- c(numbs, h)
}

primer <- sum(m1(u))/length(u)

# CALCULAR SEGUNDO MOMENTO

numbs1 <- c()
m2 <- function(u){
      h1     <- -(((u^(-1))-1)^(2))*erlang((u^(-1))-1)*(-u^-2)
      numbs1 <- c(numbs1, h1)
}

segundo <- sum(m2(u))/length(u)

# VARIANZA APROXIMADA

var_aprox  <- segundo - (primer)^2
var_aprox

# VARIANZA EXACTA 

var_exacta <- 3/((1.5)^2)
var_exacta

# DIFERENCIA DE VARIANZAS

var_exacta - var_aprox

