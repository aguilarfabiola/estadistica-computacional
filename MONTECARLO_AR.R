#     TALLER SEMANAL 3: ESTADÍSTICA COMPUTACIONAL
#                  FABIOLA AGUILAR
####################################################

# GRAFICA 

curve(atan(x), from = 0, to = 1, ylim = c(0,0.89))

# VALOR DE C

c <- 0.85

#CALCULO DE ACIERTOS

n_a <- c()
i <- 0
while(i < 10000){
  u1 <- runif(1)
  u2 <- runif(1,0,0.85)
  id <- u2 < sqrt(atan(u1))
  n_a <- c(n_a,id)
  i <- i+1 
  
}

  
p <- (sum(n_a)/10000)

#INTERVALO DE CONFIANZA

alpha <- 0.05
z <- qnorm(0.975,0,1)

a <- (c*p) - z*(c*sqrt(p*(1-p)))/sqrt(10000)
b <- (c*p) + z*(c*sqrt(p*(1-p)))/sqrt(10000)