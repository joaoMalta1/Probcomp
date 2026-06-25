#a)
lambda <- 4
x <- 0:15 # a escolher 
# Função de probabilidade
fx <- dpois(x, lambda)
# Função acumulada
Fx <- ppois(x, lambda)

par(mfrow = c(1, 2))

# Gráfico da função de probabilidade
plot(
  x, fx,
  type = "h",
  lwd = 4,
  main = "Função de Probabilidade",
  xlab = "Pacotes perdidos por minuto (X)",
  ylab = "P(X = x)"
)
points(x, fx, pch = 19)

# Gráfico da função acumulada
plot(
  x, Fx,
  type = "s",
  lwd = 2,
  main = "Função Acumulada",
  xlab = "Pacotes perdidos por minuto (X)",
  ylab = "P(X ≤ x)"
)
points(x, Fx, pch = 19)

#b) 
lambda2 <- 8 # 4/min     == 8/2 min
cat("a probabilidade é de ", dpois(6, lambda2) *100,"\n")

#c) 
lambda2 <- 8 # 4/min     == 8/2 min
cat("a probabilidade é de ", ppois(3, lambda2) *100,"\n")