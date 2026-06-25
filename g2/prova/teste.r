set.seed(123)

gerar_X_intervalo <- function(n) {
  u <- runif(n)
  # Fórmula: início_do_intervalo + (amplitude * sqrt(u))
  # Para [1, 5], amplitude = 4
  x <- 1 + 4 * sqrt(u)
  return(x)
}

nsamples <- 10000
amostra <- gerar_X_intervalo(nsamples)

# Validação
hist(amostra, 
     probability = TRUE, 
     breaks = 50, 
     col = "lightgreen", 
     border = "white",
     xlab = "x", 
     ylab = "Densidade",
     main = "Validação: f(x) = 1/8 * (x - 1) no intervalo [1, 5]")

curve(0.125 * (x - 1), 
      from = 1, to = 5, 
      add = TRUE, 
      col = "red", 
      lwd = 2)