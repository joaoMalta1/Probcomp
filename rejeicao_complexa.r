# Densidades
p <- function(x) { 2 * exp(-2 * x) }
h <- function(x) { exp(-x) }

gerar_X_rejeicao_exp <- function(n) {
  amostras <- numeric(n)
  aceitos <- 0
  c <- 2 # Constante calculada
  
  while(aceitos < n) {
    # 1. Gerar Y seguindo h(x) (Método da Inversa para Exponencial)
    u_inv <- runif(1)
    y <- -log(1 - u_inv) # Gerador de Y ~ Exp(1)
    
    # 2. Gerar U para o teste de aceitação
    u <- runif(1)
    
    # 3. Teste de aceitação: U <= p(y) / (c * h(y))
    razao <- p(y) / (c * h(y))
    
    if(u <= razao) {
      aceitos <- aceitos + 1
      amostras[aceitos] <- y
    }
  }
  return(amostras)
}

# Validação
set.seed(123)
amostra <- gerar_X_rejeicao_exp(10000)

hist(amostra, probability = TRUE, breaks = 50, col = "wheat", 
     main = "Rejeição com Proposta Exponencial", xlab = "x")
curve(2 * exp(-2 * x), from = 0, to = 5, add = TRUE, col = "darkred", lwd = 3)