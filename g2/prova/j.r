# ==========================================
# LETRA A: f(x) = 5x^4
# ==========================================

gerar_amostra_A <- function(n) {
  # 1. Gera 'n' valores de uma Uniforme(0,1)
  U <- runif(n)
  
  # 2. Aplica a função inversa: x = U^(1/5)
  X <- U^(1/5)
  
  return(X)
}

# Gerando amostra e plotando (Letra A)
set.seed(123)
n_amostra <- 10000
amostra_A <- gerar_amostra_A(n_amostra)

# Plot Letra A
hist(amostra_A, probability = TRUE, breaks = 50, col = "lightblue", border = "white",
     main = "Método da Inversa - Letra A: f(x) = 5x^4", xlab = "x", ylab = "Densidade")
# Sobrepondo a curva teórica f(x)
curve(5 * x^4, from = 0, to = 1, add = TRUE, col = "red", lwd = 2)
legend("topleft", legend = c("Amostra Gerada", "Curva Teórica"), fill = c("lightblue", "red"))


# ==========================================
# LETRA B: Função por Partes (Triangular)
# ==========================================

gerar_amostra_B <- function(n) {
  U <- runif(n)
  X <- numeric(n) # Vetor vazio para guardar os resultados
  
  # Separando os índices baseados na condição da acumulada F(x) = 0.5
  idx_menor <- U <= 0.5
  idx_maior <- U > 0.5
  
  # Aplicando as inversas corretas para cada trecho (vetorizado para ser mais rápido)
  X[idx_menor] <- sqrt(U[idx_menor] / 2)
  X[idx_maior] <- 1 - sqrt((1 - U[idx_maior]) / 2)
  
  return(X)
}

# Função densidade teórica para o plot da Letra B
f_b <- function(x) {
  ifelse(x <= 0.5, 4 * x, 4 * (1 - x))
}

# Gerando amostra e plotando (Letra B)
amostra_B <- gerar_amostra_B(n_amostra)

# Plot Letra B
# O comando x11() ou windows() abre uma nova janela para não apagar o gráfico anterior
# x11() 
hist(amostra_B, probability = TRUE, breaks = 50, col = "lightgreen", border = "white",
     main = "Método da Inversa - Letra B: Função por Partes", xlab = "x", ylab = "Densidade")
# Sobrepondo a curva teórica g(x)
curve(f_b(x), from = 0, to = 1, add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Amostra Gerada", "Curva Teórica"), fill = c("lightgreen", "red"))