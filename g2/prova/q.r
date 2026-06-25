# 1. Definindo as funções
p <- function(x) {
  return(12 * x^2 * (1 - x))
}

h <- function(x) {
  return(1) # Distribuição Uniforme(0,1)
}

# 2. Implementando a função do Método da Rejeição
gerar_amostra_rejeicao <- function(n) {
  amostras <- numeric(n) # Vetor para guardar os valores aceitos
  aceitos <- 0           # Contador de valores aceitos
  c <- 16 / 9            # Constante calculada analiticamente
  
  while(aceitos < n) {
    # Passo a: Gerar Y a partir de h(x), que é Unif(0,1)
    y <- runif(1, min = 0, max = 1)
    
    # Passo b: Gerar U a partir de uma Unif(0,1)
    u <- runif(1, min = 0, max = 1)
    
    # Passo c: Teste de aceitação U <= p(Y) / (c * h(Y))
    razao <- p(y) / (c * h(y))
    
    if (u <= razao) {
      aceitos <- aceitos + 1
      amostras[aceitos] <- y
    }
  }
  
  return(amostras)
}

# 3. Gerando uma amostra de tamanho 10.000
tamanho_amostra <- 10000
set.seed(123) # Para reprodutibilidade
amostra_X <- gerar_amostra_rejeicao(tamanho_amostra)

# 4. Plotando o histograma e a curva teórica
hist(amostra_X, 
     probability = TRUE, # Eixo Y como densidade, não frequência
     breaks = 50, 
     col = "lightblue", 
     border = "white",
     main = "Histograma da Amostra vs Densidade Teórica",
     xlab = "x", 
     ylab = "Densidade")

# Sobrepondo a curva da função p(x) teórica para conferência
curve(p(x), from = 0, to = 1, add = TRUE, col = "red", lwd = 2)
legend("topleft", legend = c("Amostra Gerada", "p(x) Teórica"), 
       fill = c("lightblue", "red"))