# Definir parâmetros
n <- 10000      # Tamanho da amostra
mu <- 12        # Média
sigma <- 4      # Desvio padrão

# Definir semente para reprodutibilidade
set.seed(123)

# Gerar variáveis independentes X e Y
# A função rnorm(n, mean, sd) gera n observações de uma normal
X <- rnorm(n, mean = mu, sd = sigma)
Y <- rnorm(n, mean = mu, sd = sigma)

# Validação:
# 1. Verificar correlação (deve ser próxima de 0 para independência)
correlacao <- cor(X, Y)
cat("Correlação entre X e Y:", correlacao, "\n")

# 2. Visualização das variáveis
plot(X, Y, pch = 20, col = rgb(0, 0, 1, 0.2), 
     main = "Simulação de Variáveis Normais Independentes",
     xlab = "X", ylab = "Y")

# 3. Estatísticas descritivas
cat("Média de X:", mean(X), "\n")
cat("Desvio padrão de X:", sd(X), "\n")