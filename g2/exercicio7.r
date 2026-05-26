#1 a)
set.seed(123)
clientes <- 1000000
total_sinistros <- 0
sinistros_b <- 0
ate_30 <- 0

for (i in 1:clientes)
{
  plano = runif(1)
  
  if(plano <= 0.4) { 
    
    idade = runif(1)
    if(idade <= 0.50) {
      prob_sinistro = 0.06  # até 30 anos 
        if(runif(1) <= prob_sinistro) {
            total_sinistros = total_sinistros + 1
            sinistros_b = sinistros_b + 1
            ate_30 = ate_30 + 1 # <-- Incrementa aqui
        }
    } else {
      prob_sinistro = 0.04  # acima de 30 anos
      if(runif(1) <= prob_sinistro) {
        total_sinistros = total_sinistros + 1
        sinistros_b = sinistros_b + 1
      }
    }
    
  } else { #caiu no plano A 60%
    
    idade = runif(1)
    if(idade <= 0.30) {

      prob_sinistro = 0.03  # até 30 anos (30% do plano)
        if(runif(1) <= prob_sinistro) {
            total_sinistros = total_sinistros + 1
            ate_30 = ate_30 + 1 # <-- Incrementa aqui
        }    
    } else {
      prob_sinistro = 0.015 # acima de 30 anos (70% do plano)
      if(runif(1) <= prob_sinistro) {
        total_sinistros = total_sinistros + 1
      }
    }
    

  }
}

p_b_dado_sinistro = sinistros_b / total_sinistros
p_ate_30_dado_sinistro = ate_30 / total_sinistros

cat("total de sinistros:", total_sinistros, "\n")
cat("sinistros de B:", sinistros_b, "\n")
cat("P(B|sinistro):", round(p_b_dado_sinistro * 100, 2), "%\n")
cat("P(ate_30|sinistro):", round(p_ate_30_dado_sinistro * 100, 2), "%\n") #round arredonda para 2 casas decimais




#FALTOU FAZER P(𝐵 ∣ Sinistro e ateˊ 30 anos).
