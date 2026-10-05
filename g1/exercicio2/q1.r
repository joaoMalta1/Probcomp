set.seed(123)
nsamples <- 10000

pf_nao_ajustado <- 3500

#vet pra cada simulação
pf_ajustado_vec <- numeric(nsamples)
tempo_semanas_vec <- numeric(nsamples)
custo_vec <- numeric(nsamples)

for(i in 1:nsamples) {
    dist_dados <- sample(c(2, 3, 4), 1, replace = TRUE, prob = c(0.2, 0.6, 0.2))
    req_desempenho <- sample(c(3, 4, 5), 1, replace = TRUE, prob = c(0.1, 0.7, 0.2))
    reusabilidade <- sample(c(1, 2, 3), 1, replace = TRUE, prob = c(0.1, 0.7, 0.2))
    processamento <- sample(c(2, 3, 4, 5), 1, replace = TRUE, prob = c(0.1, 0.6, 0.2, 0.1))
    outras_10 <- sample(c(1, 2, 3), 10, replace = TRUE, prob = c(0.2, 0.6, 0.2))
    
    # soma das caracteristicas e calculo do FA
    soma_caracteristicas <- dist_dados + req_desempenho + reusabilidade + processamento + sum(outras_10)
    fa <- 0.65 + 0.01 * soma_caracteristicas
    
    # PF Ajustado
    pf_ajustado <- pf_nao_ajustado * fa
    pf_ajustado_vec[i] <- pf_ajustado
    
    # 5. produtividade (horas/PF)
    produtividade <- sample(c(4, 5, 6), 1, replace = TRUE, prob = c(0.2, 0.7, 0.1))
    
    # tempo em semanas (40 horas/semana)
    tempo_horas <- pf_ajustado * produtividade  #cada ponto de funcao ajustado leva * horas definidas no sample anterior
    tempo_semanas <- tempo_horas / 40
    tempo_semanas_vec[i] <- tempo_semanas
    
    # custo 
    custo_hora <- sample(c(80, 100, 120), 1, replace = TRUE, prob = c(0.2, 0.6, 0.2))
    custo_total <- tempo_horas * custo_hora #as horas usadas pra finalizar todos os PF * custo 
    custo_vec[i] <- custo_total
}

# ==================== RESULTADOS #

# a) 
media_pf_ajustado <- mean(pf_ajustado_vec)
cat("Valor médio esperado de PF_ajustado", media_pf_ajustado,"\n" )

# b) 
media_tempo_semanas <- mean(tempo_semanas_vec)
cat("Tempo médio em semanas", media_tempo_semanas,"\n" )

# c) 
media_custo <- mean(custo_vec)
cat("Custo médio do sistema", media_custo, "\n")

# d)
prob_custo_1500k <- mean(custo_vec < 1500000)
cat("Probabilidade do custo ser menor que R$ 1.500.000,00", prob_custo_1500k,"\n" )

# e) 
prob_tempo_450w <- mean(tempo_semanas_vec < 450)
cat("Probabilidade do tempo ser menor que 450 semanas", prob_tempo_450w,"\n" )

# f) histogramas 

hist(pf_ajustado_vec, 
main = "PF Ajustado", 
xlab = "PF", 
col = "skyblue", 
border = "white")

hist(tempo_semanas_vec, 
main = "Tempo (Semanas)", 
xlab = "Semanas", 
col = "salmon", 
border = "white")

hist(custo_vec, 
main = "Custo Total (R$)", 
xlab = "R$", 
col = "lightgreen", 
border = "white")

