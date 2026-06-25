T_total <- 24000
n_caixas <- 3
media_chegada <- 2
media_atendimento <- 3

simular_metro <- function() {
  tempo_atual <- 0
  # Vetor que guarda quando cada caixa ficará livre
  caixas_livres_em <- rep(0, n_caixas)
  
  atendidos <- 0
  nao_atendidos <- 0
  
  while (tempo_atual < T_total) {
    # Gera o intervalo até a próxima chegada
    tempo_atual <- tempo_atual + rexp(1, rate = 1/media_chegada)
    
    if (tempo_atual >= T_total) break
    
    # Verifica se há algum caixa livre no momento da chegada
    # Um caixa está livre se o tempo que ele termina o atendimento atual 
    # for menor ou igual ao tempo de chegada do cliente
    caixa_disponivel <- which(caixas_livres_em <= tempo_atual)
    
    if (length(caixa_disponivel) > 0) {
      atendidos <- atendidos + 1
      # O cliente ocupa o primeiro caixa disponível
      duracao_atendimento <- rexp(1, rate = 1/media_atendimento)
      caixas_livres_em[caixa_disponivel[1]] <- tempo_atual + duracao_atendimento
    } else {
      # Todos os caixas estão ocupados
      nao_atendidos <- nao_atendidos + 1
    }
  }
  return(c(atendidos = atendidos, nao_atendidos = nao_atendidos))
}

# a) Simulação de um único período
set.seed(123)
res_a <- simular_metro()
cat("--- Item a ---\n")
print(res_a)

# b) 100 experimentos
set.seed(123)
res_b <- replicate(100, simular_metro())
medias <- rowMeans(res_b)
cat("\n--- Item b (Médias de 100 experimentos) ---\n")
print(medias)