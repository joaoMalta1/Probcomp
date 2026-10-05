set.seed(123)
nsamples <- 10000

vet_pfa <- numeric(nsamples)
vet_tempo <- numeric(nsamples)
vet_custo <- numeric(nsamples)

for(i in 1:nsamples){
	entrada_externa <- sample(c(3, 4, 6), 25, replace = TRUE, c(0.3, 0.5, 0.2))
	saida_externa <- sample(c(4, 5, 7), 20, replace = TRUE, c(0.25, 0.6, 0.15))
	consulta_externa <- sample(c(3, 4, 6), 15, replace = TRUE, c(0.4, 0.4, 0.2))
	arquivo_logico <- sample(c(7, 10, 15), 12, replace = TRUE, c(0.2, 0.5, 0.3))
	arquivo_interface <- sample(c(5, 7, 10), 8, replace = TRUE, c(0.35, 0.45, 0.2))
	
	somatorio_pf <- sum(entrada_externa) + sum(saida_externa) + sum(consulta_externa) + sum(arquivo_interface) + sum(arquivo_logico)
	fa <- sample(c(1.05, 1.15, 1.25), 1, replace = FALSE, c(0.2, 0.6, 0.2))

	pf_ajustado = somatorio_pf * fa
	vet_pfa[i] <- pf_ajustado

	produtividade <- sample(c(4, 5, 6), 1, replace = FALSE, c(0.2, 0.6, 0.2))

	horas_trabalhadas <- produtividade * pf_ajustado
	horas_semana <- horas_trabalhadas/40
	vet_tempo[i] <- horas_semana

	custo <- sample(c(80, 100, 120), 1, replace = FALSE, c(0.2, 0.6, 0.2))
	vet_custo[i] <- custo * horas_trabalhadas
}

#a)
cat("valor medio dos pfa", mean(vet_pfa),"\n")

#b)
cat("tempo media em semanas", mean(vet_tempo),"\n")

#c)
cat("custo medio ", mean(vet_custo),"\n")

#d)
cat("A probabilidade de o custo ser menor que R$ 280.000,00: ", sum(vet_custo < 280000)/ sum(vet_custo >1),"\n")

#e)
cat(" A probabilidade de o tempo ser menor que 60 semanas ", sum(vet_tempo < 60)/sum(vet_tempo > 1),"\n")

#f)
hist(vet_pfa,
xlab = "pontos de função",
col= "lightgreen",
border = "white"
)

hist(vet_tempo,
xlab = "semana",
col = "lightblue",
border = "white"

)

hist(vet_custo,
xlab = "custo",
col = "salmon",
border = "white"

)

