#Q2 A 
set.seed(123)
prob <- 0.1
amostra <- 10

cat("exatamente 3", dbinom(3, amostra ,prob),"\n")
cat("mais de 2 ", 1 - pbinom(2, amostra ,prob),"\n")
cat("até 4", pbinom(4, amostra ,prob),"\n")


# Q2 B
vet <- dbinom(0:amostra, amostra, prob)
plot(vet,
xlab = "mouses defeituosos",
ylab = "probabilidade",
type = "p")


#na simulação podemos usar r"distribuicao" nsample vezes e fazer a operação de vetor e no fim usar mean 
n_simulacoes <- 100000 
resultados_simulados <- rbinom(n_simulacoes, size = amostra, prob = prob)
cat("simu ext 3:", mean(resultados_simulados == 3), "\n")
cat("simu mais de 2:", mean(resultados_simulados > 2), "\n")
cat("simu ate 4:", mean(resultados_simulados <= 4), "\n")


#Q3  A geometrica é até o primeiro acerto 
prob <- 0.85
cat("a chance de seja exatamente 4 é: ", dgeom(3, prob), "\n")
cat("a chance de ser no maximo 4 é: ", pgeom(3, prob), "\n")
resultados_simulados <- rgeom(n_simulacoes,prob)
cat("a chance de ao menos 4 é: ",mean(resultados_simulados >= 3), "\n")

#Q4 A
# --- DADOS DO BRASIL ---
# População: 8 destros (m), 2 não destros (n) -> Total 10
# Sorteio (k): 3 nadadores
# Queremos que TODOS os 3 sorteados sejam destros (x = 3)
prob_br_todos_destros <- dhyper(x = 3, m = 8, n = 2, k = 3)

# --- DADOS DA ITÁLIA ---
# População: 7 destros (m), 3 não destros (n) -> Total 10
# Sorteio (k): 4 nadadores
# Queremos que TODOS os 4 sorteados sejam destros (x = 4)
prob_it_todos_destros <- dhyper(x = 4, m = 7, n = 3, k = 4)

cat(1-(prob_br_todos_destros * prob_it_todos_destros)) 

#Q4 B

resultados_simulados_br <- rhyper(n_simulacoes, 8, 2, 3)
resultados_simulados_it <- rhyper(n_simulacoes, 7, 3, 4)
cat("teste bonito: ", 1-(sum((resultados_simulados_br == 3) &(resultados_simulados_it == 4)))/n_simulacoes, "\n") #outro jeito de fazer 
resultado_total_destros <- resultados_simulados_br + resultados_simulados_it 
cat("a chance de ter ao menos 1 esquerdista: ",mean(resultado_total_destros < 7), "\n")

#Q5
lambda <- 4
vet <- dpois(1:100, lambda)
cat(vet)
vet_acumulada <- ppois(1:100,lambda)
plot(vet,
xlab = "qtd_pacotes perdidos",
ylab = "prob",
type = "l")

plot(vet_acumulada,
col = "lightgreen",
type = "l")