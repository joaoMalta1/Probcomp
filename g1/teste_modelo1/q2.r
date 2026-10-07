segurados <- 1000
aux_anual <- 20000
premio_uni <- 100000
t <- 15
r <- 0.05
valor_total_futuro <- 0
valor_total_presente <- 0

#chance de ativo virar invalido e obito é 0,12 e 0,03
#chance de invalido virar valido e obito é 0,08 e 0,20

set.seed(123)

vetor_pessoas <- numeric(segurados)
vetor_pessoas <- vetor_pessoas + 1 

for (i in 1:t){
    for (j in 1:segurados){
        if(vetor_pessoas[j] == 1){
            vetor_pessoas[j] <- sample(c(1,2,3),1,replace = TRUE, c(0.85,0.12,0.03))
        }
        else if(vetor_pessoas[j] == 2){
            vetor_pessoas[j] <- sample(c(1,2,3),1,replace = TRUE, c(0.08,0.72,0.20))
        }
        else if(vetor_pessoas[j] == 0){
            next
        }
        else{
            vetor_pessoas[j] <- sample(c(1,2,3),1,replace = TRUE, c(0,0,1))
        }
    }
    valor_2 <- sum(vetor_pessoas == 2) * 20000
    valor_3 <- sum(vetor_pessoas == 3) * premio_uni
    vetor_pessoas[vetor_pessoas == 3] <- 0
    valor_futuro <- valor_2
    valor_presente <- valor_futuro/((1+r)^i)
    valor_total_futuro <- valor_total_futuro + valor_futuro
    valor_total_presente <- valor_total_presente + valor_presente
}

cat(valor_total_futuro ,"\n")
cat(valor_total_presente)
