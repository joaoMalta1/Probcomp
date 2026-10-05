######################## QUESTAO A
set.seed(123)
qtd_clientes <- 1000000

ate_30_a <- 0
acima_30_a <- 0
ate_30_b <- 0
acima_30_b <- 0

acima_30_a_teve_sinestro <- 0
ate_30_a_teve_sinestro <- 0
acima_30_b_teve_sinestro <- 0
ate_30_b_teve_sinestro <- 0


for(i in 1:qtd_clientes){
	plano <- sample(c("A", "B"), 1, replace = FALSE,c(0.6, 0.4))
	if(plano=="A"){ 
		if(runif(1) <= 0.3){
			ate_30_a <- ate_30_a +1
			if(runif(1) <= 0.03){
				ate_30_a_teve_sinestro <- ate_30_a_teve_sinestro +1
			}
		}
		else{
			acima_30_a <- acima_30_a +1 
			if(runif(1) <= 0.015){
				acima_30_a_teve_sinestro <- acima_30_a_teve_sinestro +1
			}
		}
	}
	else if(plano == "B"){
		if(runif(1) <= 0.5){
			ate_30_b <- ate_30_b +1
			if(runif(1) <= 0.06){
				ate_30_b_teve_sinestro <- ate_30_b_teve_sinestro +1
			}
		}
		else{
			acima_30_b <- acima_30_b +1 
			if(runif(1) <= 0.04){
				acima_30_b_teve_sinestro <- acima_30_b_teve_sinestro +1
			}
		}
	}
}

total_sinestro <- ate_30_a_teve_sinestro + ate_30_b_teve_sinestro + acima_30_a_teve_sinestro + acima_30_b_teve_sinestro

cat("P(B|Sinistro) == ", (ate_30_b_teve_sinestro + acima_30_b_teve_sinestro)/ total_sinestro,"\n")
cat("P(ATÉ 30|Sinistro) == ", (ate_30_b_teve_sinestro + ate_30_a_teve_sinestro)/ total_sinestro,"\n")
cat("P(B | Sinistro e Até 30) == ", (ate_30_b_teve_sinestro)/ (ate_30_b_teve_sinestro + ate_30_a_teve_sinestro),"\n")

######################## QUESTAO B
clientes_a <- 0.6 * qtd_clientes
clientes_a_ate30 <- 0.3 * clientes_a
clientes_a_cima30 <- clientes_a * 0.7

clientes_a_cima30_teve_sinestro <- clientes_a_cima30 * 0.015
clientes_a_ate30_teve_sinestro <- clientes_a_ate30 * 0.03

clientes_b <- 0.4 * qtd_clientes
clientes_b_ate30 <- 0.5 * clientes_b
clientes_b_cima30 <-  0.5 * clientes_b

clientes_b_cima30_teve_sinestro <- clientes_b_cima30 * 0.04
clientes_b_ate30_teve_sinestro <- clientes_b_ate30 * 0.06

clientes_totais_teve_sinestro <- clientes_b_ate30_teve_sinestro + clientes_b_cima30_teve_sinestro + clientes_a_ate30_teve_sinestro + clientes_a_cima30_teve_sinestro

cat("=========== Resultados analiticos=================\n")

cat("P(B|Sinistro) == ", (clientes_b_ate30_teve_sinestro + clientes_b_cima30_teve_sinestro)/ clientes_totais_teve_sinestro,"\n")
cat("P(ATÉ 30|Sinistro) == ", (clientes_b_ate30_teve_sinestro + clientes_a_ate30_teve_sinestro)/ clientes_totais_teve_sinestro,"\n")
cat("P(B | Sinistro e Até 30) == ", (clientes_b_ate30_teve_sinestro)/ (clientes_b_ate30_teve_sinestro + clientes_a_ate30_teve_sinestro),"\n")


######################## QUESTAO C
ate_30_a <- 0
acima_30_a <- 0
ate_30_b <- 0
acima_30_b <- 0

acima_30_a_teve_sinestro <- 0
ate_30_a_teve_sinestro <- 0
acima_30_b_teve_sinestro <- 0
ate_30_b_teve_sinestro <- 0

for(i in 1:qtd_clientes){
    plano <- sample(c("A", "B"), 1, replace = FALSE, prob = c(0.6, 0.4))
    if(plano == "A"){ 
        # aumento de 20%, logo atual * 1.2
        if(runif(1) <= 0.03 * 1.2){
            ate_30_a <- ate_30_a + 1
            if(runif(1) <= 0.03){
                ate_30_a_teve_sinestro <- ate_30_a_teve_sinestro + 1
            }
        } else {
            acima_30_a <- acima_30_a + 1 
            if(runif(1) <= 0.015){
                acima_30_a_teve_sinestro <- acima_30_a_teve_sinestro + 1
            }
        }
    } else if(plano == "B"){
        # aumento de 20%, logo atual * 1.2
        if(runif(1) <= 0.5 * 1.2){
            ate_30_b <- ate_30_b + 1
            if(runif(1) <= 0.06){
                ate_30_b_teve_sinestro <- ate_30_b_teve_sinestro + 1
            }
        } else {
            acima_30_b <- acima_30_b + 1 
            if(runif(1) <= 0.04){
                acima_30_b_teve_sinestro <- acima_30_b_teve_sinestro + 1
            }
        }
    }
}

total_sinestro <- ate_30_a_teve_sinestro + ate_30_b_teve_sinestro + acima_30_a_teve_sinestro + acima_30_b_teve_sinestro
total_sinestro_ate30 <- ate_30_b_teve_sinestro + ate_30_a_teve_sinestro

cat("=========== Resultados Questão C =================\n")
cat("Probabilidade Global de Sinistro == ", total_sinestro / qtd_clientes, "\n")
cat("P(B|Sinistro) == ", (ate_30_b_teve_sinestro + acima_30_b_teve_sinestro) / total_sinestro, "\n")
cat("P(ATÉ 30|Sinistro) == ", total_sinestro_ate30 / total_sinestro, "\n")
cat("P(B | Sinistro e Até 30) == ", ate_30_b_teve_sinestro / total_sinestro_ate30, "\n")