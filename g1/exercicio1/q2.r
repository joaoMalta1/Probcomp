set.seed(123)

timer <- 720
gasto_simu <- 0
troca_peca <- 0
falha_total <- 0 
nsamples <- 1000

for (i in 1:nsamples){
	vida <- 1 
	for (j in 1:timer){
		prob_falah_total <- runif(1)
		if(prob_falah_total <= 0.002){
			falha_total <- falha_total + 1
			troca_peca <- troca_peca +1 
			vida <- 1 
			gasto_simu <- gasto_simu + 2000 
			next #avança proxima hora do loop, deveria ter?
		}
		falha <- sample(c("L", "M", "S"), 1, replace = TRUE ,c(0.7, 0.2, 0.1))
		if(falha == "L"){
			vida <- vida -0.01
		}
		else if (falha == "M"){
			vida <- vida -0.03
		}
		else{
			vida <- vida -0.07
		}

		if(vida <= 0){
			vida <- 1
			troca_peca<- troca_peca + 1 
			if(falha == "L"){
			gasto_simu <- gasto_simu + 400
		}
		else if (falha == "M"){
			gasto_simu <- gasto_simu + 500
		}
		else{
			gasto_simu <- gasto_simu + 700
		}
		}
	}
}
#A
cat("media de substituicao: ", (troca_peca )/nsamples)
#B
cat("custo medio: ", gasto_simu/nsamples)
#C
cat("numero de falhas medio: ", falha_total/nsamples)
