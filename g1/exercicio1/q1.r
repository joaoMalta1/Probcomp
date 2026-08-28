###A
set.seed(123)

lcg<- function(a,x,c,m,qtd){
	u <- numeric(qtd)
	for(i in 1:qtd){
		x <- (a*x + c) %% m
		u[i] <- x/m
	}
	return (u)
}

qtd_jogadores <- 10
jogadores <- numeric(10)
a <- 39373 
c <- 0
m <-  (2^31) - 1
x0 <- 3



forca <- 10 + (20 - 10) * lcg(a, x0, c, m, qtd_jogadores)
agilidade <- 5 + (15 - 5) * lcg(a, x0, c, m, qtd_jogadores) 
inteligencia <- 8 + (18 - 8)* lcg(a, x0, c, m, qtd_jogadores) 

for(i in 1:qtd_jogadores){
	cat("estatisticas jogador",i, "FOR", forca[i], "agi", agilidade[i], "int", inteligencia[i],"\n")
}

cat("\n###########################\n")
###B

forca_runif <- 10 + (20 - 10) * runif(qtd_jogadores)
agilidade_runif <- 5 + (15 - 5) * runif(qtd_jogadores) 
inteligencia_runif <- 8 + (18 - 8)* runif(qtd_jogadores) 

for(i in 1:qtd_jogadores){
	cat("estatisticas jogador, com runif ",i, "FOR", forca[i], "agi", agilidade[i], "int", inteligencia[i],"\n")
}


