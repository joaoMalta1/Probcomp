#todos destros brasil 
br<- dhyper(3, 8, 2, 3)
#todos destros italia
it<- dhyper(4, 7, 3, 4)


cat("chance de ter alguem canhoto", 1- (br * it),"\n")

lambda_minuto <- 4

cat("chance de exatamente 6 pacotes perdidos, ", dpois(6, lambda_minuto * 2), "\n")
cat("chance de no maximo 3 pacotes perdidos, ", ppois(3, lambda_minuto * 2 ), "\n")
cat("o numero esperado é ", lambda_minuto * 5, "e a variancia também é ", lambda_minuto * 5, "\n")

