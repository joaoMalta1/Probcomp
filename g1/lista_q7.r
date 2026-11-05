nsamples <- 10000
dentro <- 0

for (i in 1:nsamples){
    x <- runif(1)
    y <- runif(1)

    if ((x^2 + y^2 <= 1) & ((x-1)^2 + (y-1)^2 <= 1)){
        dentro <- dentro +1 
    }
}

proporcao <- dentro /nsamples
meses <- 12
qtd <- 100
area <- proporcao * 1
cat("valor em um ano", area * meses * qtd, "\n")