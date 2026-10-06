set.seed(123)

nsamples <- 10000

lista_opcoes <- c(0, 0 ,0, 0)
for(i in 1:nsamples){
    x1 <- runif(1)
    y1 <- runif(1)

    
    x2 <- runif(1)
    y2 <- runif(1)

    
    x3 <- runif(1)
    y3 <- runif(1)

    area <- (1/2 * abs(x1* (y2 - y3) + x2* (y3 - y1) + x3 * (y1 - y2)))
    if(area < 0.05){
        lista_opcoes[1] <- lista_opcoes[1] +1 
    }
    else if(area < 0.1){
        lista_opcoes[2] <- lista_opcoes[2] +1
    }
    else if(area < 0.2){
        lista_opcoes[3] <- lista_opcoes[3] +1
    }
    else{
        lista_opcoes[4] <- lista_opcoes[4] +1
    }
    
}

cat("probabilidades", lista_opcoes/nsamples)