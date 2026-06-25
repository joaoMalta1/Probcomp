curva <- function(x){
    ifelse(x <= 0.5, 4*x , 4*(1-x))
}

gerarAmostraInversa<- function(n){
    u <- runif(n)
    amostras <- numeric(n)
    idx_menor <- u <= 0.5
    idx_maior <- u > 0.5
    amostras[idx_menor] <- u[idx_menor]*4
    amostras[idx_maior] <- (4*u[idx_maior]) -1 
    return (amostras)
}


amostras <- gerarAmostraInversa(10000)

hist(amostras, probability = TRUE, lwd = 4, xlab = "eixo x", ylab = "densidade")

curve(curva,
    xlab = "x",
    ylab='densidade',
    add = TRUE,)