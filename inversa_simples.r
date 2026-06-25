gerarAmostra <- function(nsamples){
    u <- runif(nsamples)
    resp <- 4 * (u^(1/2)) +1 
    return (resp)
}


nsamples <- 1000
amostra <- gerarAmostra(nsamples)

hist(
    amostra,
    probability = TRUE,
    xlab = "x",
    ylab = "densidade")

curve(
    1/4 * (x-1),
    xlab = " x",
    ylab = "",
    add = TRUE,
    lwd = 2

)