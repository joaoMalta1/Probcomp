px <- function(x){
    (12 * x^2) * (1-x)
}

hx<- function(n){
    return (1)
}



gerarAmostraRej <- function (n){
    u <- runif(n)
    y <- runif(n)
    validos <- numeric(n)
    c <- 16/9 #derivar e igualar a 0 ai encontra o x depois aplcia esse valor em f(x) 
    aceitos<- 0
    while(aceitos <n ){
        razao <- fx(u)/ c* (hx(u))
        if(razao<= y){
            aceeitos <-aceitos +1 
            validos[aceitos]<- u
        }
    }
    return(validos)
}