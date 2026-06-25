px <- function(x){
    (12* x^2) - (12 * x^3)
 }

hx <- function(x){
    return(1) #pq ele pede uniforme 
}

gerarAmostraRej <- function(n){
    aceitos<- 0
    validos<- numeric(n)
    while(aceitos < n){
        u <- runif(1)
        y<- runif(1)
        razao<- px(y)/ ((16/9) *hx(y))
        if(u <= razao){
            aceitos<- aceitos +1
            validos[aceitos] <- y
        }
    }
    return (validos)
}

nsamples<- 100000
x <- gerarAmostraRej(nsamples)

hist(
    x,
    probability=TRUE,
    xlab ="eixo x ",
    ylab = "densidade"
)
curve(
    (12* x^2) - (12 * x^3),
    xlab ="eixo x ",
    ylab = "densidade",
    add = TRUE
)