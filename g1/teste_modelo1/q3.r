set.seed(123)

benford <-function(algarismo){
    return(log10(1+(1/algarismo)))
}

#A
armazena_benford <- numeric(9)
for(i in 1:9){
    cat("prob do algarimso " , i , " ", benford(i),"\n")
    armazena_benford[i] <- benford(i)
}

#B
qtd <- 10000
processoA<- sample(1:1000000, replace = TRUE,  qtd)
aux <- substr(processoA, 1, 1)
armazena_processoA <- numeric(9)


for (i in 1:9){
    cat("chance de ", i , sum(aux == as.character(i))/qtd,"\n")
    armazena_processoA[i] <- sum(aux == as.character(i))/qtd
}

#C
processoB <- numeric(qtd) 
for (i in 1:qtd){
    aux <- sample(1:9, replace = TRUE,10)
    produto <- 1
    for(j in 1:10){
        produto <- produto * aux[j]
    }
    processoB[i] <- produto
}

aux <- substr(processoB, 1, 1)
armazena_processoB <- numeric(9)
for (i in 1:9){
    cat("chance de ", i , sum( aux == as.character(i))/qtd,"\n")
    armazena_processoB[i] <- sum( aux == as.character(i))/qtd
}

#D
plot(1:9, armazena_processoA,
    xlab = "Algarismo",
    ylab = "Frequência",
    col = "red",
    )

points(1:9, armazena_benford,
    col = "blue",
    )

#E

plot(armazena_processoB,
xlab = "algarismos",
ylab = "frequencia",
col = "red")

lines(armazena_benford,
type = "p",
col = "blue")


#F preguiça 


#G preguiça 