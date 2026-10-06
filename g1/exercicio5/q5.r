set.seed(123)

nsamples <- 1000
time <- 720

a <- 0.0005
b <- 0.004 

custo <- 0
falha_total <- 0
substituicao_simulacao <- numeric(nsamples)
custo_simulacao <- numeric(nsamples)


for(simulacao in 1:nsamples){
    vida <- 1
    substituicao <- 0
    custo <- 0
    for(hora in 1:time){
        if(runif(1)<= a + b * (1-vida)){
            #deu pt 
            falha_total <- falha_total +1 
            custo <- custo + 2000 
            vida <- 1
            substituicao <- substituicao +1 
            next
        }
        tipo_dano <- sample(c("LEVE","MODERADA","SEVERA"), 1, replace = TRUE, c(0.7, 0.2, 0.1))
        if(tipo_dano == "LEVE"){
            vida <- vida - 0.01
            if(vida <= 0 ){
                custo <- custo + 300
                vida <- 1 
                substituicao <- substituicao +1 

            }
        }
        else if( tipo_dano == "MODERADA"){
            vida <- vida - 0.03
            if(vida <= 0 ){
                custo <- custo + 400
                vida <- 1 
                substituicao <- substituicao +1 

            }
        }
        else{
            vida <- vida - 0.06
            if(vida <= 0 ){
                custo <- custo + 800
                vida <- 1
                substituicao <- substituicao +1 

            }
        }
    }
    substituicao_simulacao[simulacao]<- substituicao
    custo_simulacao[simulacao]<- custo
    
}

cat("media de sub por simulacao", mean(substituicao_simulacao), "\n")
cat("custo medio ", mean(custo_simulacao),"\n")
cat("media de falhas aleatorias totais", falha_total/nsamples, "\n")
