set.seed(123)

nsamples <- 10000
custo <- 0 
falhou <- 0
sucesso_direto <- 0
sucesso_com_retry <- 0

retry <- function(qtd_retry, chance_erro){
    for(i in 1:2){
        if(runif(1) >= chance_erro){
            return(TRUE)
        }
    }
    return(FALSE)
}

for(i in 1:nsamples){

    tentou_retry <- 0 
    # GATEWAY
    tenta_g <- runif(1) 

    if(tenta_g <= 0.02){
        tentou_retry <- tentou_retry + 1 
        tenta_g <- retry(2, 0.02)
        if(tenta_g == FALSE){
            falhou <- falhou + 1
            next
        }
    } 

    # PAGAMENTO
    tenta_p <- runif(1) 
    if(tenta_p <= 0.05){
        tentou_retry <- tentou_retry + 1 
        tenta_p <- retry(2, 0.05)
        if(tenta_p == FALSE){
            falhou <- falhou + 1
            next
        }
    }

    # INVENTÁRIO
    tenta_i <- runif(1) 
    if(tenta_i <= 0.08){
        tentou_retry <- tentou_retry + 1 
        tenta_i <- retry(2, 0.08)
        if(tenta_i == FALSE){
            falhou <- falhou + 1
            custo <- custo + 150
            next
        }
    }

    # SUCESSO
    if(tentou_retry != 0){
        sucesso_com_retry <- sucesso_com_retry + 1
    }
    else{
        sucesso_direto <- sucesso_direto + 1
    }
}


cat("chance de sucesso direto:",
    sucesso_direto/nsamples, "\n")

cat("chance global de sucesso:",
    (sucesso_direto + sucesso_com_retry)/nsamples, "\n")

cat("custo medio:",
    custo/nsamples, "\n")