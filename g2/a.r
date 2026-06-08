set.seed(123)
nsamples <- 1000000

eh_b <- 0
sofreu_sinestro_b_menos_trinta <- 0
sofreu_sinestro_b_mais_trinta <-0

eh_a <- 0
sofreu_sinestro_a_menos_trinta <- 0
sofreu_sinestro_a_mais_trinta <-0
for (i in 1:nsamples)
{
    plano = runif(1)
    if(plano < 0.4)
    {
        eh_b <- eh_b + 1
        idade <- sample(c("+30", "-30"), 1, replace = T, c(0.5,0.5))
        if (idade == "-30")
        {
            chance_sinestro <- runif(1)
            if(chance_sinestro < 0.066)
            {
                sofreu_sinestro_b_menos_trinta <- sofreu_sinestro_b_menos_trinta + 1
            }
        }
        else
        {
            chance_sinestro <- runif(1)
            if(chance_sinestro < 0.04)
            {
                sofreu_sinestro_b_mais_trinta <- sofreu_sinestro_b_mais_trinta + 1
            }
        }
    } else {
        eh_a <- eh_a + 1
        idade <- sample(c("+30", "-30"), 1, replace = T, c(0.7, 0.3))
    if (idade == "-30")
        {
            chance_sinestro <- runif(1)
            if(chance_sinestro < 0.03)
            {
                sofreu_sinestro_a_menos_trinta <- sofreu_sinestro_a_menos_trinta + 1
            }
        }
        
        else
        {
            chance_sinestro <- runif(1)
            if(chance_sinestro < 0.015)
            {
                sofreu_sinestro_a_mais_trinta <- sofreu_sinestro_a_mais_trinta + 1
            }
        }
    }
}

total_sinistros <- sofreu_sinestro_a_mais_trinta + sofreu_sinestro_a_menos_trinta + sofreu_sinestro_b_mais_trinta + sofreu_sinestro_b_menos_trinta

total_sinistros_ate30 <- sofreu_sinestro_a_menos_trinta + sofreu_sinestro_b_menos_trinta

p_b_dado_sinistro <- (sofreu_sinestro_b_menos_trinta + sofreu_sinestro_b_mais_trinta) / total_sinistros
p_b_dado_sinistro_e_ate30 <- sofreu_sinestro_b_menos_trinta / total_sinistros_ate30

cat("P(B|Sinistro):", round(p_b_dado_sinistro * 100, 2), "%\n") #1
cat ((total_sinistros_ate30 )/ total_sinistros, "\n") #2
cat("P(B|Sinistro e até 30 anos):", round(p_b_dado_sinistro_e_ate30 * 100, 2), "%\n") #3

#B)



#C)





