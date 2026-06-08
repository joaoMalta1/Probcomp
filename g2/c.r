set.seed(123)
nsamples <- 1000000

eh_b <- 0
sofreu_sinestro_b_menos_trinta <- 0
sofreu_sinestro_b_mais_trinta <- 0

eh_a <- 0
sofreu_sinestro_a_menos_trinta <- 0
sofreu_sinestro_a_mais_trinta <- 0
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

cat("Cenário atual\n")
cat("P(B|Sinistro):", round(p_b_dado_sinistro * 100, 2), "%\n")
cat("P(Até 30 anos | Sinistro):", round((total_sinistros_ate30 / total_sinistros) * 100, 2), "%\n")
cat("P(B|Sinistro e até 30 anos):", round(p_b_dado_sinistro_e_ate30 * 100, 2), "%\n\n")

# C) Alteração: aumentar a proporção de clientes jovens em 20 pontos percentuais
# Aqui mudamos apenas as probabilidades de idade nos dois planos.
eh_b <- 0
sofreu_sinestro_b_menos_trinta <- 0
sofreu_sinestro_b_mais_trinta <- 0

eh_a <- 0
sofreu_sinestro_a_menos_trinta <- 0
sofreu_sinestro_a_mais_trinta <- 0
for (i in 1:nsamples)
{
    plano = runif(1)
    if(plano < 0.4)
    {
        eh_b <- eh_b + 1
        idade <- sample(c("+30", "-30"), 1, replace = T, c(0.7,0.3)) # alterado: jovens de 50% para 70% no plano B
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
        idade <- sample(c("+30", "-30"), 1, replace = T, c(0.5,0.5)) # alterado: jovens de 30% para 50% no plano A
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

cat("Cenário com mais jovens (+20 pontos)\n")
cat("P(B|Sinistro):", round(p_b_dado_sinistro * 100, 2), "%\n")
cat("P(Até 30 anos | Sinistro):", round((total_sinistros_ate30 / total_sinistros) * 100, 2), "%\n")
cat("P(B|Sinistro e até 30 anos):", round(p_b_dado_sinistro_e_ate30 * 100, 2), "%\n")
