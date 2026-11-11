#Q1 A
set.seed(123)
prob <- 0.15
amostra <- 10
cat("uma peça defeituosa " , dbinom(1, amostra, prob) , "\n")
cat("nenhuma peça defeituosa " , dbinom(0, amostra, prob) , "\n")
cat("menos de das peças defeituosas " ,pbinom(1, amostra,prob) , "\n")

#Q1 B
nsample <- 100000
vet_random <- rbinom(nsample, amostra, prob)

cat("uma peça defeituosa com simu" , mean(vet_random == 1) , "\n")
cat("nenhuma peça defeituosa com simu" , mean(vet_random == 0) , "\n")
cat("menos de das peças defeituosas com simu " , mean(vet_random < 2) , "\n")


#Q1 C
vet_prob <- dbinom(0:amostra, amostra, prob)
cat("a distribuição é essa:" vet_prob, "\n")

#Q1 D
cat("euas lampadas ruins " , dhyper(2, 5, 15, 3) , "\n")
cat("duas ou mais defeituosas " , 1 - phyper(1, 5, 15, 3) , "\n")
cat("nenhuma defeituosa" , dhyper(0, 5, 15, 3), "\n")

#Q1 E
vet_random_hyper <- rhyper(nsample, 5, 15, 3)

cat("duas lampadas ruins com simu " , mean(vet_random_hyper == 2) , "\n")
cat("duas ou mais defeituosas com simu" , mean(vet_random_hyper >= 2) , "\n")
cat("nenhuma defeituosa com simu" , mean(vet_random_hyper == 0), "\n")


#Q2 A
lambda_diario <- 30 / 7
p_nao_responder <- 1 - ppois(3, lambda_diario)
dias <- 5
resp_a <- dbinom(5, dias, p_nao_responder)	
cat("a) Em cada um dos 5 dias:", resp_a, "\n")

#Q2 B
resp_b <- 1 - pbinom(0, dias, p_nao_responder) 
cat("b) Em pelo menos um dos 5 dias:", resp_b, "\n")
