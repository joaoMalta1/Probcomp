#a)
#nao pois ao ter um dos problemas a chance de ter o outro 

#para que os evento sejam independenter a interseçãp tem que ser igual a multiplicação da chance dos dois
#𝑃(𝐸1 ∩ 𝐸2) = 0,04
#𝑃(𝐸1) 𝑃(𝐸2) = 0,016
#Como 𝑃(𝐸1 ∩ 𝐸2) != P(E1) 𝑃(𝐸2) os eventos 𝐸1 e 𝐸2 não são independentes.

set.seed(123)
nsamples <- 10000; cont.acerto <- 0; cont.gabaritou <- 0; nquestao <- 10
gabarito <- c("V","F","V","V","F","V","F","V","F","F") # um gabarito qualquer
for (i in 1:nsamples) {
cont.acerto <- 0
for (questao in 1:nquestao) { # escolhe as 10 respostas
resposta <- sample(c("V","F"), 1, replace = T)
if (resposta == gabarito[questao]) { # verifica se acertou a resposta
cont.acerto <- cont.acerto + 1
}
}
if (cont.acerto == nquestao) { # verifica a quantidade de acertos
cont.gabaritou <- cont.gabaritou + 1
}
}
prob <- (cont.gabaritou/nsamples)
print(prob)
