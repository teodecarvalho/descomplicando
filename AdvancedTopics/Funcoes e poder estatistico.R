library(tidyverse)

faca_simulacoes <- function(n.rep = 3, n.sims = 1000, desvio = 30){
  signif <- 0
  for(sim in 1:n.sims){
    doses <- c(seq(0, 100, by = 15), 100)
    dados <- expand.grid(Doses = doses, Rep = 1:n.rep)
    beta <- .3
    alpha <- 100
    erros <- rnorm(n = nrow(dados), sd = desvio)
    
    dados$Fit <- alpha + beta * dados$Doses
    dados$Cresc <- dados$Fit + erros
    
    modelo <- lm(Cresc ~ Doses, data = dados)
    sumario <- summary(modelo)
    p <- sumario$coefficients[2, 4]
    if(p <= 0.05){
      signif <- signif + 1
    }
  }
  prob_positivo <- 100 * signif / n.sims
  return(prob_positivo)
}


set.seed(27072020)
resultados <- map_dbl(3:20, faca_simulacoes)


plot(y = resultados, x = 3:20)
lines(y = resultados, x = 3:20)
