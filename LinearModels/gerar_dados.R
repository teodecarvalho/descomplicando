library(tidyverse)

# Dados lognormais
set.seed(23072020) 
alpha <- 7
beta <- -5
pH <- seq(3, 7, by = .5)
dados <- expand.grid(pH = pH, Rep = 1:5)
residuos <- rnorm(n = nrow(dados), sd = 3)
dados$Conc <- alpha + beta * dados$pH + residuos
dados$Conc <- exp(dados$Conc)

# Relação quadratica
doses <- expand.grid(N = c(0, 30, 60, 90, 120, 150, 180),
                     Rep = 1:5)

media_geral <- 100
efeitos <- c(1 * media_geral,   # Intercepto
             .2,                # Efeito primeiro grau N
             -.001)             # Efeito segundo grau N

mm <- model.matrix(~N + I(N^2), data = doses)
doses$Prod <- as.vector(mm %*% efeitos)
set.seed(28102016) # Apenas para assegurar reprodutibilidade
desvio_erro <- 10
doses$Prod <- doses$Prod + rnorm(n = nrow(doses), sd = desvio_erro)

# Dados de contagem
# Apenas para assegurar reprodutibilidade
set.seed(28102016) 
efeito <- .8
beta <- log(efeito)
coefs <- c(alpha = 6.91, beta = beta)
pragas <- expand.grid(Dose = seq(0, 30, by = 5), Rep = 1:5)
mm_pragas <- model.matrix(~Dose, data = pragas)
pragas$lambda <- exp(mm_pragas %*% coefs)[, 1]
pragas$Insetos <- rpois(n = nrow(pragas), lambda = pragas$lambda)





