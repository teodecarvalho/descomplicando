library(tidyverse)
library(car)
library(gvlma)
# Global validation of linear model assumptions

set.seed(1072020)

# Numero de repeticoes
n <- 5
# Doses de N
doses <- seq(0, 100, by = 10)
# Numero total de observacoes
n_obs <- n * length(doses)
# Efeito simulado do N
N_efeito <- 2
# Valor simulado do intercepto
Intercepto <- 50
# Sigma do erro
sigma_erro <- 10
# Variavel com doses de N
Nitro <- rep(doses, each = n)
# Parte deterministica da MSPA
MSPA_determ <- Intercepto + N_efeito * Nitro
# Simulação dos residuos
residuos <- rnorm(n = n_obs, mean = 0, sd = sigma_erro)
# MSPA
mspa <- MSPA_determ + residuos
# Criar dataframe a partir dessas variaveis
dados <- data.frame(N = Nitro, MSPA = mspa)

# Analise exploratoria
theme_set(theme_bw())
ggplot(dados) +
  aes(x = N,
      y = MSPA) +
  geom_point() +
  geom_smooth(method = "lm")

# Modelagem
reg <- lm(MSPA ~ N, data = dados)

gvlma(reg)
plot(reg)
qqPlot(reg)


#### Simulando o efeito de um outlier
dados2 <- dados
dados2[55, 2] <- 1000
reg2 <- lm(MSPA ~ N, data = dados2)
plot(reg2)
#####

# Interpretacao
anova(reg)
summary(reg)
confint(reg)

y_pred <- predict(reg, se.fit = TRUE)
dados$Fit <- y_pred$fit
dados$SE <- y_pred$se.fit

ggplot(dados) +
  aes(x = N,
      y = Fit) +
  geom_line() +
  geom_point(aes(y = MSPA)) +
  geom_ribbon(aes(ymax = Fit + SE,
                  ymin = Fit - SE),
              alpha = .7) +
  labs(y = "MSPA")


confint(reg)
