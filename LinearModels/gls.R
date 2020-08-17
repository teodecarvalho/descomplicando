library(tidyverse)
library(gvlma)
library(broom)
library(nlme)
library(car)
library(emmeans)

iris

ggplot(iris) +
  aes(x = Species,
      y = Petal.Length) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(width = .3)
m1 <- lm(Petal.Length ~ Species, data = iris)
plot(m1)
gvlma(m1)

diagnosticos <- augment(m1)
ggplot(diagnosticos) +
  aes(x = Species, 
      y = .std.resid) +
  geom_jitter()

m2 <- lm(log(Petal.Length) ~ Species, data = iris)
plot(m2)

diagnosticos <- augment(m2)
ggplot(diagnosticos) +
  aes(x = Species, 
      y = .std.resid) +
  geom_jitter()

qqPlot(m2)
gvlma(m2)
anova(m2)
emmeans(m2, ~Species, type = "response") %>%
  cld(method = "tukey", Letters = letters)

ggplot(iris) +
  aes(x = Species,
      y = Petal.Length) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(width = .3)

# Generalized least squares
# Mínimos quadrados generalizado
m3 <- gls(Petal.Length ~ Species, data = iris,
    weights = varIdent(form = ~1|Species))

m4 <- gls(Petal.Length ~ Species, data = iris)

anova(m3, m4)


plot(m3)
iris$Residuos <- residuals(m3, type = "normalized")
ggplot(iris) +
  aes(x = Species,
      y = Residuos) +
  geom_jitter()

qqPlot(iris$Residuos)

anova(m3)
summary(m3)

emmeans(m3, ~Species, type = "response") %>%
  cld(method = "tukey", Letters = letters)


#################    Exemplo 4 - Anova de Covariância   #################
set.seed(1582020) # Apenas para assegurar reprodutibilidade
cult_N   <- expand.grid(Cultivar = paste0("Cult", LETTERS[1:5]), 
                        N = c(0, 30, 60, 90, 120),
                        Rep = 1:5)


mm_cult_N <- model.matrix(~Cultivar * N, data = cult_N)

media_geral <- 60
# Coeficientes simulados
efeitos <- c(1   * media_geral, # Intercepto (média da cultivar A),
             -.3 * media_geral, # Diferença entre cultivar B e A
             0   * media_geral, # Diferença entre cultivar C e A
             .3  * media_geral, # Diferença entre cultivar D e A
             0   * media_geral, # Diferença entre cultivar E e A
             .003   * media_geral, # Efeito do N (cultivar A)
             .009   * media_geral, # Interação N cultivar B
             0   * media_geral, # Interação N cultivar C
             .001   * media_geral, # Interação N cultivar D
             0   * media_geral) # Interação N cultivar E

# Calculando o valor médio de MSPA para cada linha de cultivares
cult_N$MSPA <- as.vector(mm_cult_N %*% efeitos)

# Adicionando resíduo aos dados
cult_N <- cult_N %>%
  mutate(SD = ifelse(Cultivar %in% c("CultB", "CultA"), 30, 6),
         Erros = rnorm(n = n(), sd = SD),
         MSPA = MSPA + Erros)


ggplot(cult_N) +
  aes(x = N,
      y = MSPA,
      colour = Cultivar) +
  facet_wrap(~Cultivar) +
  geom_smooth(method = "lm") +
  geom_point()

m0 <- lm(MSPA ~ Cultivar * N, data = cult_N)
plot(m0)

diagnosticos <- augment(m0)
diagnosticos

ggplot(diagnosticos) +
  aes(x = .fitted,
      y = .std.resid,
      colour = Cultivar) +
  facet_wrap(~Cultivar) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed")

gvlma(m0)

m1 <- lm(log(MSPA) ~ Cultivar * N, data = cult_N)
diagnosticos <- augment(m1)
diagnosticos

ggplot(diagnosticos) +
  aes(x = .fitted,
      y = .std.resid,
      colour = Cultivar) +
  facet_wrap(~Cultivar) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed")

gvlma(m1)

m2 <- gls(MSPA ~ Cultivar * N, data = cult_N,
    weights = varIdent(form = ~1|Cultivar))

m3 <- gls(MSPA ~ Cultivar * N, data = cult_N)

anova(m2, m3)

cult_N$Residuos <- residuals(m2, type = "normalized")
cult_N$Fitted <- fitted(m2)

ggplot(cult_N) +
  aes(x = Fitted,
      y = Residuos,
      colour = Cultivar) +
  facet_wrap(~Cultivar) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed")

summary(m2)

qqPlot(cult_N$Residuos)



