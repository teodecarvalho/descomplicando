library(tidyverse)
library(gvlma)
library(car)
library(emmeans)
library(ggthemr)
#################    Anova em Fatorial Duplo   #################
set.seed(13082020) # Apenas para assegurar reprodutibilidade

cult_cal   <- expand.grid(Cultivar = paste0("Cult", LETTERS[1:5]), 
                          Calagem = c("Sem", "Com"),
                          Rep = 1:5)


mm_cult_cal <- model.matrix(~Cultivar * Calagem, data = cult_cal)

media_geral <- 60
# Coeficientes simulados
efeitos <- c(1   * media_geral, # Intercepto (média da cultivar A),
             -.1 * media_geral, # Diferença entre cultivar B e A
             0   * media_geral, # Diferença entre cultivar C e A
             .3  * media_geral, # Diferença entre cultivar D e A
             0   * media_geral, # Diferença entre cultivar E e A
             .3   * media_geral, # Efeito da calagem (cultivar A)
             .5   * media_geral, # Interação calagem cultivar B
             0   * media_geral, # Interação calagem cultivar C
             .1   * media_geral, # Interação calagem cultivar D
             0   * media_geral) # Interação calagem cultivar E

# Calculando o valor médio de MSPA para cada linha de cultivares
cult_cal$MSPA <- as.vector(mm_cult_cal %*% efeitos)
# Adicionando resíduo aos dados
desvio_erro <- 6
cult_cal$MSPA <- cult_cal$MSPA + rnorm(n = nrow(cult_cal), sd = desvio_erro)


ggplot(cult_cal) +
  aes(x = Cultivar,
      y = MSPA,
      colour = Calagem) +
  geom_point()


modelo <- lm(MSPA ~ Cultivar * Calagem, data = cult_cal)
plot(modelo)
gvlma(modelo)

anova(modelo)
summary(modelo)

emmeans(modelo, ~Cultivar|Calagem) %>%
  cld(method = "tukey", Letters = letters)

emmeans(modelo, ~Calagem|Cultivar) %>%
  cld(method = "tukey", Letters = letters)
m1 <- modelo

c1 <- emmeans(modelo, ~Cultivar|Calagem) %>%
  cld(method = "tukey", Letters = LETTERS, reversed = TRUE, sort = TRUE) %>%
  as.data.frame() %>%
  mutate(letras_c1 = str_trim(.group))

c2 <- emmeans(modelo, ~Calagem|Cultivar) %>%
  cld(method = "tukey", Letters = letters, reversed = TRUE, sort = TRUE) %>%
  as.data.frame() %>%
  mutate(letras_c2 = str_trim(.group)) %>%
  select(Calagem, Cultivar, letras_c2)

medias <- c1 %>%
  left_join(c2, by = c("Calagem", "Cultivar")) %>%
  mutate(Letras = str_c(letras_c1, letras_c2)) %>%
  rename(MSPA = emmean,
         UL = upper.CL,
         LL = lower.CL)

dodge = position_dodge(width = 1)
ggthemr("dust", layout = "clean")
ggplot(medias) +
  aes(x = Cultivar, 
      y = MSPA,
      fill = Calagem) +
  geom_col(position = dodge) +
  geom_text(aes(label = Letras,
                y = UL),
            vjust = -1,
            position = dodge) +
  geom_blank(aes(y = UL * 1.1)) +
  geom_errorbar(aes(ymax = UL,
                    ymin = LL),
                size = .3,
                position = dodge,
                width = .5,
                colour = "black") +
  scale_y_continuous(expand = c(0, 0))

