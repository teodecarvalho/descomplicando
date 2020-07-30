library(tidyverse)
library(gvlma)
library(emmeans)
library(car)

ggplot(iris) +
  aes(x = Species,
      y = Sepal.Width) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(width = .3)

modelo <- lm(Sepal.Width ~ Species, data = iris)
gvlma(modelo)

plot(modelo)

anova(modelo)
summary(modelo)
model.matrix(modelo)
confint(modelo)

medias <- emmeans(modelo, ~Species) %>%
  CLD(method = "tukey", Letters = letters) %>%
  as.data.frame() %>%
  mutate(letras = str_trim(.group),
         Species = paste("Iris", Species))

theme_set(theme_classic(14))
ggplot(medias) +
  aes(x = Species,
      y = emmean) +
  geom_col(fill = "white",
           col = "black",
           width = .5,
           size = .3) +
  geom_blank(aes(ymax = upper.CL * 1.1)) +
  geom_errorbar(aes(ymax = upper.CL,
                    ymin = lower.CL),
                width = .2,
                size = .3) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_text(aes(label = letras,
                y = upper.CL),
            vjust = -1) +
  labs(y = "Sepal Width (cm)") +
  theme(axis.text.x = element_text(face = "italic"))



