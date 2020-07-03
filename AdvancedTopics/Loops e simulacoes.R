set.seed(3072020)

y1 <- rnorm(5, mean = 10, sd = 1)
y2 <- rnorm(5, mean = 11, sd = 1)

dados <- data.frame(y = c(y1, y2),
                    grupos = rep(c("A", "B"), each = 5))

modelo <- lm(y ~ grupos, data = dados)
summary(modelo)


for(i in 1:10){
  print(paste0("Nesta iteração o valor de i é:" , i))
}

set.seed(3072020)

nsims <- 1000
nsamples <- 20
p <- rep(NA, nsims)
for(i in 1:nsims){
  y1 <- rnorm(nsamples, mean = 10, sd = 1)
  y2 <- rnorm(nsamples, mean = 11, sd = 1)
  
  dados <- data.frame(y = c(y1, y2),
                      grupos = rep(c("A", "B"), each = nsamples))
  
  modelo <- lm(y ~ grupos, data = dados)
  sumario <- summary(modelo)
  p[i] <- sumario$coefficients[2, 4]
}

100 * sum(p <= 0.05) / nsims
