
#https://www.youtube.com/watch?v=03tx4v0i7MA

set.seed(16062020)

#rbinom(n = 1, size = 1, prob = .5)

#rbinom(n = 100, size = 1, prob = .5)

#rbinom(n = 100, size = 1, prob = .01)

#rbinom(n = 100, size = 1, prob = .1)

rbinom(n = 1, size = 100, prob = .5)

probs <- dbinom(x = 0:100, size = 100, prob = .5)
plot(probs, type = "h", ylim = c(0, .4))

probs <- dbinom(x = 0:100, size = 100, prob = .7)
lines(probs, type = "h", col = "red")

probs <- dbinom(x = 0:100, size = 100, prob = .9)
lines(probs, type = "h", col = "blue")


probs <- dbinom(x = 0:100, size = 100, prob = .99)
lines(probs, type = "h", col = "violet")


set.seed(16062020)
rpois(n = 1, lambda = 40)
rpois(n = 1, lambda = 40)
rpois(n = 1, lambda = 40)

rpois(n = 30, lambda = 40)
dpois(x = 50, lambda = 40)
range <- 0:60
probs <- dpois(x = range, lambda = 40)
plot(x = range, y = probs, type = "h", ylim = c(0, .3))

probs <- dpois(x = range, lambda = 20)
lines(x = range, y = probs, type = "h", col = "red")

probs <- dpois(x = range, lambda = 10)
lines(x = range, y = probs, type = "h", col = "blue")

probs <- dpois(x = range, lambda = .2)
lines(x = range, y = probs, type = "h", col = "violet")

probs <- dbinom(x = 0:100, size = 100, prob = .99)
lines(probs, type = "h", col = "violet")

set.seed(16062020)
rnorm(n = 1, mean = 150, sd = 20)

range <- 0:300
dens <- dnorm(x = range, mean = 50, sd = 20)
plot(x = range, y = dens, type = "l")

dens <- dnorm(x = range, mean = 100, sd = 20)
lines(x = range, y = dens, type = "l", col = "red")

dens <- dnorm(x = range, mean = 150, sd = 20)
lines(x = range, y = dens, type = "l", col = "blue")

