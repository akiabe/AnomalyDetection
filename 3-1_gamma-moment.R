library(car)
library(MASS)

data(Davis)
head(Davis, n = 3)

N <- length(Davis$weight)
N

mu <- mean(Davis$weight)
si <- sd(Davis$weight) * sqrt((N - 1) / N)
c(mu, si)

kmo <- (mu / si)^2
smo <- si^2 / mu
c(kmo, smo)

ml <- fitdistr(Davis$weight, "gamma")
ml

kml <- ml$estimate["shape"]
sml <- 1 / ml$estimate["rate"]
c(kml, sml)

a <- Davis$weight / smo - (kmo - 1) * log(Davis$weight/smo)
th <- order(a, decreasing = T)[0.01*N]

plot(a, ylab = "anomaly score")
lines(0:200, rep(a[th], length(0:200)), col = "red", lty = 2)
