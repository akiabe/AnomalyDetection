library(car)
library(KernSmooth)

x <- Davis[, c("weight", "height")]
head(x, n = 3)

h <- c(dpik(x$weight), dpik(x$height))
est <- bkde2D(x, bandwidth = h, gridsize = c(10^3, 10^3))

d <- list(
  x = est$x1,
  y = est$x2,
  z = est$fhat
)

image(
  d,
  col = terrain.colors(7),
  xlim = c(35, 110),
  ylim = c(145, 200)
)

contour(d, add = TRUE)

n <- nrow(x)
K <- matrix(-1, n, n)
prefac <- (2 * pi * h)^(-0.5)

for (i in 1:n) {
  xnn <- x[i,]
  x_x1 <- x[,1] - as.numeric(xnn[1])
  K1 <- prefac[1] * exp(-0.5 * (1 / h[1])^2 * (x_x1 * x_x1))
  x_x2 <- x[,2] - as.numeric(xnn[2])
  K2 <- prefac[2] * exp(-0.5 * (1 / h[2])^2 * (x_x2 * x_x2))
  K[,i] <- K1 * K2
}

aa <- colSums(K) - diag(K)
lowerLim <- 10^(-20)
aa[(aa < lowerLim)] <- lowerLim
a <- (-1) * log(aa / (n - 1))

plot(a, xlab = "sample ID", ylab = "anomaly score")
