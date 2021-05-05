library(car)

data(Davis)
head(Davis, n = 3)

X <- cbind(Davis$weight, Davis$height)
head(X, n = 3)

plot(X[,1], X[,2], pch = 16, xlab = "weight", ylab = "height")

mx <- colMeans(X)
Xc <- X - matrix(1, nrow(X), 1) %*% mx
Sx <- t(Xc) %*% Xc /
