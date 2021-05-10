library(mclust)
library(car)

X <- Davis[-12, c("weight", "height")]
head(X, n = 3)

result <- Mclust(X)

print(summary(result, parameters = TRUE))
plot(result)

pi <- result$parameters$pro
X <- Davis[, c("weight", "height")]
XX <- cdens(
  modelName = result$modelName,
  data = X,
  parameters = result$parameters
)
a <- -log(as.matrix(XX) %*% as.matrix(pi))
plot(a, ylab = "anomaly score")
