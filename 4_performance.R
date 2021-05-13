score <- c(0.19, 0.86, 0.17, 0.12,
           0.04, 0.78, 0.16, 0.51, 
           0.57, 0.27)
anomaly <- c(F, T, F, F, F, T, F, T, T, F)
data0 <- cbind(score, anomaly)
rownames(data0) <- c("x1", "x2", "x3", "x4", "x5",
                     "x6", "x7", "x8", "x9", "x10")
data0

barplot(data0[,"score"], 
        ylim = c(0, 1),
        col = data0[,"anomaly"])

data1 <- data0[order(score, decreasing = TRUE),]
data1

score_sorted <- data1[,"score"]
score_sorted

anomaly_sorted <- data1[,"anomaly"]
anomaly_sorted

n_total <- length(anomaly)
n_anom <- sum(anomaly)
n_norm <- n_total - n_anom

coverage <- rep(0, n_total)
detection <- rep(1, n_total)

for (i in 1:n_total) {
  n_detectedAnom <- sum(anomaly_sorted[1:i])
  n_detectedNorm <- (n_total - i) - sum(anomaly_sorted[-(1:i)])
  coverage[i] <- n_detectedAnom / n_anom
  detection[i] <- n_detectedNorm / n_norm
}

plot(score_sorted, type = "l", coverage)
par(new = TRUE)
plot(score_sorted, type = "l", detection)

plot(1 - detection, coverage, type = "b")
