library(kernlab)

set.seed(1)

x <- rbind(
  matrix(rnorm(120), ncol=2),
  matrix(rnorm(120, mean=3), ncol=2))
x <- scale(x)

rbf <- rbfdot(sigma = 0.5)
ocsvm <- ksvm(x, type = "one-svc", kernel = rbf, nu = 0.1)

colorcode<- rep(0, nrow(x))
colorcode[ocsvm@alphaindex] <- 1
plot(x, pch = 21, bg = colorcode)

getR2s.ocsvm  <- function(x_selected, ocsvm, knl,CC=1){
  
  idx.onSphere <- which(ocsvm@alpha != CC)
  R2s <- rep(-1, length(idx.onSphere))
  
  # Computing R2
  XX <- ocsvm@xmatrix
  iid <- 1
  for(idx in idx.onSphere){
    x.onSphere <- ocsvm@xmatrix[idx,]
    R2s[iid] <- getR2.ocsvm(x.onSphere, ocsvm, knl,CC)
    iid <- iid + 1 
  }
  return(R2s)
}
getR2.ocsvm <- function(x_selected, ocsvm, knl,CC=1){
  XX <- ocsvm@xmatrix
  Knn <- kernelMatrix(knl,t(x_selected),t(x_selected)) # must be 1 for rbf kernel
  Ka <- kernelMult(knl,x=XX,y=XX,z=ocsvm@alpha)
  aKa <- ocsvm@alpha %*% Ka 
  
  K <- kernelMatrix(knl,x=XX,y=t(x_selected)) # column vector
  aK <- ocsvm@alpha %*% K
  return(Knn + aKa -2.* aK)
}

# Select on-sphere samples to check the value of R
R2s <- getR2s.ocsvm(x_selected, ocsvm, rbf)

# Drawing contor
N1 <- 100
N2 <- 100
x1min <- min(ocsvm@xmatrix[,1])
x1max <- max(ocsvm@xmatrix[,1])
x2min <- min(ocsvm@xmatrix[,2])
x2max <- max(ocsvm@xmatrix[,2])
x1s <- seq(x1min, x1max, length = N1)
x2s <- seq(x2min, x2max, length = N2)
zz <- matrix(0,nr=N2,nc=N1)

for (ind in 1:N1) {
  for (ind2 in 1:N2) {
    xin <- c(x1s[ind], x2s[ind2])
    zz[ind, ind2] <- getR2.ocsvm(xin, ocsvm, rbf) - mean(R2s)
  }
}

d <- list(x = x1s, y = x2s, z = zz)

image(d, col = terrain.colors(9))
contour(d, add = TRUE)
points(x, pch = 21, bg = colorcode, cex = 1.5)
