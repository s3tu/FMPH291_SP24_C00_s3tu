## 1, 2, 3
tabll <- function(p.vec, eta.mat, X){
  # X is a N by C matrix of counts
  # p.vec is a vector of length M
  N <- nrow(X)
  C <- ncol(X)
  M <- length(p.vec)
  if(ncol(eta.mat) != C) warning("input don't have compatible dimensions")
  if(nrow(eta.mat) != M) warning("input don't have compatible dimensions")
  m.vec <- rowSums(X)
  ll <- 1
  for(i in 1:N){
    lli <- 0
    for(j in 1:M){
      lli <- lli + p.vec[j] * dmultinom(X[i,], prob = eta.mat[j,])
    }
    ll <- ll * lli # observed data likelihood here
  }
  log(ll) # output log-likelihood
}


## 4
# get proportion of row elements
prop.by.row <- function(X) sweep(X, 1, rowSums(X), "/")
ExZ.X <- function(p.vec, eta.mat, X){
  N <- nrow(X)
  C <- ncol(X)
  M <- length(p.vec)
  Ez <- matrix(nrow = N, ncol = M)
  for(i in 1:N){
    for(j in 1:M){
      Ez[i, j] <- p.vec[j] * dmultinom(X[i,], prob = eta.mat[j,])
    }
  }
  prop.by.row(Ez)
}

## 5
Mparms <- function(expect.Z, X){
  eta.mat <- prop.by.row(t(expect.Z) %*% X)
  p.vec <- colMeans(expect.Z)
  list(eta = eta.mat, p = p.vec)
}

## 10
updatell <- function(p.vec, eta.mat, X, n.iter = 10){
  # check if the input have compatible dimensions
  if(length(p.vec)!=dim(eta.mat)[1]|dim(eta.mat)[2]!=dim(X)[2])# M=M|C=C
  {warning("input don't have compatible dimensions")
    return(NULL)
  }
  for(it in 1:n.iter){
    expect.Z <- ExZ.X(p.vec, eta.mat, X)
    expect.M <- Mparms(expect.Z, X)
    p.vec <- expect.M$p
    eta.mat <- expect.M$eta
  }
  return(list(eta = eta.mat, p = p.vec, ll = tabll(p.vec, eta.mat, X)))
}
