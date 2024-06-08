rtrunc4 <- function(n, tmean, tsd, mean=0.0, sigma=1.0){
  xx <- c()# create a blank vector
  while (length(xx)<n) {# sampling until the length reach n
    a <- rnorm(n, tmean, tsd)
    x <- rnorm(n, mean, sigma)
    xx <- c(xx, na.omit(ifelse(a < x, x, NA)))# compare each element of a and x, 
    # if a<x, keep x, or use NA instead of x
    # add new truncated data after the existing valid data xx, excluding NA
  }
  xx[1:n] # output
}

#f3(x)
dtrunc3 <- function(x, a, mean=0.0, sd=1.0, log = FALSE){
    # den <- ifelse(a < x, dnorm(x, mean, sd), 0) / integrate(dnorm, a, Inf, mean = mean, sd = sd)$value# pnorm()?
  den <- ifelse(a < x, dnorm(x, mean, sd), 0) / (1-pnorm(a,mean = mean, sd = sd))
  if(log) return(log(den))# treat with parameter 'log'
  return(den)
}

#f4(x)
dtrunc4 <- function(x, tmean, tsd, mean=0.0, sd=1.0, log = FALSE){
  #unnormalized
  den <- pnorm(x, tmean, tsd) * dnorm(x, mean, sd) / integrate(dnorm, -Inf, 0, mean = tmean - mean, sd = (tsd^2 + sd^2)^(1/2))$value
  # den <- pnorm(x, tmean, tsd) * dnorm(x, mean, sd) / pnorm(0, mean = tmean - mean, sd = (tsd^2 + sd^2)^(1/2))
  #normalized
  # den <- pnorm(x, tmean, tsd) * dnorm(x, mean, sd) / pnorm((mean-tmean)/sqrt(tsd^2 + sd^2))
  if(log) return(log(den))
  return(den)
}

loglik4 <- function(parm, x){
  sum(dtrunc4(x, parm[1], parm[2], parm[3], parm[4], log = T))
}

loglik3 <- function(parm, x){
  sum(dtrunc3(x, parm[1], parm[2], parm[3], log = T))
}

loglik4.neg <- function(parm, x){-loglik4(parm, x)}
loglik3.neg <- function(parm, x){-loglik3(parm, x)}

maxlik <- function(x, model = c("three","four"), parm = NULL, ...){
  model <- match.arg(model)
  parm3 <- (if(length(parm) == 4) parm[c(1, 3, 4)] else parm)
  switch(model, three = optim(par = parm3, fn = loglik3.neg, x = x), four = optim(par = parm, fn = loglik4.neg, x = x))
}
