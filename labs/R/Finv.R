## From Luke Tierney's notes on Computer Arithmetic
## from  his STAT7400 class

Finv0 <- function(u, a) {
  p <- pnorm(a) # CDF
  qnorm(p + u * (1 - p)) # given CDF, output quantiles
}

## Some plots:
u <- (1:100) / 101
plot(u, Finv0(u, 0), type = "l")
plot(u, Finv0(u, 2), type = "l")
plot(u, Finv0(u, 4), type = "l")
plot(u, Finv0(u, 8), type = "l")

## An improved version:
Finv1 <- function(u, a) {
  q <- pnorm(a, lower.tail = FALSE) # 1-CDF, q=1-p
  qnorm(q * (1 - u), lower.tail = FALSE) # this one is negative to that lower.tail=FALSE(default)
}

lines(u, Finv1(u, 8), col = "red") 
# copy and run
Finv1(0.5, 100)
# the result is Inf


# write function Finv2
Finv2 <- function(u, a) 
  {
  q <- pnorm(a, lower.tail = FALSE, log.p = TRUE)# here use log,e.g.,log_p=pnorm(100, lower.tail = FALSE, log.p = TRUE)=-5005.524
  # as pnorm() and qnorm() are inverse functions of each other,q*(1-u)=exp(log(q))*exp(log(1-u))=exp(log(q)+log(1-u))
  # so the final result should be exp(log(q)+log(1-u))
  return(paste("exp(", q+log(1-u)))
  
}
#try it
Finv2(0.5,100)# result is "exp( -5006.21735587477"