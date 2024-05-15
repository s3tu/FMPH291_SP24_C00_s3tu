qpareto <- function(p, alpha, beta, lower.tail = TRUE, log.p = FALSE){
  max_length <- max(length(p), length(alpha), length(beta))
  alpha <- rep(alpha, length = max_length)
  beta <- rep(beta, length = max_length)
  p <- rep(p, length = max_length)
  if(log.p) p = exp(p)
  if(lower.tail) p = 1 - p
  q <- suppressWarnings(alpha/(p^(1/beta)))
  q <- suppressWarnings(ifelse(alpha <=0 | beta <=0, NaN, q))
  if (any( is.nan( q )) == TRUE){
    warning("NaNs produced")
  }
  q
}