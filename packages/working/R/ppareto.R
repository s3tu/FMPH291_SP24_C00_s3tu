ppareto <- function(q, alpha, beta, lower.tail = TRUE, log.p = FALSE){
  max_length <- max(length(q), length(alpha), length(beta))
  alpha <- rep(alpha, length = max_length)
  beta <- rep(beta, length = max_length)
  q <- rep(q, length = max_length)
  log_p <- suppressWarnings(ifelse(q<alpha, -Inf, beta*(log(alpha)-log(q))))
  log_p <- suppressWarnings(ifelse(alpha <=0 | beta <=0, NaN, log_f))
  if (any( is.nan( log_f )) == TRUE){
    warning("NaNs produced")
  }
  p <- log_p
  if(! log.p) p <- exp(log_f)
  if(lower.tail) p <- 1 - p
  if(log.p & (! lower.tail)) p <- log(1-exp(log_p))
  p
}