dparetoR = function(x, alpha, beta, log = FALSE)
{
  #We carry out recycling
  max_length <- max(length(x), length(alpha), length(beta))
  alpha <- rep(alpha, length = max_length)
  beta <- rep(beta, length = max_length)
  x <- rep(x, length = max_length)
  #We calculate log-likelihood
  log_f <- suppressWarnings(ifelse(x<alpha, -Inf, log(beta)+ beta*log(alpha) - (beta+1)*log(x)))
  #If alpha<=0 or beta<=0, the log-likelihood is NaN
  log_f <- suppressWarnings(ifelse(alpha <=0 | beta <=0, NaN, log_f))
  if (any( is.nan( log_f )) == TRUE){
    warning("NaNs produced")
  }
  #We return either the log density or the density depending on what the user specifier
  if(log == TRUE)
  {
    return(log_f)
  }
  else{
    return(exp(log_f))
  }
}