#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double paretodens(double x, double alpha, double beta, bool logd = false)
{
  double log_f;
  if (alpha <= 0.0 || beta <= 0.0)
  {
    return NAN;
  }
  if(x<alpha)
  {
    return 0;
  }
  else{
    log_f = log(beta)+ beta*log(alpha) - (beta+1)*log(x);
  }
  if(logd == true)
  {
    return log_f;
  }
  else{
    return exp(log_f);
  }
}


// [[Rcpp::export(name = dpareto)]]
NumericVector dpareto(
    NumericVector x, NumericVector alpha, NumericVector beta, bool logd = false) {
  int xn = x.size();
  int alphan = alpha.size();
  int betan = beta.size();
  int n = xn;
  if (alphan > n) n = alphan;
  if (betan > n) n = betan;
  NumericVector result;
  result = NumericVector(n);
  int ix=0, ialpha=0, ibeta = 0;
  for (int i = 0; i<n; ++i) {
    result[i] = paretodens(x[ix], alpha[ialpha], beta[ibeta], logd=logd );
    // Update the indexes for the next pass:
    if (++ix == xn ) ix = 0 ;
    if (++ialpha == alphan ) ialpha = 0;
    if (++ibeta == betan ) ibeta = 0;
  }
  return result;
}
