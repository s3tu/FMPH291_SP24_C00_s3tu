\name{paretoDistr}
\alias{paretoDistr}
\alias{dpareto}
\alias{ppareto}
\alias{qpareto}
\alias{rpareto}
%\concept{incomplete pareto function}
\title{The pareto Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the pareto distribution with parameters \code{alpha} and
  \code{beta}.
}
\usage{
dpareto(x, alpha, beta, log = FALSE)
ppareto(q, alpha, beta, lower.tail = TRUE, log.p = FALSE)
qpareto(p, alpha, beta, lower.tail = TRUE, log.p = FALSE)
rpareto(n, alpha, beta)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{alpha, beta}{shape and scale parameters.  Must be positive strictly.}
  \item{log, log.p}{logical; if \code{TRUE}, probabilities/densities \eqn{p}
    are returned as \eqn{log(p)}.}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}
}
\value{
  \code{dpareto} gives the density,
  \code{ppareto} gives the distribution function,
  \code{qpareto} gives the quantile function.
  \code{rpareto} generates random deviates.

  Invalid arguments will result in return value \code{NaN}, with a warning.

  The length of the result is determined by the maximum of the lengths of the
  numerical arguments for the other functions.

  The numerical arguments other than \code{n} are recycled to the length of the result.  Only the first elements of the logical arguments are used.
}
\details{
  The density of Pareto Distribution is given by: \deqn{f(x|\alpha,\beta) = \frac{\beta{\alpha}^{\beta}}{x^{\beta+1}}}
for \eqn{0<\alpha\le x} and \eqn{\beta>0}. Note that densities are defined on the entire real line; they are zero outside of the support of the distribution.
}
% Have caught all currently known problems; hence no longer say:
%   Similarly, \code{qpareto} has a very hard job for
%   small \code{scale}, and warns of potential unreliability for
%   \code{scale < 1e-10}.
\examples{
dpareto(2:4, 1, 2)
ppareto(2:4, 1, 2)
qpareto(0.5, 1, 2)
rpareto(10, 1, 2)
}
\keyword{distribution}