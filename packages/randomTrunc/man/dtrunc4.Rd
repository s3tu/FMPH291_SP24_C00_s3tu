\name{dtrunc4}
\alias{dtrunc4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
    Compute the values for the probability density of a trancated value 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
    This function computes values for the probability density function of a trancated random value. The trancated random value follows a Normal distribution. 
}
\usage{
dtrunc4(x, tmean, tsd, mean = 0, sd = 1, log = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
  is a vector of values
}
  \item{tmean}{
%%     ~~Describe \code{tmean} here~~
  is the mean of the truncation threshold variable
}
  \item{tsd}{
%%     ~~Describe \code{tsd} here~~
  is the standard deviation of the truncation threshold variable
}
  \item{mean}{
%%     ~~Describe \code{mean} here~~
  is the mean of the untruncated distribution
}
  \item{sd}{
%%     ~~Describe \code{sd} here~~
  is the standard deviation of the untruncated distribution
}
  \item{log}{
%%     ~~Describe \code{log} here~~
  indicates whether to return the logarithm of the density
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
  This function can be used for a trucation value that has a Normal distribution with its own mean and standard deviation. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
    \code{dtrunc4} gives the density for with a truaction value that has a Normal distbution. It could be potentially used for estimating the mean and standard deviation of untracated distribution. 
}
\references{
%% ~put references to the literature/web site here ~
http://parker.ad.siu.edu/Olive/ch4.pdf

\href{https://homepage.divms.uiowa.edu/~luke/classes/STAT7400/}{Luke Tierney STAT7400 course}
}
\author{
%%  ~~who you are~~
Shengjia Tu
}
\note{
%%  ~~further notes~~
It is assumed without truncation the distribution of the data would have been Normal. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
dtrunc4( c(1:10), 1.5, 0.1, 2.0, 1.0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
