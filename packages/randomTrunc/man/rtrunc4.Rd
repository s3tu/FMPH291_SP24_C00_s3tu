\name{rtrunc4}
\alias{rtrunc4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
    Generate a random vector following a Truacted Normal distribution. 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
    This function generates a random number following a Truacted Normal distribution by a normally distributed threshold with known mean and standard deviation. 
}
\usage{
rtrunc4(n, tmean, tsd, mean = 0, sigma = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
  gives the number of values to generate
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
  \item{sigma}{
%%     ~~Describe \code{sigma} here~~
  is the standard deviation of the untruncated distribution
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
When \code{tsd} is very small, it returns a random vector following the Truncated Normal distribution with a trunctation point at \code{tmean}. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A vector of random numbers from a truncated distribution.
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
rtrunc4( 10000, 1.5, 1e-6, 2.0, 1.0)
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
