\name{loglik4}
\alias{loglik4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
  Loglikelihood for Truncated Normal Distribution
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The loglikelihood for the four parameter model
}
\usage{
loglik4(parm, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parm}{
%%     ~~Describe \code{parm} here~~
  is a numeric vector of length 4
}
  \item{x}{
%%     ~~Describe \code{x} here~~
  is a numeric vector
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
 The four values in \code{parm} refers to \code{c(tmean, tsd, mean, sd)} for the \code{dtrunc4} function.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
  The loglikelihood of a truncated normal distribution for the four parameter model.
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
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~


% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
