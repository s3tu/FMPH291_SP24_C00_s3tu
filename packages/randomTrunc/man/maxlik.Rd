\name{maxlik}
\alias{maxlik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
  Maximization of Loglikehood
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
  Maximize the loglikelihood for either model
}
\usage{
maxlik(x, model = c("three", "four"), parm = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
  is a numeric vector
}
  \item{model}{
%%     ~~Describe \code{model} here~~
  is a character string that specifies which model to use. The default value is "three".
}
  \item{parm}{
%%     ~~Describe \code{parm} here~~
  is an optional numeric vector of starting values for the parameters
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
  are any extra parameters that might be passed to the optimizer
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
  In the paticular case that \code{model} is \code{"three"} but the \code{parm} vector is in length of 4, the function will use \code{parm[-2]} as \code{parm}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
  A list generated by \code{optim} function.
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


\examples{
x <- rtrunc4(10000, 1.5, 1e-6, 2.0, 1.2)
maxlik(x, model = "three", parm = c(1.0, 1.0, 1.0))
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
