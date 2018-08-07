\name{ran.nb}
\alias{ran.nb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate negative binomial random samples. 
}
\description{
A simple function to generate negative binomial random samples using \code{\link{rnbinom}}.
}
\usage{
ran.nb(par, nsamp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par}{
A numeric vector of length 2. The first is the mean and the second is the dispersion parameter, which is the reciprocal of size in \code{\link{rnbinom}}.
}
  \item{nsamp}{
An integer indicating the sample size.
}
}

\value{
A numerical vector of negative binomial random sample.}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ran.poi}}, \code{\link{NBsimu}}, \code{\link{sample.nb}}
}
\examples{
par= c(5, 0.2)
ran.nb(par, 5)
}

