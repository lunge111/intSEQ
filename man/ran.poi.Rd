\name{ran.poi}
\alias{ran.poi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate Poisson random sample.
}
\description{
A simple function to generate Poisson random samples using \code{\link{rpois}}. 
}
\usage{
ran.poi(par, nsamp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par}{
A numeric vector of length 2. The first is the mean and the second is the dispersion parameter, which is the reciprocal of size in \code{\link{rnbinom}}. The second parameter will not be used in simulation.
}
  \item{nsamp}{
An integer indicating the sample size.
}
}

\value{
A numerical vector of Poisson random sample.}

\author{
Yilun Zhang, David Rocke
}

\seealso{
\code{\link{ran.nb}}, \code{\link{NBsimu}}, \code{\link{sample.nb}}
}
\examples{
par= c(5, 0.2)
ran.poi(par, 5)
}

