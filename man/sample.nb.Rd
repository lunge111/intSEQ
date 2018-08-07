\name{sample.nb}
\alias{sample.nb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate negative binomial count table random sample. 
}
\description{
Simulate a count table follows negative binomial or Poisson distribution with same mean.
}
\usage{
sample.nb(mom, nsamp = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mom}{
A matrix of two columns. The mean (1st column) and dispersions(2nd column).
}
  \item{nsamp}{
Number of samples.
}
}
\details{
This function is used to simulate Negative Binomial/Poisson sample of one group in \code{\link{NBsimu}}. See \code{\link{NBsimu}} for detail.
}
\value{
A numeric matrix of synthetic RNA-seq data
}

\author{
Yilun Zhang, David Rocke
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{ran.nb}}, \code{\link{NBsimu}}, \code{\link{ran.poi}}
}
\examples{
m = matrix(c(1:10,rep(1,10)), ncol=2)
sample.nb(m, 5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
