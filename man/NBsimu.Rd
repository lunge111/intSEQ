\name{NBsimu}
\alias{NBsimu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate synthetic RNA-seq data.
}
\description{
Simulate a count table follows negative binomial or Poisson distribution.
}
\usage{
NBsimu(mle1, null = F, nsamp = 3, lfc, sdd = 0.3631473, norm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mle1}{
A matrix of two columns. The mean (1st column) and dispersions(2nd column) for the baseline group.
}
  \item{nsamp}{
Number of samples per group.
}
  \item{lfc}{
The log2 fold change used to determine the mean of second group.
}
  \item{null}{
Logical, whether the second group is equal to the first.
}
      \item{sdd}{A scalar vector. The normalizing factor is assumed to be distributed as exp(Norm(0, sdd)). The default value is estimated from the Montgomery data.
  }

  \item{norm}{
Whether to generate artificial normalizing factors.
}
}
\details{
Given a matrix of parameters, we generate the first group of negative binomial distributed synthetic RNA-seq data with the mean equal to the first column of mle, and the dispersion equals to the second column of mle. The second group shares same dispersions but the mean is calculated so that the log2 fold change is equal to lfc. When the entries in the second group is smaller or equal to zero. A Poisson distributed data will be generated instead.
}
\value{

\item{cd}{ A numeric matrix of synthetic RNA-seq data}
\item{cond }{Vector or factors giving the experimental group/condition.}

}

\author{
Yilun Zhang, David Rocke
}

\seealso{
\code{\link{ran.poi}}, \code{\link{ran.nb}}, \code{\link{sample.nb}}

}

\examples{
mle = matrix(c(1:10,rep(1,10)), ncol=2)
lfc = rnorm(10, mean=0, sd = 0.5)
NBsimu(mle,lfc=lfc)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the

