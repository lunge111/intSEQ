\name{pcut}
\alias{pcut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the proportion of rejections for given levels.
}
\description{
The proportion of rejections is the number of all rejections divided by the number of all p values.
}
\usage{
pcut(p, levels = c(1e-06, 1e-05, 1e-04, 0.001, 0.01, 0.05, 0.1))
}

\arguments{
  \item{p}{
Numeric matrix or vector of p values.
}
  \item{levels}{
A numeric vector of p value thresholds.
}
}

\value{
A numeric vector of proportion of rejections with the same length of levels
}
\author{
Yilun Zhang, David Rocke}


\examples{
pp =runif(10000)
pcut(pp)
}
