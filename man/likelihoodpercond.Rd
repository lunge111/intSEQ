\name{likelihoodpercond}
\alias{likelihoodpercond}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The likelihood in each group.
}
\description{
Calculate the likelihood using dnbinom in stat package
}
\usage{
likelihoodpercond(theta, mu, ys,const=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
The dispersion parameter.
}
  \item{mu}{
The mean.
}
  \item{ys}{
The count data.
}
\item{const}{
A multiplying constant to make the product of likelihoods non-zero.
}
}

\value{
A numerical vector indicating likelihood.
}

\author{
Yilun Zhang
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{joint.like}}, \code{\link{joint.likelihood}}
}
\examples{
theta=1
mu=5
ys=rnbinom(10, mu=5, size=1)

likelihoodpercond(theta, mu, ys)
}

