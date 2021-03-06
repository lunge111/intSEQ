\name{joint.like}
\alias{joint.like}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the joint likelihood
}
\description{
A small function to calculate the joint likelihood.
}
\usage{
joint.like(theta, ys, m0, ma, null = FALSE, const=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
The dispersion parameter.
}
  \item{ys}{
The count data
}

  \item{m0}{
The means under the null.
}
  \item{ma}{
The means under alternative.
}
  \item{null}{
Whether to calculate under null model or full model.
}
  \item{const}{
A multiplying constant to make the product of likelihoods non-zero.
}
}
\details{
Calculate the likelihood using dnbinom() in stat package
}

\value{
A numeric scalar indicating product of likelihood.
}

\author{
Yilun Zhang
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{joint.likelihood}}, \code{\link{likelihoodpercond}}
}
\examples{
theta=1
ys=rnbinom(10, mu=5, size=1)

cond = rep(0:1, each=5)
mu0 = rep(5, 10)
mua = rep(c(4, 6) , each = 5)

joint.like(theta, ys, cond,mu0, mua, null = TRUE)
joint.like(theta, ys, cond,mu0, mua, null = FALSE)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
