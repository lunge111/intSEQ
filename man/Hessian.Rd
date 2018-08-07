\name{Hessian}
\alias{singleHessian }
\alias{hessian}
\alias{Hessian }
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hessian matrix with respect to the dispersion.
}
\description{
Lower level function to calculate the hessian matrix used in Gaussian-Hermite quadrature.
}
\usage{
singleHessian(y,mu, theta)
hessian(dat, theta)
Hessian(dat,theta, sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
The dispersion parameter.
}
  \item{y}{
A scalar of count data.
}
  \item{dat}{
A vector of count data.
}

  \item{mu}{
The mean of count data.}
  \item{sigma}{
The standard deviation of normal prior.
}

}

\author{
Yilun Zhang
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{marginal.LR}}}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
