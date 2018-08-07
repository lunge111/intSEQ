\name{tran1}
\alias{tran1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The transformation function of limma.trans.
}
\description{
The transformation function calculated by the delta method applied on the variance function of negative binomial model : \eqn{\mu} =  \eqn{\mu} + \eqn{\theta\mu^2}.
}
\usage{
tran1(y, c1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
The count data.
}
  \item{c1}{
The transformation parameter \eqn{\theta}.
}
}


\author{
Yilun Zhang, David Rocke
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{limma.trans.running}},  \code{\link{tran.est}}, \code{\link{tran.reg}}
}