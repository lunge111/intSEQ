\name{tran.reg}
\alias{tran.reg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The regression part of limma trans.
}
\description{
Regress the third root of residual squared error on the mean.
}
\usage{
tran.reg(x, tmat, cond, square = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The transformation parameter.
}
  \item{tmat}{
The count data.
}
  \item{cond}{
The predictor variable indicating different groups.
}
  \item{square}{
Whether return a squared value of regression coefficient.
}
}
\value{
A numeric scalar of (squared) regression coefficient of the variance on the started log of mean.
}

\author{
Yilun Zhang, David Rocke
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{tran1}}, \code{\link{tran.est}}, \code{\link{limma.trans.running}}

}
\examples{
data(count.data)
data(condition)
tran.reg(0.3, count.data, condition, square = FALSE)
}

