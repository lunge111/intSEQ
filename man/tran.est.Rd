\name{tran.est}
\alias{tran.est}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate the coefficient in limma trans.
}
\description{
Find the minimizer of squared slope coefficient.
}
\usage{
tran.est(count.data, cond)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{count.data}{
A numeric matrix of the count table.
}
  \item{cond}{
A vector of factors or numeric of the predictor variable.
}
}

\value{
The optimal value of transformation parameter.
}
\references{
The first paper.
}
\author{
Yilun Zhang, David Rocke
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\seealso{
 \code{\link{tran1}},  \code{\link{tran.reg}}, \code{\link{limma.trans.running}}

}
