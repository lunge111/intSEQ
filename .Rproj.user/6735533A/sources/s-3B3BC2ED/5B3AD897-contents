\name{limma.trans.running}
\alias{limma.trans.running}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transformation method with limma. 
}
\description{
Implement limma trans. See detail for default options.
}
\usage{
limma.trans.running(count.data, cond, normalize= TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{count.data}{
A numeric matrix of the count table.
}
  \item{cond}{
A vector of factors or numeric of the predictor variable.
}
\item{normalize}{
Logical, whether to use "TMM" normalization.
}
}
\details{
The limma trans first transform the data with the function \eqn{log(y + sqrt(y^2 + y/\theta^2) + 1/(2 * \theta^2))}, then estimate the \eqn{\theta} such that the regression line of variance on started log of the mean has zero slope. 

For the transformed data, we use calcNormFactors in edgeR to calculate the normalization factors with method equals to "TMM". Then we rescale each column by the \eqn{10^6}{10^6} divided by the product of normalization factor and library size. It is a "TMM" version of count per million. Then we call lmFit  and eBayes functions in limma with default options.


}
\value{
 An object of class MArrayLM
}
\references{
Our first paper.
}
\author{
Yilun Zhang, David Rocke
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{tran1}},  \code{\link{tran.est}}, \code{\link{tran.reg}}
}
\examples{
data(count.data)
data(condition)
pv.trans <- limma.trans.running(count.data,condition)
}

