\name{edgeR.QL.running}
\alias{edgeR.QL.running}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Implement Quasi likelihood method by default settings.
}
\description{
See detail for default options.
}
\usage{
edgeR.QL.running(count.data, cond, normalize= TRUE)
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
The default method of normalization is "TMM" using the calcNormFactors in edgeR. Estimation of dispersion parameters uses estimateDisp function with default options. Then glmQLFit and glmQLFTest are called with defaults.
}
\value{
 An object of class DGEGLM.
}
\references{
McCarthy DJ, Chen Y and Smyth GK (2012). Differential expression analysis of multifactor RNA-Seq
  experiments with respect to biological variation. Nucleic Acids Research 40, 4288-4297
}
\author{
Yilun Zhang, David Rocke
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
data(count.data)
data(condition)
pv.quasi <- edgeR.QL.running(count.data,condition)
}

