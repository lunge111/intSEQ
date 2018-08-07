\name{limma.voom.running}
\alias{limma.voom.running}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Implement voom with limma by default settings.
}
\description{Using voom to transform the data then pass the transformed data into limma pipeline.}
\usage{
limma.voom.running(count.data, cond, normalize= TRUE)
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
The default method of normalization is "TMM" using the calcNormFactors. Then \code{voom}, \code{lmFit} and \code{eBayes} in limma package are called sequentially with default options.
}
\value{
 An object of class MArrayLM
}
\references{
  Ritchie, M.E., Phipson, B., Wu, D., Hu, Y., Law, C.W., Shi, W., and Smyth, G.K. (2015). limma powers
  differential expression analyses for RNA-sequencing and microarray studies. Nucleic Acids Research 43(7),
  e47.
}
\author{
Yilun Zhang, David Rocke
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
data(count.data)
data(condition)
pv.voom <- limma.voom.running(count.data,condition)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

