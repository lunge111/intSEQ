\name{mednorm}
\alias{mednorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Median normalization method for limma trans.
}
\description{
Rescale each column of the count matrix with the median of that column.
}
\usage{
mednorm(mat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
The count table.
}
}

\value{
A rescaled matrix with same dimension as mat.
}
\references{
 Cloonan N, Forrest AR, Kolle G, Gardiner BB, Faulkner GJ, Brown MK, Taylor DF, Steptoe AL, Wani S, Bethel G, Robertson AJ, Perkins AC, Bruce SJ, Lee CC, Ranade SS, Peckham HE, Manning JM, McKernan KJ, Grimmond SM: Stem cell transcriptome profiling via massive-scale mRNA sequencing. Nat Methods. 2008, 5: 613-619. 10.1038/nmeth.1223.
}
\author{
Yilun Zhang, David Rocke.
}

\examples{
data(count.data)
mednorm(count.data)
}

