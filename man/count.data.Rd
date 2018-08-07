\name{count.data}
\alias{count.data}
\docType{data}
\title{
The count table of Montgomery and Pickrell data.
}
\description{
 RNA-seq count data from lymphoblastoid cell lines from 60
unrelated Caucasian individuals of European descent and 69
unrelated Nigerian individuals. 
}
\usage{data("count.data")}

\details{
The data was obtained in tweeDEseqCountData package. We filtered the data such that each row has average count at least 1. There are 8124 genes passed the filter, 129 columns represent 60 CEU and 69 YRI individuals.
}
\source{
https://bioconductor.org/packages/release/bioc/html/tweeDEseq.html
}
\references{

  Juan R Gonzalez and Mikel Esnaola (2016). tweeDEseqCountData: RNA-seq count data
  employed in the vignette of the tweeDEseq package. R package version 1.12.0.


}

\seealso{
\code{\link{condition}}
}
\examples{
data(count.data)
## maybe str(count.data) ; plot(count.data) ...
}
\keyword{datasets}
