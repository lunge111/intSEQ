\name{show}
\alias{show}
\alias{show.intres}

\title{
Show the result of \code{\link{intSEQ}}
}
\description{
Print result of \code{\link{intSEQ}}
}
\usage{
\S3method{show}{intres}(intres, sortby = c("pvalue", "LFC", "no"), shownum = 20, ...)
}
\arguments{
  \item{intres}{
An object returned by \link{intSEQ}.
}
  \item{sortby}{
Options of sorting the result. The default is "pvalue". See detail.}
  \item{shownum}{
The number of genes want to display.
}
  \item{...}{
Other arguments that are currently not used.
}
}

\details{
This function displays the result of \code{\link{intSEQ}}. The number of genes is specified by "shownum". When "sortby" equals to "pvalue", it means display genes with smallest p-value. "LFC" means display genes with largest log 2 fold change.
}



\author{
Yilun Zhang, David Rocke}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{intSEQ}}
}
\examples{
data(count.data)
data(condition)
count=count.data[,1:10]
cond=rep(0:1,each=5)
res=intSEQ(count, cond)
show(res)
}

