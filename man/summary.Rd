\name{summary.intcomp}
\alias{summary}
\alias{summary.intcomp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary of comparison results.
}
\description{
Show the simulation results of \code{\link{simuComp}} for limma voom, limma trans, integrated likelihood and Quasi likelihood method.
}
\usage{
\S3method{summary}{intcomp}(intcomp, difflevel = c("all", "small", "medium", "large"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{intcomp}{
A object returned by \code{\link{simuComp}}.
}
  \item{difflevel}{
Which subgroup of genes is shown.
}
  \item{...}{
Other arguments that are currently not used.
}
}
\details{
If the intcomp was produced under null = TRUE. A table of empirical false positive rate (the number of rejections divided by the number of outcomes, since the simulations are all under null condition, all rejections are false positives) will be displayed.

If the intcomp was produced under null = FALSE. First, the discovery rates (the number of rejections divided by the number of outcomes) under fdrlevel in \code{\link{simuComp}} will be presented for the four methods. Then a table of discovery rate under p values thresholds defined by levels in \code{\link{simuComp}} will be displayed.
}


\author{
Yilun Zhang, David Rocke
}
\examples{
\dontrun{
data(count.data)
data(condition)
count=count.data[,1:10]
cond=rep(0:1,each=5)
res=intSEQ(count, cond)
simu.res <- simuComp(res, ntime = 2)
summary(simu.res, difflevel="all")
summary(simu.res, difflevel="small")
summary(simu.res, difflevel="medium")
summary(simu.res, difflevel="large")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the

