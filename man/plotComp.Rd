\name{plotComp}
\alias{plotComp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the simulation comparison results.
}
\description{
Generate plots for the results of \code{\link{simuComp}}. The power or FPR of limma voom, limma trans, integrated likelihood and Quasi likelihood method are compared.
}
\usage{
plotComp(intcomp, ylim = c(1e-06, 1), text = "3 vs 3", pos=c(3e-6,0.9), ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{intcomp}{An object of class "intcomp" produced by \code{\link{simuComp}} function.
}



  
        \item{ylim}{ The ylim of the plot. Should be a numeric vector of length 2.
  
  }
  
  
        \item{text}{The text shown on the plot. 
  }
  \item{pos}{
  The position of the text on the plot. Should be a numeric vector of length 2.
  }
    \item{...}{
  Additional arguments passed to plot.  }
}
\details{
If the null is set to be true when simuComp is called, then the p-value thresholds are plotted against the empirical FPR, which is the ratio of rejections. Otherwise the empirical discover rate (average number of discoveries divided by the number of genes) is plotted against p-value thresholds. Four plots are shown corresponds to the small, medium, large difference genes and all genes.

}
\value{
A plot is created on the current graphics device.
}

\references{
Our paper
}
\author{
Yilun Zhang, David Rocke
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{simuComp}}
}
\examples{
\dontrun{
data(count.data)
data(condition)
count=count.data[,1:10]
cond=rep(0:1,each=5)
res=intSEQ(count, cond,nneighbour=400,lambda1=10,lambda2=0.05,meanmeth="local.mean"
         ,"loess",normalize=TRUE, coeff =c(2))
nsimu <- simuComp(res, ntime = 2, null = TRUE)
asimu <- simuComp(res, ntime = 2, null = FALSE)
plotComp(nsimu, text = " 3 vs 3 null simulation")
plotComp(asimu, text = " 3 vs 3 power simulation")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
