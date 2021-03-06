\name{simuComp}
\alias{simuComp1}
\alias{simuComp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulation for the four methods and compare the p value.
}
\description{
Simulate a count table follows negative binomial or Poisson distribution. Implement limma voom, limma trans, intSEQ and Quasi likelihood method with simulated data. 
}
\usage{
simuComp1(mle, nsamp = 3, lfc, null = F, lambda1 = 2 * nsamp, lambda2 = 0.05, 
normalize = TRUE,  sdd =0.3631473,constadj, w1=max(1-2*nsamp/100, 0)
         ,w2=max(1-2*nsamp/1000, 0))

simuComp(intres,  nsamp =3, nullgroup = NULL, 
         ntime=100,null=F, lambda1=2*nsamp, lambda2=0.05, normalize=FALSE, 
         levels=c(1e-6,1e-5,1e-4,1e-3,0.01,0.05,0.1), fdrlevel=0.1 , 
         small=NULL, medium=NULL,large=NULL, sdd=0.3631473,constadj=FALSE, 
         w1=max(1-2*nsamp/100, 0), w2=max(1-2*nsamp/1000, 0))
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mle}{
A matrix of two columns. The means (1st column) and dispersions(2nd column) for the baseline group.
}
  \item{intres}{
An object returned by \code{\link{intSEQ}}.
}
  \item{nullgroup}{
A logical vector of length of the number of columns of count.data, indicating which objects are baseline group.}

\item{ntime}{
The number of simulations to be conducted.
}

\item{fdrlevel}{
The fdr level.
}

  \item{nsamp}{
Number of samples per group
}
  \item{lfc}{
The log2 fold change used to determine the mean of second group.
}
  \item{null}{
Logical, whether the second group is equal to the first.
}
  \item{lambda1}{
The first parameter for the prior variance
}
  \item{lambda2}{
The second parameter for the prior variance
}
  \item{normalize}{
Whether to normalize the data.
}

  \item{levels}{
The p-value thresholds. Should be a numeric vector.}
  \item{small}{ A logical vector of the length of number of genes, indicating which genes has low between group difference. Those genes with fold change (larger divided by small) smaller than 1.5 are selected without specifying.
  
  }
  
    \item{medium}{ A logical vector of the length of number of genes, indicating which genes has medium between group difference. Those genes with fold change (larger divided by small) smaller than 2 and larger than 1.5 are selected without specifying.
  
  }
  
      \item{large}{ A logical vector of the length of number of genes, indicating which genes has large between group difference. Those genes with fold change (larger divided by small) larger than 2 are selected without specifying.
  
  }
      \item{sdd}{A scalar vector. The normalizing factor is assumed to be distributed as exp(Norm(0, sdd)). The default value is estimated from the Montgomery data.
  }
\item{constadj}{
Whether a constant should be multiplied to adjust the underflow problem of joint distribution. 
}
\item{w1}{
See Details in \code{\link{intSEQ}}.
}
\item{w2}{
See Details in \code{\link{intSEQ}}.
}
}
\details{
With the result of intSEQ, simulate two groups of synthetic RNA-seq that has means and dispersion equals to estimated value of the count data passed to intSEQ. Then compare the performance of four methods: limma voom, limma trans, intSEQ and Quasi likelihood method in edgeR. If null is set to be true, the ability of controlling the false positive rate is examined. Otherwise, the for methods will be tested by their power performance for given levels or fdr threshold.
}
\value{

 \item{null}{logical, indicating whether this simulation is conducted under null condition}
 
 When null is FALSE:
 \item{dissmall }{The discovery rate table for small difference genes.}
\item{dismedium }{The discovery rate table for medium difference genes.}
\item{dislarge }{The discovery rate table for large difference genes.}
\item{disall }{The discovery rate table for all genes.}
\item{disc}{The discovery rate given the fdr threshold.}
\item{levels}{The p-value thresholds. Should be a numeric vector.}
\item{plist}{A list stored p-values in the simulation.}
\item{fdr}{A scalar indicating the fdr threshold.}
\item{diff}{A data frame of logical indicators of the belongings of genes of three 
categories: small, medium and large}
When null is FALSE:
\item{table}{The FPR table under levels for the four methods.}
\item{levels}{The p-value thresholds. Should be a numeric vector.}
\item{plist}{A list stored p-values in the simulation.}
}
\references{
our paper

  Ritchie, M.E., Phipson, B., Wu, D., Hu, Y., Law, C.W., Shi, W., and Smyth, G.K. (2015). limma powers
  differential expression analyses for RNA-sequencing and microarray studies. Nucleic Acids Research 43(7),
  e47.
  
  Lund S P, Nettleton D, McCarthy D J, et al. Detecting differential expression in RNA-sequence data using quasi-likelihood with shrunken dispersion estimates[J]. Stat Appl Genet Mol Biol, 2012, 11(5): 8.
}
\author{
Yilun Zhang, David Rocke
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{edgeR.QL.running}}, \code{\link{limma.trans.running}}, \code{\link{edgeR.QL.running}}, \code{\link{intSEQ}}
}
\examples{
#select the first 10 columns of mont&pick data
data(count.data)
data(condition)
count=count.data[,1:10]
cond=rep(0:1,each=5)
res=intSEQ(count, cond)
simu.res <- simuComp(res, ntime = 2)

}


