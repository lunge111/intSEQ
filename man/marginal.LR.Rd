\name{marginal.LR}
\alias{marginal.LR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the integrated likelihood ratio.
}
\description{
A function to calculate the integrated likelihood ratio statistics.
}
\usage{
marginal.LR(thetas, disp.fit, data,  thet, fit.thet, lambda1, 
lambda2, meanmeth, is.smooth = TRUE, m0, ma,const,w1,w2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{thetas}{
Selected dispersions that located in the neighborhood of objective gene.
}
  \item{disp.fit}{
Selected smoothed dispersions that located in the neighborhood of objective gene.
}
  \item{data}{
The count data of objective gene
}

  \item{thet}{
The estimated dispersion of objective gene.
}
  \item{fit.thet}{
The smoothed dispersion of objective gene.
}
  \item{lambda1}{
The first parameter of prior variance. See details of intSEQ.
}
  \item{lambda2}{
The second parameter of prior variance. See details of intSEQ.
}
  \item{meanmeth}{
Use the estimated dispersion or the local mean as the mean of normal prior.
}
  \item{is.smooth}{
logical. Whether to use the smoothed dispersion or the estimator itself as the mean of normal prior.
}
  \item{m0}{
A numeric vector indicating the mean of objective gene under null model.
}
  \item{ma}{
A numeric vector indicating the mean of objective gene under full model.
}
\item{const}{
A multiplying constant to make the product of likelihoods non-zero.
}
\item{w1}{
See Details in \code{\link{intSEQ}}.
}
\item{w2}{
See Details in \code{\link{intSEQ}}.
}
}
\details{
The marginal.LR returns the integrated likelihood ratio statistic for a objective gene. See \code{\link{intSEQ}} for detail.
}
\value{
A scalar indicating the integrated likelihood ratio statistic
}
\references{
our paper
}
\author{
Yilun Zhang, David Rocke
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{intSEQ}}
}
