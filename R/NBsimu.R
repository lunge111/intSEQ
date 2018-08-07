NBsimu <-
function(mle1, null=F,nsamp=3,lfc,sdd=0.3631473,norm=TRUE){
  #simulate a count table looks like the real data based on NB distribution
  #default of sdd is the standard deviation
  mle1=as.matrix(mle1)
  n=nrow(mle1)
  if(null) {
    ct=sample.nb(mle1,2*nsamp)
  }else{
    m1=mle1[,1]
    m2=m1*2^(-lfc)
    mle2 <- cbind(t(t(m2)), mle1[, 2,drop=F])
    ct1 <- sample.nb(mle1,nsamp)
    ct2 <- sample.nb(mle2,nsamp)
    ct <- cbind(ct1,ct2)
  }
  cond = rep(0:1,each=nsamp)
  if(norm){
    normfac=exp(rnorm(2*nsamp,mean = 0, sd=sdd))
    ct=round(ct%*%diag(normfac),0)   
  }   



  return(list(cd=ct, cond=cond))
}
