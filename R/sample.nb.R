sample.nb <-
function(mom, nsamp = 3){
  #NB sample with given parameters, if the input NB dispersion is non-positive, generate poisson sample instead
  means <- mom[, 1]
  disper <- mom[, 2]
  pois.index = disper<=0  
  count.data = matrix(NA,ncol=nsamp, nrow=length(means))
  count.data[!pois.index,]=t(apply(mom[!pois.index,],
                                   1, ran.nb, nsamp=nsamp))
  count.data[pois.index,]=t(apply(mom[pois.index,,drop=F], 
                                  1 ,ran.poi, nsamp=nsamp))
count.data
}
