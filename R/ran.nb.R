ran.nb <-
function(par, nsamp){
  rnbinom( mu = par[1], size = 1/par[2], n=nsamp)
}
