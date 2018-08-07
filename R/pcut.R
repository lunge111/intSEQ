pcut <-
function(p, levels=c(1e-6,1e-5,1e-4,1e-3,0.01,0.05,0.1)){
  p[is.na(p)]=1
  sapply(levels, function(x) sum(p<x))/length(p)
}
