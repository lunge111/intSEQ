mednorm <-
function(mat){
  cmeds <- apply(mat,2,median)
  medmed <- median(cmeds)
  appnorm <- function(x){
    return(medmed*x/cmeds)
  }
  norm.mat <- t(apply(mat,1,appnorm))
  return(norm.mat)
}
