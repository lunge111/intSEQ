joint.likelihood <-
function(theta, ys,m0,ma,null=FALSE,const=1){
  #get joint likelihood for a vector of dispersion
  sapply(theta, joint.like,ys=ys,m0=m0,ma=ma,null=null,const = const)
}
