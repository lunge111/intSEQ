joint.like <-
function(theta,  ys, cond, m0,ma,null = FALSE,const=1){
  #find the joint likelihood
  if(null) {
  ma<-m0
  }
    l=likelihoodpercond(theta, mu=ma, ys=ys,const)
  prod(l)
}
