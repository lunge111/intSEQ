marginal.LR <-
function(thetas,disp.fit, data, cond,thet, fit.thet, lambda1,lambda2,meanmeth
                         ,is.smooth=TRUE,m0,ma,const){
   #calculate the marginal likelihood ratio statistics
   n=length(cond)
  if(is.null(lambda1)){ lambda1=2
   lambda2=0}
   if(!is.smooth){
     thett=thetas[thetas>1e-3]
     m=mean(thett)
     v=var(thett)
   }else{thett=thetas[thetas>1e-3]
   disp.fit=disp.fit[thetas>1e-3]
   m=switch(meanmeth,
            estimator = thet,
            local.mean = fit.thet)
   v=var(thett-disp.fit)
   }
   sdd =sqrt(lambda1*v+lambda2)
   sdd=max(sdd, 0.05) #aviod too small variance
if(n>40) {
  sdd=1e6
}
   nullfunc <- function(y){

     joint.likelihood(exp(y),ys=data, cond=cond,m0,ma, null=T,const)*exp(y+y^2)
   }
   fullfunc <- function(y){
     joint.likelihood(exp(y),ys=data, cond=cond,m0,ma, null=F,const)*exp(y+y^2)
   }
   rule15 <- gaussHermiteData(20)
  nullLik=ghQuad(f = nullfunc, rule = rule15)
   fullLik=ghQuad(f = fullfunc, rule =  rule15)
   if(is.na(nullLik)|is.na(fullLik)){
     LR<- 0.0001
     warning("Integral is probably divergent, the null is not rejected")
   }else{
    LR<- -2*(log(nullLik)-log(fullLik))
   }
   LR
}
