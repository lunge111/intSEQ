marginal.LR <-
function(thetas,disp.fit, data, thet, fit.thet, lambda1,lambda2,meanmeth
                         ,is.smooth=TRUE,m0,ma,const,w1,w2){
   #calculate the marginal likelihood ratio statistics
   n=length(ma)
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
   if(n<=60){
  if(is.null(lambda1)){ lambda1=2
   lambda2=0}
   
   sdd =sqrt(lambda1*v+lambda2)
   sdd=max(sdd, 0.05) #aviod too small variance

   nullfunc <- function(y){

     joint.likelihood(exp(y),ys=data,  m0, ma, null=T, const)*dnorm(exp(y)
                                                               ,mean = m,sd = sdd)*exp(y)
   }
   fullfunc <- function(y){
     joint.likelihood(exp(y),ys=data, m0,ma, null=F,const)*dnorm(exp(y)
                                                               ,mean = m,sd = sdd)*exp(y)
   }
   }else{
     #for large sample size, use non-informative prior
     #define the null and full likelihood function
     nullfunc <- function(y){
       
       joint.likelihood(exp(y),ys=data,  m0, ma, null=T, const)*exp(y)
     }
     fullfunc <- function(y){
       joint.likelihood(exp(y),ys=data, m0,ma, null=F,const)*exp(y)
     }
   }
   ghrule <- gaussHermiteData(20)
   
 
   muu = w1*m+(1-w1)*thet
  # Hhat = Hessian(dat = data, theta = muu, sigma = sdd)
   Hhat = -n
   sigma = w2*1+(1-w2)*sqrt(-1/Hhat)
   nullLik=aghQuad(g = nullfunc, muHat = log(muu), sigmaHat = sigma, rule = ghrule)
   fullLik=aghQuad(g = fullfunc, muHat = log(muu)
                   , sigmaHat = sigma, rule = ghrule)
   if(is.na(nullLik)|is.na(fullLik)){
     LR<- 0.0001
     warning("Integral is probably divergent, the null is not rejected")
   }else{
    LR<- -2*(log(nullLik)-log(fullLik))
   }
   LR
}
