singleHessian <- function(y,mu, theta){
  -(y * (1/theta^2) - ((1/theta^2 * (gamma(y + 1/theta) * (1/theta^2 *
  trigamma(y + 1/theta)) + 1/theta^2 * (gamma(y + 1/theta) *
 digamma(y + 1/theta)) * digamma(y + 1/theta)) + 2 * theta/(theta^2)^2 *
 (gamma(y + 1/theta) * digamma(y + 1/theta)))/gamma(y + 1/theta) -
 1/theta^2 * (gamma(y + 1/theta) * digamma(y + 1/theta)) *
(1/theta^2 * (gamma(y + 1/theta) * digamma(y + 1/theta)))/gamma(y +
1/theta)^2 - ((1/theta^2 * (gamma(1/theta) * (1/theta^2 *
   trigamma(1/theta)) + 1/theta^2 * (gamma(1/theta) * digamma(1/theta)) *
 digamma(1/theta)) + 2 * theta/(theta^2)^2 * (gamma(1/theta) *
 digamma(1/theta)))/gamma(1/theta) - 1/theta^2 * (gamma(1/theta) *
 digamma(1/theta)) * (1/theta^2 * (gamma(1/theta) * digamma(1/theta)))/gamma(1/theta)^2)) -
((y + 1/theta) * (mu * mu/(1 + theta * mu)^2) + 1/theta^2 *
(mu/(1 + theta * mu)) + (1/theta^2 * (mu/(1 + theta *
 mu)) - 2 * theta/(theta^2)^2 * log(1 + theta * mu))))
}

hessian <- function(dat, theta){
  mu = rep(mean(dat),length(dat))
  sum(singleHessian(dat,mu,theta))

}

Hessian <- function(dat,theta, sigma){
  y=log(theta)
  mean0=mean(dat)
 (( hessian(dat,theta)-1/sigma)*exp(3*y)+exp(y))*
    joint.likelihood(theta, ys=dat,m0=mean0 , null = TRUE,const=1)*dnorm(theta, mean = theta,sd = sigma)
    
}

