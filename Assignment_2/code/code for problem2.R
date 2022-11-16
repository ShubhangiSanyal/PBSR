library(MASS)
library(LaplacesDemon)
#Question 1
negloglike=function(theta,d1,d2){
  beta0=theta[1]
  beta1=theta[2]
  sigma=theta[3]
  d2=beta0+beta1*d2
  l=sum(dnorm(d1,mean=d2,sd=sigma,log=T))
  return(-l)
}
x=Insurance$Holders
y=Insurance$Claims
estimate=optim(c(10,0,1),negloglike,d1=y,d2=x)
estimate$par
bic=2*(negloglike(estimate$par,y,x))+log(length(x))*3
bic
#Question 2
negloglikelap=function(theta,d1,d2){
  beta0=theta[1]
  beta1=theta[2]
  sigma=theta[3]
  d2=beta0+beta1*d2
  l=sum(dlaplace(d1,location=d2,scale=sigma,log=T))
  return(-l)
}
estimate=optim(c(1,1,1),negloglikelap,d1=y,d2=x)
estimate$par
bic=2*(negloglikelap(estimate$par,y,x))+log(length(x))*3
bic
#Question 3
negloglikelognorm=function(theta,d1,d2){
  n=length(x)
  l=0
  beta0=theta[1]
  beta1=theta[2]
  sigma=theta[3]
  for (i in 1:n){
    if(d1[i]>0){
      mu=beta0+beta1*d2[i]
      l=l+(dlnorm(d1[i],meanlog=mu,sdlog=sigma,log=T))
    }
  }
  return(-l)
}
estimate=optim(c(1,0,1),negloglikelognorm,d1=y,d2=x)
estimate$par
bic=2*(negloglikelognorm(estimate$par,y,x))+log(length(x))*3
bic
#Question 4
negloglikegamma=function(theta,d1,d2){
  n=length(x)
  l=0
  beta0=theta[1]
  beta1=theta[2]
  sigma=theta[3]
  for (i in 1:n){
    if(d1[i]>0){
      mu=beta0+beta1*d2[i]
      l=l+(dgamma(d1[i],shape=mu,scale=sigma,log=T))
    }
  }
  return(-l)
}
estimate=optim(c(1,1,1),negloglikegamma,d1=y,d2=x)
estimate$par
