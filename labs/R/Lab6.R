# create p of M=3 probabilities of exclusive events
p=c(1/6,2/6,3/6)
# create M=3 row vectors of size 2×1, called mu1, mu2, mu3
mu1=c(-3,-4); mu2=c(0,0); mu3=c(4,5)
# create M=3 symmetric positive definite matrices of size 2×2, called sigma1, sigma2 and sigma3
sigma1=matrix(c(4,1,1,1),nrow=2)
sigma2=matrix(c(1,0,0,1),nrow=2)
sigma3=matrix(c(9,2,2,1),nrow=2)
# set N
N=500
# initialize X, J
X=matrix(nrow=N, ncol = 2)
J=integer(N)
Z=matrix(nrow=N, ncol = 3)

# load package for mvrnorm()
library(MASS)
# set j and z
for (i in 1:N) 
{
  Ji=sample(1:3, size= 1, prob = p)
  J[i]=Ji
  if(J[i]==1)
  {
    X[i,]=mvrnorm(n=1, mu=mu1, Sigma=sigma1)
    Z[i,1]=1;Z[i,2]=0;Z[i,3]=0
  }
  if(J[i]==2)
  {
    X[i,]=mvrnorm(n=1, mu=mu2, Sigma=sigma2)
    Z[i,1]=0;Z[i,2]=1;Z[i,3]=0
  }
  if(J[i]==3)
  {
    X[i,]=mvrnorm(n=1, mu=mu3, Sigma=sigma3)
    Z[i,1]=0;Z[i,2]=0;Z[i,3]=1
  }
}

library(mvtnorm)




EM=function(X){
  # function EStep
  EStep=function(X,p1_h,p2_h,p3_h,sigma1_h,sigma2_h,sigma3_h,mu1_h,mu2_h,mu3_h){
    p_h=matrix(0, nrow = N, ncol = 3)
    for (i in 1:N) {
      temp1=p1_h*dmvnorm(X[i,],mean=mu1_h,sigma = sigma1_h)
      temp2=p2_h*dmvnorm(X[i,],mean=mu2_h,sigma = sigma2_h)
      temp3=p3_h*dmvnorm(X[i,],mean=mu3_h,sigma = sigma3_h)
      # calculating belonging probabilities for every i
      p_h[i,1]=temp1/(temp1+temp2+temp3)
      p_h[i,2]=temp2/(temp1+temp2+temp3)
      p_h[i,3]=temp3/(temp1+temp2+temp3)
    }
    p_h
  }
  # function MStep
  Mstep=function(X,p_h){
    px1=matrix(0,nrow = 1, ncol = 2)
    px2=matrix(0,nrow = 1, ncol = 2)
    px3=matrix(0,nrow = 1, ncol = 2)
    nomi1=matrix(0,nrow = 2, ncol = 2)
    nomi2=matrix(0,nrow = 2, ncol = 2)
    nomi3=matrix(0,nrow = 2, ncol = 2)
    for (m in 1:N)
    {
      px1=px1+p_h[m,1]*X[m,]
      px2=px2+p_h[m,2]*X[m,]
      px3=px3+p_h[m,3]*X[m,]
    }
    # estimate mu
    mu1_hplus<<-px1/sum(p_h[,1])
    mu2_hplus<<-px2/sum(p_h[,2])
    mu3_hplus<<-px3/sum(p_h[,3])
    for (m in 1:N)
    {
      nomi1=nomi1+p_h[m,1]*t(X[m,]-mu1_hplus)%*%(X[m,]-mu1_hplus)
      nomi2=nomi2+p_h[m,2]*t(X[m,]-mu2_hplus)%*%(X[m,]-mu2_hplus)
      nomi3=nomi3+p_h[m,3]*t(X[m,]-mu3_hplus)%*%(X[m,]-mu3_hplus)
    }
    # estimate sigma
    sigma1_hplus<<-nomi1/sum(p_h[,1])
    sigma2_hplus<<-nomi2/sum(p_h[,2])
    sigma3_hplus<<-nomi3/sum(p_h[,3])
    # estimate p1, p2 and p3
    p1_hplus<<-sum(p_h[,1])/N
    p2_hplus<<-sum(p_h[,2])/N
    p3_hplus<<-1-p1_hplus-p2_hplus
    theta_hplus=c(p1_hplus, p2_hplus, p3_hplus, mu1_hplus, mu2_hplus, mu3_hplus, as.vector(sigma1_hplus)[-3], as.vector(sigma2_hplus)[-3], as.vector(sigma3_hplus)[-3])
    theta_hplus
  }
  # set initial values of p1, p2, p3, pi_hat
  p1_h=1/3; p2_h=1/3; p3_h=1/3
  p1_hplus=1/3;p2_hplus=1/3;p3_hplus=1/3
  p_h=matrix(1/3,nrow=N, ncol = 3)
  mu1_h=c(0,0); mu2_h=c(0,0); mu3_h=c(0,0)
  mu1_hplus=c(0,0); mu2_hplus=c(0,0); mu3_hplus=c(0,0)
  sigma1_h=diag(1,2); sigma2_h=diag(1,2); sigma3_h=diag(1,2)
  sigma1_hplus=diag(1,2)
  sigma2_hplus=diag(1,2)
  sigma3_hplus=diag(1,2)
  
  p1_h=1/3; p2_h=1/3; p3_h=1/3
  # mu1_h=colMeans(X); mu2_h=colMeans(X); mu3_h=colMeans(X)cov(X)
  mu1_h=c(-1,-1); mu2_h=c(0,0); mu3_h=c(1,1)
  sigma1_h=cov(X)
  sigma2_h=cov(X)
  sigma3_h=cov(X)
  
  j=0
  while(j<100)
  {
    
    p_h=EStep(X,p1_h,p2_h,p3_h,sigma1_h,sigma2_h,sigma3_h,mu1_h,mu2_h,mu3_h)
    theta_hplus=Mstep(X, p_h)
    
    
    theta_h=c(p1_h, p2_h, p3_h, mu1_h, mu2_h, mu3_h, as.vector(sigma1_h)[-3], as.vector(sigma2_h)[-3], as.vector(sigma3_h)[-3])
    delta_norm = norm(as.matrix(theta_hplus-theta_h), type = "f")
    
    if((j %% 5) == 0){
      print(j)
      print(delta_norm)
    }
    
    p1_h=p1_hplus; p2_h=p2_hplus; p3_h=p3_hplus
    mu1_h=mu1_hplus; mu2_h=mu2_hplus; mu3_h=mu3_hplus
    sigma1_h=sigma1_hplus; sigma2_h=sigma2_hplus; sigma3_h=sigma3_hplus
    j <- j + 1
    
    if(
      delta_norm < 0.01*norm(as.matrix(theta_h), type = "f")# norm change of parameter<1%
    ){
      print(j)
      print(delta_norm)
      cat(paste(p1_h,p2_h,p3_h,"\n", sep = "\t"))
      print(mu1_h)
      print(mu2_h)
      print(mu3_h)
      print(sigma1_h)
      print(sigma2_h)
      print(sigma3_h)
      break
    }
  }
}

EM(X)

# the out puts are
# p:
# 0.336001647241334	0.139219984599924	0.524778368158742
# the true value of p is 0.333, 0.167, and 0.050
# mu:
#     [,1]        [,2]
# [1,] -0.103236 -0.05925602
#     [,1]      [,2]
# [1,] -2.71932 -4.090003
#     [,1]     [,2]
# [1,] 4.14481 5.019391
# the true value of mu is (0,0), (-3,-4), (4,5)
# sigma:
#       [,1]      [,2]
# [1,] 1.2337155 0.1566621
# [2,] 0.1566621 1.4653899
#       [,1]      [,2]
# [1,] 4.1569311 0.7033556
# [2,] 0.7033556 0.7206826
#       [,1]     [,2]
# [1,] 8.482224 1.898359
# [2,] 1.898359 1.003236
# the true value of sigma is c(1,0,0,1),c(4,1,1,1),c(9,2,2,1)
# The result is close to the true values
