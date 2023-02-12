#1
set.seed(2023)
data1 <- (rcauchy(1000, location=0, scale=1))
ECDF <- ecdf(data1)
empiricalCDF <- ECDF(data1)
D <- max(abs(empiricalCDF-pnorm(data1)))

ks_func <- function(data1){
  ECDF <- ecdf(data1)
  empiricalCDF <- ECDF(data1)
  D <- max(abs(empiricalCDF-pnorm(data1)))
  n <- length(data1)
  p <- 2*exp(-D^2 * (2*n)/(1+n/n))
  return(print(paste('p-value: ', p, 'D: ', D)))
}

ks.test(data1, 'pnorm')
ks_func(data1)
?ks.test

#2
set.seed (123)
data2 <- data.frame(x = runif(200, 1, 10))
data2$y <- 0 + 2.75*data2$x + rnorm(200, 0, 1.5)

olsreg <- lm(y~x, data=data2)
summary(olsreg)

lin_lik <- function(theta, y, x){
  n <- nrow(x)
  k<- ncol(x)
  beta <- theta[1:k]
  sigma2 <- theta[k+1]^2
  e <- y-x%*%beta
  logl <- -.5*n*log(2*pi)-.5*n*log(sigma2)-((t(e))%*%e)/(2*sigma2)
return(-logl)
}


mle <- optim(fn=lin_lik,
             par= c(1,1,1),
             hessian =TRUE,
             y= data2$y, 
             X= cbind(1, data2$x),
             method='BFGS')

mle$par


