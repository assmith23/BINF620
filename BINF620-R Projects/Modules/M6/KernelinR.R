#https://www.r-bloggers.com/2012/09/learning-kernels-svm/
install.packages("kernlab")
library(kernlab)
require('kernlab')

#Linear Kernel
kfunction <- function(linear =0, quadratic=0)
{
  k <- function (x,y)
  {
    linear*sum((x)*(y)) + quadratic*sum((x^2)*(y^2))
  }
  class(k) <- "kernel"
  k
}
n = 25
a1 = rnorm(n)
a2 = 1 - a1 + 2* runif(n)
b1 = rnorm(n)
b2 = -1 - b1 - 2*runif(n)
x = rbind(matrix(cbind(a1,a2),,2),matrix(cbind(b1,b2),,2))
y <- matrix(c(rep(1,n),rep(-1,n)))

svp <- ksvm(x,y,type="C-svc",C = 100, kernel=kfunction(1,0),scaled=c())
plot(c(min(x[,1]), max(x[,1])),c(min(x[,2]), max(x[,2])),type='n',xlab='x1',ylab='x2')
title(main='Linear Separable Features')
ymat <- ymatrix(svp)
points(x[-SVindex(svp),1], x[-SVindex(svp),2], pch = ifelse(ymat[-SVindex(svp)] < 0, 2, 1))
points(x[SVindex(svp),1], x[SVindex(svp),2], pch = ifelse(ymat[SVindex(svp)] < 0, 17, 16))

# Extract w and b from the model   
w <- colSums(coef(svp)[[1]] * x[SVindex(svp),])
b <- b(svp)

# Draw the lines
abline(b/w[2],-w[1]/w[2])
abline((b+1)/w[2],-w[1]/w[2],lty=2)
abline((b-1)/w[2],-w[1]/w[2],lty=2)


#quadracetic kernel
require('kernlab')
kfunction <- function(linear =0, quadratic=0)
{
  k <- function (x,y)
  {
    linear*sum((x)*(y)) + quadratic*sum((x^2)*(y^2))
  }
  class(k) <- "kernel"
  k
}
n = 20
r = runif(n)
a = 2*pi*runif(n)
a1 = r*sin(a)
a2 = r*cos(a)
r = 2+runif(n)
a = 2*pi*runif(n)
b1 = r*sin(a)
b2 = r*cos(a)
x = rbind(matrix(cbind(a1,a2),,2),matrix(cbind(b1,b2),,2))
y <- matrix(c(rep(1,n),rep(-1,n)))

svp <- ksvm(x,y,type="C-svc",C = 100, kernel=kfunction(0,1),scaled=c())
par(mfrow=c(1,2))
plot(c(min(x[,1]), max(x[,1])),c(min(x[,2]), max(x[,2])),type='n',xlab='x1',ylab='x2')
title(main='Feature Space')
ymat <- ymatrix(svp)
points(x[-SVindex(svp),1], x[-SVindex(svp),2], pch = ifelse(ymat[-SVindex(svp)] < 0, 2, 1))
points(x[SVindex(svp),1], x[SVindex(svp),2], pch = ifelse(ymat[SVindex(svp)] < 0, 17, 16))

# Extract w and b from the model   
w2 <- colSums(coef(svp)[[1]] * x[SVindex(svp),]^2)
b <- b(svp)
x1 = seq(min(x[,1]),max(x[,1]),0.01)
x2 = seq(min(x[,2]),max(x[,2]),0.01)
points(-sqrt((b-w2[1]*x2^2)/w2[2]), x2, pch = 16 , cex = .1 )
points(sqrt((b-w2[1]*x2^2)/w2[2]), x2, pch = 16 , cex = .1 )
points(x1, sqrt((b-w2[2]*x1^2)/w2[1]), pch = 16 , cex = .1 )
points(x1,  -sqrt((b-w2[2]*x1^2)/w2[1]), pch = 16, cex = .1 )
points(-sqrt((1+ b-w2[1]*x2^2)/w2[2]) , x2, pch = 16 , cex = .1 )
points( sqrt((1 + b-w2[1]*x2^2)/w2[2]) , x2,  pch = 16 , cex = .1 )
points( x1 , sqrt(( 1 + b -w2[2]*x1^2)/w2[1]), pch = 16 , cex = .1 )
points( x1 , -sqrt(( 1 + b -w2[2]*x1^2)/w2[1]), pch = 16, cex = .1 )
points(-sqrt((-1+ b-w2[1]*x2^2)/w2[2]) , x2, pch = 16 , cex = .1 )
points( sqrt((-1 + b-w2[1]*x2^2)/w2[2]) , x2,  pch = 16 , cex = .1 )
points( x1 , sqrt(( -1 + b -w2[2]*x1^2)/w2[1]), pch = 16 , cex = .1 )
points( x1 , -sqrt(( -1 + b -w2[2]*x1^2)/w2[1]), pch = 16, cex = .1 )
xsq <- x^2
svp <- ksvm(xsq,y,type="C-svc",C = 100, kernel=kfunction(1,0),scaled=c())
plot(c(min(xsq[,1]), max(xsq[,1])),c(min(xsq[,2]), max(xsq[,2])),type='n',xlab='x1^2',ylab='x2^2')
title(main='Quadratic Kernel Space')
ymat <- ymatrix(svp)
points(xsq[-SVindex(svp),1], xsq[-SVindex(svp),2], pch = ifelse(ymat[-SVindex(svp)] < 0, 2, 1))
points(xsq[SVindex(svp),1], xsq[SVindex(svp),2], pch = ifelse(ymat[SVindex(svp)] < 0, 17, 16))

# Extract w and b from the model   
w <- colSums(coef(svp)[[1]] * xsq[SVindex(svp),])
b <- b(svp)

# Draw the lines
abline(b/w[2],-w[1]/w[2])
abline((b+1)/w[2],-w[1]/w[2],lty=2)
abline((b-1)/w[2],-w[1]/w[2],lty=2)

#Kernel matrix centering and alignment functions are implemented in the 'R':
require('kernlab')
n = 100
a1 = 4*runif(n)-2
a2 = 1 - a1^2 - 0.5 - 2*runif(n)
b1 = 4*runif(n)-2
b2 = 1 - b1^2 + 0.5 + 2*runif(n)
x = rbind(matrix(cbind(a1,a2),,2),matrix(cbind(b1,b2),,2))
y <- matrix(c(rep(1,n),rep(-1,n)))
kfunction <- function(linear, quadratic)
{
  k <- function (x,y)
  {
    linear*sum((x)*(y)) + quadratic*sum((x^2)*(y^2))
  }
  class(k) <- "kernel"
  k
}
center_kernel <- function(kernel)
{
  m <- length(kernel[,1])
  ones <- matrix(rep(1,m),m)
  (diag(m) - ones %*% t(ones) / m)*kernel*(diag(m) - ones %*% t(ones) / m)
}
f_product<- function(x,y)
{
  sum(diag(crossprod(t(x),y)))
}
f_norm<- function(x)
{
  sqrt(f_product(x,x))
}
kernel_alignment <- function(x,y)
{
  f_product(x,y)/(f_norm(x)*f_norm(y))
}

x_kernel1 <- kernelMatrix(kfunction(1,0),x)
y_kernel   <- y %*% t(y)
x_kernel2 <- kernelMatrix(kfunction(0,1),x)
x_kernel1_c <- center_kernel(x_kernel1)
x_kernel2_c <- center_kernel(x_kernel2)
y_kernel_c <- center_kernel(y_kernel)
alignment1 <- kernel_alignment(x_kernel1_c,y_kernel_c)
alignment2 <- kernel_alignment(x_kernel2_c,y_kernel_c)
x_kernel3 <- kernelMatrix(kfunction(alignment1, alignment2),x)
x_kernel3_c <- center_kernel(x_kernel3)
alignment3 <- kernel_alignment(x_kernel3_c,y_kernel_c)
svp <- ksvm(x,y,type="C-svc",C = 100, kernel=kfunction(alignment1,alignment2), scaled=c())
par(mfrow=c(2,1))
plot(c(min(x[]), max(x[])), c(min(x[]), max(x[])),type='n',xlab='x1',ylab='x2')
title(main='Parabolic Featureset (Mixed Kernel SVM)')
ymat <- ymatrix(svp)
points(x[-SVindex(svp),1], x[-SVindex(svp),2], pch = ifelse(ymat[-SVindex(svp)] < 0, 2, 1))
points(x[SVindex(svp),1], x[SVindex(svp),2], pch = ifelse(ymat[SVindex(svp)] < 0, 17, 16))

# Extract w and b from the model   
w <- colSums(coef(svp)[[1]] * (alignment1*x[SVindex(svp),]))
v <- colSums(coef(svp)[[1]] * (alignment2*x[SVindex(svp),]^2))
b <- b(svp)
x1 = seq(min(x[]),max(x[]),0.01)
x2 = seq(min(x[]),max(x[]),0.01)
points( sqrt( (b-w[2]*x2-v[2]*x2^2)/v[1] + (w[1]/(2*v[1]))^2 ) - w[1]/(2*v[1]) , x2, pch = 16, cex = .1 )
points( -sqrt( (b-w[2]*x2-v[2]*x2^2)/v[1] + (w[1]/(2*v[1]))^2 ) - w[1]/(2*v[1]) , x2, pch = 16, cex = .1 )
points( x1, sqrt( (b-w[1]*x1-v[1]*x1^2)/v[2] + (w[2]/(2*v[2]))^2 ) - w[2]/(2*v[2]) , pch = 16, cex = .1 )
points( x1, -sqrt( (b-w[1]*x1-v[1]*x1^2)/v[2] + (w[2]/(2*v[2]))^2 ) - w[2]/(2*v[2]) , pch = 16, cex = .1 )
points( sqrt( (1+ b-w[2]*x2-v[2]*x2^2)/v[1] + (w[1]/(2*v[1]))^2 ) - w[1]/(2*v[1]) , x2, pch = 16, cex = .1 )
points( -sqrt( (1 + b-w[2]*x2-v[2]*x2^2)/v[1] + (w[1]/(2*v[1]))^2 ) - w[1]/(2*v[1]) , x2, pch = 16, cex = .1 )
points( x1, sqrt( (1+b-w[1]*x1-v[1]*x1^2)/v[2] + (w[2]/(2*v[2]))^2 ) - w[2]/(2*v[2]) , pch = 16, cex = .1 )
points( x1, -sqrt( (1+b-w[1]*x1-v[1]*x1^2)/v[2] + (w[2]/(2*v[2]))^2 ) - w[2]/(2*v[2]) , pch = 16, cex = .1 )
points( sqrt( (-1+ b-w[2]*x2-v[2]*x2^2)/v[1] + (w[1]/(2*v[1]))^2 ) - w[1]/(2*v[1]) , x2, pch = 16, cex = .1 )
points( -sqrt( (-1 + b-w[2]*x2-v[2]*x2^2)/v[1] + (w[1]/(2*v[1]))^2 ) - w[1]/(2*v[1]) , x2, pch = 16, cex = .1 )
points( x1, sqrt( (-1+b-w[1]*x1-v[1]*x1^2)/v[2] + (w[2]/(2*v[2]))^2 ) - w[2]/(2*v[2]) , pch = 16, cex = .1 )
points( x1, -sqrt( (-1+b-w[1]*x1-v[1]*x1^2)/v[2] + (w[2]/(2*v[2]))^2 ) - w[2]/(2*v[2]) , pch = 16, cex = .1 )
plot(x[]^2,type='n',xlab='x1^2',ylab='x2^2')
title(main='Not Linear Separable in Quadratic Kernel Coordinates')
points(x[1:100,]^2,pch=16)
points(x[101:200,]^2,pch=2)

