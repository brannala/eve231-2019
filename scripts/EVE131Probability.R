
# EVE 131 Lecture Mon Oct 21, 2019
# Exploring probability disttributions in R

# Rounding fun
round(12.500,0)
round(13.500,0)
x <- c(1.2500,1.3500,1.467,1.499)
round(x,1)


# Discrete distributions

# Probability mass functions (pmfs) and cumulative distribution functions (cdfs)
par(mfrow=c(1,1))
# Binomial Distribution
# pmf
p=0.1; n=10
dbinom(4,n,p)
x <- seq(0,n,by=1)
y <- dbinom(x,n,p)
plot(x,y,type="h",main="Binomial pmf")
# expectation
e <- y*x
expbin <- sum(e)
# variance
v <- y*(x - expbin)^2
varbin <- sum(v)
# cdf
pbinom(4,n,p)
x1 <- seq(0,n,by=1)
y2 <- pbinom(x1,n,p)
plot(x1,y2,type="h",main="Binomial cdf")
# simulate random variables
rx <- rbinom(100,n,p)
mean(rx)
var(rx)

# Poisson Distribution
la=1.2; n2=round(10*la,0)
dpois(4,la)
x3 <- seq(0,n2,by=1)
y3 <- dpois(x3,la)
plot(x3,y3,type="h",main="Poisson pmf")
# expectation
e3 <- y3*x3
expbin3 <- sum(e3)
# variance
v3 <- y3*(x3 - expbin3)^2
varbin3 <- sum(v3)
#cdf
ppois(4,la)
x4 <- seq(0,n2,by=1)
y4 <- ppois(x4,la)
plot(x4,y4,type="h",main="Poisson cdf")
# simulate random variables
rxp <- rpois(100,la)
mean(rxp)
var(rxp)


# Poisson approximation to binomial (n -> Infty & np -> la)
# n=10 p=0.1 la=1
p=0.1; n=10
par(mfrow=c(1,2))
x <- seq(0,n,by=1)
y <- dbinom(x,n,p)
plot(x,y,type="h",main="Binomial")
la=1; 
x3 <- seq(0,n,by=1)
y3 <- dpois(x3,la)
plot(x3,y3,type="h",main="Poisson")
# n=100 p=0.01 la=1
p=0.01; n=100
par(mfrow=c(1,2))
x <- seq(0,10,by=1)
y <- dbinom(x,n,p)
plot(x,y,type="h",main="Binomial")
la=1; 
x3 <- seq(0,10,by=1)
y3 <- dpois(x3,la)
plot(x3,y3,type="h",main="Poisson")

# Continuous distributions

# Probability density functions (pdfs) and cumulative distribution functions (cdfs)
par(mfrow=c(1,1))
# Exponential Distribution
# pdf
la=2; ne=10
dexp(0.1,la)
xe <- seq(0,ne,by=0.1)
ye <- dexp(xe,la)
plot(xe,ye,type="l",main="Exponential pdf")
# probability variable is between 1 and 2
integrate(f <- function(x){return(dexp(x,la))},1,2)
integrate(f <- function(x){return(dexp(x,la))},1,2)$value
# expectation
expexp <- integrate(f <- function(x){return(x*dexp(x,la))},0,la*10)
print(expexp$value)
# variance
varexp <- integrate(f <- function(x){return(dexp(x,la)*(expexp$value-x)^2)},0,la*10) 
print(varexp$value)
# cdf
pexp(2,la)
integrate(f <- function(x){return(dexp(x,la))},0,2)$value
# simulate random variables
rxe <- rexp(100,2)
mean(rxe)
var(rxe)

# Normal Distribution
# pdf
mu1=1; sd1=2; ne=10
dnorm(2,mu1,sd1)
xn <- seq(-ne+mu1,ne+mu1,by=0.1)
yn <- dnorm(xn,mu1,sd1)
plot(xn,yn,type="l",main="Normal pdf")
# expectation
expnorm <- integrate(f <- function(x){return(x*dnorm(x,mu1,sd1))},-20,20)
print(expnorm$value)
# variance
varnorm <- integrate(f <- function(x){return(dnorm(x,mu1,sd1)*(expnorm$value-x)^2)},-20,20) 
print(varnorm$value)
# cdf
pnorm(1.96,0,1)
integrate(f <- function(x){return(dnorm(x,0,1))},-20,1.96)$value
# simulate random variables
rxn <- rnorm(100,mu1,sd1)
mean(rxn)
var(rxn)

