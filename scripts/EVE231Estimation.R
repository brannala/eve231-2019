### EVE 131 Point estimation and confidence intervals Oct 28 2019

### simulate sample from an exponential distribution with lambda=2 (mean=1/2)
mydat <- rexp(1000,2)
mean(mydat)
(1/2)*sqrt(1/length(mydat))
sd(mydat)

### function for calculating alpha-percent confidence interval if sigma is known
mean.ci <- function(data,sigma,alpha)
{
    me <- mean(data)
    se <- sigma*sqrt(1/length(data))
    eps <- qnorm(1 - alpha/2.0)*se
    return(c(me - eps, me + eps))
}

### simulate the sampling distribution
sampleDistExp <- function(reps,n,lambda)
{
    meanvec=c();
    for(i in 1:reps)
    {
        meanvec[i] <- mean(rexp(n,lambda))
    }
    return(meanvec)
}

sampleDistNorm <- function(reps,n)
{
    meanvec=c();
    for(i in 1:reps)
    {
        meanvec[i] <- mean(rnorm(n))
    }
    return(meanvec)
}

### compare predicted sd with observed sd
myMean <- sampleDistExp(1000,1000,2)
hist(x=myMean)
mean(myMean)
sd(myMean)
(1/2)*sqrt(1/1000)

sampleIntExp <- function(reps,n,lambda)
{
    CIvec=matrix(nrow=reps,ncol=2);
    for(i in 1:reps)
    {
        CIvec[i,] <- mean.ci(rexp(n,lambda),1/lambda,0.05)
    }
    return(CIvec)
}

sampleIntNorm <- function(reps,n)
{
    CIvec=matrix(nrow=reps,ncol=2);
    for(i in 1:reps)
    {
        CIvec[i,] <- mean.ci(rnorm(n),1,0.05)
    }
    return(CIvec)
}


myInt <- sampleIntExp(1000,1000,2)

checkInt <- function(CImat,Tparam)
{
    inInterval=0;
    for(i in 1:length(CImat[,1]))
    {
        if((CImat[i,1] < Tparam)&&(CImat[i,2] > Tparam))
           inInterval <- inInterval + 1;
    }
    return(inInterval/length(CImat[,1]))
}

checkInt(myInt,0.5)

### t distribution for normally distributed data with unknown mean and variance
myDat <- rnorm(100,10,2)
t.test(myDat)

## simulation based confidence interval (parametric bootstrap)
origMean <- mean(myDat)
origSD <- sd(myDat)
meanVec <- c()
for(i in 1:5000)
    meanVec[i] = mean(rnorm(length(myDat),origMean,origSD))
quantile(meanVec,c(0.025,0.975))

## bootstrap based confidence interval
for(i in 1:50000)meanVec[i] = mean(sample(myDat,length(myDat),replace=TRUE))
quantile(meanVec,c(0.025,0.975))
