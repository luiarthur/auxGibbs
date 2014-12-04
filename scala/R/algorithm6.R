rm(list=ls())
#par(mfrow=c(1,1))
y <- c(-1.48,-1.4,-1.16,-1.08,-1.02,.14,.51,.53,.78)
#y <- c(rnorm(10,-1.5,.1), rnorm(10,-1,.1), rnorm(10,-.5,.1), rnorm(10,.5,.1))
#y <- c(rnorm(10,-1.5,.1), rnorm(10,-.5,.1), rnorm(10,.5,.1))
#y <- rnorm(100, c(-.5,.5), .1)
#y <- geyser[,1]

n <- length(y)
R <- 10000
a <- 1

theta <- rep(rnorm(1,72,13),length=n)

results <- matrix(NA,nrow=R,ncol=n)
newTheta <- numeric(R)
for (r in 1:R) {
  #cat("Iteration ",r,"\n")
  for (i in 1:n){
    # cat(i,": ",paste(theta,collapse=", "),"\n")
    #x <- c(theta[-i],rnorm(1,0,sd(y)))
    x <- c(theta[-i],rnorm(1,0,1))
    prob <- c(rep(1/(n-1+a),n-1), a/(n-1+a))
    cand <- sample(x,1,prob=prob)
    if ( log(runif(1)) < dnorm(y[i],cand,.1,log=TRUE) - 
                         dnorm(y[i],theta[i],.1,log=TRUE) ) {
      theta[i] <- cand
    }
  }
  results[r,] <- theta
  newTheta[r] <- sample(c(theta,rnorm(1)),size=1,prob=c(rep(1/(n+a),n),a/(n+a)))
}

apply(results,2,mean)
plot(density(results[,1]))
plot(density(results[,n]))

plot(results[,1],type="l")
plot(results[,n],type="l")

prediction <- rnorm(length(newTheta),mean=newTheta,sd=0.1)
plot(density(prediction))

# Pictures
#par(mfrow=c(2,1))
#source("./micky.R")

phi <- as.matrix(as.double(rownames(table(theta))))
draw <- function(x){
  rnorm(100,x,.1)
}
M <- apply(phi,1,draw)
plot(y,xlim=c(1,100),ylim=c(min(y,M),max(y,M)),cex=5)
for (i in 1:ncol(M)){
  points(M[,i],col=i+1,pch=20)
}  

