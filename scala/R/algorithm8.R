rm(list=ls())
#install.packages("ggplot2")
#library(ggplot2)

y <- c(-1.48,-1.4,-1.16,-1.08,-1.02,.14,.51,.53,.78)
y <- sort(y)
#y <- c(rnorm(10,-1.5,.1), rnorm(10,-.5,.1), rnorm(10,.5,.1))
#y <- sort(rnorm(200,c(-.5,.5),.1))
#y <- sort(rnorm(300,c(-.5,.5,1.5),.1))
#y <- sample(y)

n <- length(y)

#x <- c(5,2,3,1,1,3,3,4,2,2,5,2,5,1)
relabel <- function(x){
  uniq <- unique(x)
  y <- NULL
  for (i in 1:length(uniq)){
    y[x==uniq[i]] <- i
  }
  y
}

rg <- function(n=1){
  rnorm(n,0,1)
  #rnorm(n,0,sd(y))
}

rF <- function(n=1,x){
  #rnorm(n,x,.1)
  rnorm(n,x,.1)
}

dF <- function(x,phi){
  #dnorm(x,phi,.1)
  dnorm(x,phi,.1)
}

# Take out the c[i]
# c[i] needs to go back to one of the existing clusters
# OR it needs to get a new cluster.
# In practice, a=1, m=1.

c <- rep(1,n) 
#c <- sample(1:n,n,replace=TRUE)
phi <- rg(length(unique(c))+1)

# Chinese Restaurant:
for (i in 1:n){ # start from two because the first cluster is 1
  print(paste("Iteration",i))
  k <- length(unique(c[-i]))
  h <- k + 1
  #phi <- phi[1:h]
  c[-i] <- relabel(c[-i])

  if (any(c[-i]==c[i])){
    phi[h] <- rg(1) # when m = 1
  }
  
  w <- NULL; w[h] <- 1
  for (t in 1:k){
    w[t] <- sum(c[-i]==t)
  }

  samp <- 1:h
  prob <- w * dF(y[i],phi) #a=1, m=1
  c[i] <- sample(samp,1,prob=prob)
}
phi <- phi[1:length(unique(c))]

# scala data
#dat <- read.table('../results.txt',header=F)
#c <- dat[1:200,]
#phi <- dat[-c(1:200),]

# Metropolis: Draw new Phi's
N <- 10000; cs <- .3; burn <- N/10
results <- matrix(0,N,length(unique(c)))
mh <- function(k) {  
  #yi <- y[which(c==unique(c)[k])] # same as line below:
  yi <- y[which(c==k)]
  #print(paste("yi: ",yi))
  #Sys.sleep(1)
  lik <- function(phi){
    -.5 * (100*sum((yi-phi)^2) + phi^2)
  }

  cnt <- 0
  out <- rnorm(1)
  for (i in 2:(N+burn)){
    out[i] <- out[i-1]
    cand <- rnorm(1,out[i],cs)
    if (log(runif(1)) < lik(cand) - lik(out[i])){
      out[i] <- cand
      if (i>burn) {cnt <- cnt + 1}
    }
  }

  print(paste('theta',k,': ',cnt/(N-burn),sep=''))
  out[-c(1:burn)]
}

library(foreach)
library(doMC)
registerDoMC(16)
useParallel <- T
if (useParallel) {
  parallel.time <-
  system.time(results <- foreach(k=1:length(unique(c)),.combine=cbind) %dopar% mh(k))
} else {
  b <- Sys.time()
  for (k in 1:length(unique(c))){
    results[,k] <- mh(k)
  }
  e <- Sys.time()
  loop.time <- e-b
}

plotDen <- function(x){
  #plot(density(x))
  mean(x)
}
Sys.sleep(2)
theta <- apply(results,2,plotDen)

#SCALA:
SCALA <- T
if (SCALA ==T){
  system('cd ../; ./run')
  yc <- read.table('../yc.txt',header=T)
  y <- as.numeric(as.character((yc[,1])))
  c <- (yc[,2])
  theta <- read.table('../theta.txt',header=T)[,1]
}
  
r <- 100
new.y <- matrix(0,r,length(theta))
for (i in 1:length(theta)){
  new.y[,i] <- rF(r,theta[i])
}

xlim=c(0,max(r,length(y))); ylim=c(min(y,new.y),max(y,new.y))
plot(y,xlim=xlim,ylim=ylim,cex=5,pch=1,col=(1+c))
for (i in 1:length(theta)){
  points(new.y[,i],col=(i+1),pch=20)
  abline(h=mean(theta[i]),col=i+1)
}
c
table(c)

#ggplot:
#tx <- 1:10
#ty <- 2*tx + rnorm(10)
#t.df <- data.frame(cbind(tx,ty))
#temp <- ggplot(t.df,aes(x=tx,y=ty)) + geom_point()
