#y <- c(-1.48,-1.4,-1.16,-1.08,-1.02,.14,.51,.53,.78)
#plot(y)
#
#x <- c(rnorm(50,0,1), rnorm(50,7,1), rnorm(50,15,1))
#plot(x)


#n <- 9
#a <- 1 # large a -> uniform; small a -> dirac delta

crp <- function(n=9,a=1,plot=F) {

  customer <- 1
  table <- 1

  for (i in 2:n){
    K <- length(table)
    samp <- 1:(K+1)
    prob <- c(table[1:K]/(a+i-1),a/(a+i-1))
    customer[i] <- sample(samp,1,prob=prob)
    if (customer[i] == K+1) {
      table[K+1] <- 1
    } else {  
      table[customer[i]] <- table[customer[i]] + 1
    }  
  }

  out <- table
  if (plot) plot(1:length(out),out,type='o')
  customer
}

#crp(plot=T)
