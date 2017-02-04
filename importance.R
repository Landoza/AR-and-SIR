# Demo code for SIR sampling. Grabbed and slightly modified from Stackoverflow: http://stackoverflow.com/questions/22060675/r-monte-carlo-integration-using-importance-sampling

#Without Importance Sampling
set.seed(1909)
X <- runif(100000,0.01,1)
Y <- (1+sinh(2*X)*log(X))^(-1)
c(mean(Y), var(Y))


#Importance sampling Monte Carlo
w <- function(x) dunif(x, 0.01, 1)/dnorm(x, mean=0.5, sd=0.25) * (1-2*pnorm(0, mean=0.5, sd=0.25))
f <- function(x) (1+sinh(2*x)*log(x))^(-1)
X <- rnorm(100000, mean=0.5, sd=0.25)
X <- X[X>0.01 & X<1] # throw away candidate values outside of range here - they won't be used in any case.
Y1 <- w(X)
Y2 <- f(X)
Y <- Y1*Y2
c(mean(Y), var(Y))

