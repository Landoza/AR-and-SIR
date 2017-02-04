# Demo code for rejection sampling. Grabbed (and slightly modified) from R-bloggers 4/8/16. https://www.r-bloggers.com/rejection-sampling/

### Rejection sampling. Let  M(x) be a function such that M(x) = ƒ(x) on [a,b].:
# Until stopping condition is met (e.g. large enough accepted sample size):
# Step 1: Generate T with density m.
# Step 2: Generate U, uniform on [0,1] and independent of T.
# If M(T)*U = ƒ(T), then let X = T (i.e. accept T as a value for X). Otherwise, go to Step 1 (i.e. reject T).

# Here we will estimate a beta(6,3) distribution using 3*Unif(0,1,) distribution as candidate.
sample.x = runif(100000,0,1) # runif(n,min,max) simulates n draws from Unif(min,max).
accept = c()

for(i in 1:length(sample.x))
{
 U = runif(1, 0, 1)
	if(dunif(sample.x[i], 0, 1)*3*U <= dbeta(sample.x[i], 6, 3)) 
	{
	  accept[i] = 'Yes'
	}
	else if(dunif(sample.x[i], 0, 1)*3*U > dbeta(sample.x[i], 6, 3))
	{
	  accept[i] = 'No'
	}
}
T = data.frame(sample.x, accept = factor(accept, levels= c('Yes','No')))

acceptCount = sum(accept=='Yes')
rejectCount = length(accept) - acceptCount
acceptCount
rejectCount

# We can plot the results along with the true distribution with the following code.

hist(T[,1][T$accept=='Yes'], breaks = seq(0,1,0.01), freq = FALSE, main = 'Histogram of X', xlab = 'X')
x = seq(0,1,0.00001)
lines(x, dbeta(x,6,3), col="red")


# With 100,000 observations sampled, the data fit very well.
# We can look at the densities of both the accepted and rejected values to get an idea of what’s going on.

library(ggplot2)
print(qplot(sample.x, data = T, geom = 'density', color = accept))


# Looking at a stacked histogram of all the sampled values together we can really see how much wasted data there are in this example.

print(qplot(sample.x, data = T, geom = 'histogram', fill = accept, binwidth=0.01))