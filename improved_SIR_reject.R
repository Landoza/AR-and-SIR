# We modify the demo code for rejection sampling to obtain the SIR algorithm. For SIR the aim is to sample from dense regions of the target more than sparse regions.

### Recall the basic rejection sampling approach: Let  M(x) be a function such that M(x) = ƒ(x) on [a,b].:
# Until stopping condition is met (e.g. large enough accepted sample size):
# Step 1: Generate T with density m.
# Step 2: Generate U, uniform on [0,1] and independent of T.
# If M(T)*U = ƒ(T), then let X = T (i.e. accept T as a value for X). Otherwise, go to Step 1 (i.e. reject T).

# Here we will estimate a beta(6,3) distribution using 3*Unif(0,1) distribution as candidate. For HW use the given candidate distributions.
# We use a lot of the same quantities as in the basic rejection sampling approach.

rm(list=ls()) #First clear the R environment

cand_sample = runif(1000000,0,1) #Draw a large sample of values of theta from the candidate (starting) distribution. First column of the table in the Chapter 2 slides.
head(cand_sample)

post_sample = dbeta(cand_sample, 6, 3) #Calculate the value of the unscaled posterior at each value sampled. Second column of the table in the slides.
head(post_sample)
max_post = max(post_sample) #Find the peak value of the target - can use this to estimate the scaling term M. Here it is about 2.6, so we'll add a small "fudge factor" and use 3 for safety.
max_post

cand_vals = 3*dunif(cand_sample,0,1) #Calculate the value of the scaled starting distribution at each value sampled. Third column in the notes. 
#(This is not strictly necessary if we use a uniform distribution, the values will all be the same...! For other candidate distributions they won't be.)
head(cand_vals)

ratios = post_sample/cand_vals #Calculate the ratios of the unscaled posterior values to the starting distribution for each value.
head(ratios)
normalizer = sum(ratios) #Sum the entries in this vector to get the normalizing term for the weights.
normalizer

samp_weights = ratios/normalizer #Calculate the sampling weights. Fifth column in the slides.
head(samp_weights)
sum(samp_weights) #Should be 1
hist(samp_weights) #Ideally this should be left skewed, i.e. lots of high weights, but here it isn't.


###############################
# MAKING THE FINAL SIR SAMPLE #
###############################
samp_size = ceiling(length(cand_sample)/10) #Set sample size as 10% of size of sample from prior (This needs to be an integer - here I rounded up.). "Final step" in slides.

final_sample = sample(cand_sample, size=samp_size, replace = T, prob = samp_weights) #This is our sample from the posterior distribution over the parameter.

# We can plot the results along with the true distribution with the following code.
hist(final_sample, breaks = seq(0,1,0.005), freq = FALSE, main = 'Histogram of X', xlab = 'X')
x = seq(0,1,0.00001)
lines(x, dbeta(x,6,3), col="red")