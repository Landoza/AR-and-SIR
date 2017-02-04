########################################################################
## Comparison of Acceptance Rejection and Sampling Importance Resampling
## Created by Keshav Mahindra
########################################################################

## Setting up environment
rm(list = ls()) 
getwd()
options(scipen = 999) ## removes scientific notation

## Loading packages and libraries 
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("Runuran")
library("ggplot2")
library("ggthemes")
library("Runuran")

###################################################### Question 1 ##############################################

## Logs of the target and candidate closed forms show candidate has fatter tails
## So candidate actually dominates target
# unscaled.targ = function(x){log(exp(-0.5*(x-4)^2))}
# candidate.dens = function(x){log(0.5*exp(-abs(x-3)))}

unscaled.targ = function(x){exp(-0.5*(x-4)^2)}
candidate.dens = function(x){0.5*exp(-abs(x-3))}

ggplot(data.frame(x = c(-20, 20)), aes(x)) +
  stat_function(fun = unscaled.targ, geom = "line", aes(colour = "Target")) +
  stat_function(fun = candidate.dens, geom="line", aes(colour = "Candidate")) +
  xlab("θ") + ylab("g(θ|y)") + ggtitle("Target Density vs. Candidate Density") + 
  scale_colour_manual(name="",
                      values=c(Target="red", Candidate="blue")) + theme_gdocs()

## Using calculus and taking the first derivative, the max. of target dist. occurs at (4,1)
## The candidate dist. has value 0.183940 at θ = 4. So the scale factor (M) is set to 1/0.183940
## This is approximately 5.4366 (4dp)
## This didn't allow the candidate to completely cover the target, so I used a scale factor of 10
## found by trial and error, to get a decent fit.

scaled.candidate = function(x){10*candidate.dens(x)}

ggplot(data.frame(x = c(-10, 10)), aes(x)) +
  stat_function(fun = unscaled.targ, geom = "line", aes(colour = "Target")) +
  stat_function(fun = scaled.candidate, geom="line", aes(colour = "Scaled Candidate")) +
  xlab("θ") + ylab("g(θ|y)") + ggtitle("Target Density vs. Scaled Candidate Density") + 
  scale_colour_manual(name="",
                      values=c(Target="red", "Scaled Candidate"="blue")) + theme_gdocs()


## It is observed that the candidate density dominates the target density, as required.

## Draw sample of 50,000 from Laplace(3,1)
sample = urlaplace(50000, 3, 1) 
accept <- c()
reshaped.sample <- numeric()

## Reshape sample using Rejection Sampling

for (i in 1:length(sample)) {
  a = unscaled.targ(sample[i]) ## target density
  b = scaled.candidate(sample[i]) ## scaled candidate density
  test.ratio = a/b ## their ratio
  test.val = runif(1, 0, 1)
  if (test.ratio > test.val) {
    accept[i] = 'Yes'
    reshaped.sample <- c(reshaped.sample, sample[i])
  } else {
    accept[i] = 'No'
  }
}

avg1 <- mean(reshaped.sample)
variance.1 <- var(reshaped.sample)

## Visualizing the histogram of the reshaped sample and the target distrbution to check fit
## Fit is okay, but using a laplace(3,1) for a target w/ mean ~ 4 leads to high rejection rate

T = data.frame(sample, accept = factor(accept, levels= c('Yes','No')))
hist(T[,1][T$accept=='Yes'],breaks = -10:10, freq = FALSE, main = 'Histogram of X', xlab = 'X')
x = seq(-50,50,0.01)
lines(x, unscaled.targ(x), col="red")

print(qplot(sample, data = T, geom = 'density', color = accept))

print(qplot(sample, data = T, geom = 'histogram', fill = accept, binwidth=0.1))

## Simulating 9 more times using Rejection Sampling 

## vectors which store the mean and variance for each reshaped sample
avg2 <- numeric()
variance.2 <- numeric()

for (i in 1:9) {
  
  sample <- urlaplace(50000,3,1)
  accept <- c()
  reshaped.sample <- numeric()
  
  for (i in 1:length(sample)) {
    a = unscaled.targ(sample[i])
    b = scaled.candidate(sample[i])
    test.ratio = a/(b)
    test.val = runif(1, 0, 1)
    if (test.ratio > test.val) {
      accept[i] = 'Yes'
      reshaped.sample <- c(reshaped.sample, sample[i])
    } else {
      accept[i] = 'No'
    }
  }
  
  avg2 <- c(avg2, mean(reshaped.sample))
  variance.2 <- c(variance.2, var(reshaped.sample))
}
 
avg2 <- c(avg2, avg1)
variance.2 <- c(variance.2, variance.1)
## Printing out means and std errors from repeated samples
print(data.frame(avg2, variance.2))
mean(avg2)
se.1 <- sd(avg2)/sqrt(length(avg2)) 


## Repeat process for Sampling Importance Resampling (SIR), with 10 iterations

avg3 <- numeric()
variance.3 <- numeric()

for (i in 1:10) {
  
  sir.sample = urlaplace(50000,3,1)

  ## vectors which organise the SIR approach in the table format found in the notes
  unscaled.val <- numeric()
  candidate.val <- numeric()
  ratios <- numeric()
  weights <- numeric()

  for (i in 1:length(sir.sample)) {
    unscaled.val <- c(unscaled.val, unscaled.targ(sir.sample[i])) ## target density value
    candidate.val <- c(candidate.val, scaled.candidate(sir.sample[i])) ## candidate density value
    ratios <- c(ratios, unscaled.targ(sir.sample[i])/scaled.candidate(sir.sample[i])) ## ratio of the two
  }
  ## Storing sum of all ratios for normalization
  sum.ratio <- sum(ratios)
  ## calculating probabilty weights
  for (i in 1:length(sir.sample)) {
    weights <- c(weights, ratios[i]/sum.ratio) ## weight assigned to particular instance
  }

# head(data.frame(sir.sample, unscaled.val, candidate.val, ratios, weights), 10)

## Sample of 5000 (10% of 50000) with replacement from starting sample.
## Sample probabilities are from the vector of weights calculated above
  reshaped.sample.sir <- sample(sir.sample, size = 5000, replace = TRUE, prob = weights)
  
  avg3 <-c(avg3, mean(reshaped.sample.sir))
  variance.3 <- c(variance.3, var(reshaped.sample.sir))
  
}

## Outputing the means and standard errors
print(data.frame(avg3, variance.3))
mean(avg3)
se.2 <- sd(avg3)/sqrt(length(avg3))

## Visualizing SIR reshaped sample
hist(reshaped.sample.sir, breaks = seq(-20,20,0.5), freq = FALSE, main = 'Histogram of X', xlab = 'X')
x = seq(-20, 20, 0.5)
lines(x, unscaled.targ(x), col="red")

###################################################### Question 2 ##############################################

unscaled.targ.2 = function(x){0.75*exp(-0.5*(x-3)^2) + 0.25*exp(-0.25*(x-6)^2)}
## I converted the N(4, 3^2) into a closed form function to make things easier
candidate.dens.2 = function(x){1/(3*sqrt(2*pi)) * exp(-1/18 * (x-4)^2)}

## Again, checking logs to see if candidate actually dominates target in the tails
# unscaled.targ.2 = function(x){log(0.75*exp(-0.5*(x-3)^2) + 0.25*exp(-0.25*(x-6)^2))}
# candidate.dens.2 = function(x){log(1/(3*sqrt(2*pi)) * exp(-1/18 * (x-4)^2))}

## Plotting the logs of the target and candidate show that the tails of the candidate are fatter
## than the tails of the target, so clearly the candidate dominates the target, as required.

ggplot(data.frame(x = c(-20, 20)), aes(x)) +
  stat_function(fun = unscaled.targ.2, geom = "line", aes(colour = "Target")) +
  stat_function(fun = candidate.dens.2, geom="line", aes(colour = "Candidate")) +
  xlab("θ") + ylab("g(θ|y)") + ggtitle("Target Density vs. Candidate Density") + 
  scale_colour_manual(name="",
                      values=c(Target="red", Candidate="blue")) + theme_gdocs()

## To scale up the candidate, I used trial and error to determine the scale factor of 6.50
## This seemed to fit quite well when visualizing the graphs, without being too inefficient.
scaled.candidate.2 = function(x){6.50 * candidate.dens.2(x)}


ggplot(data.frame(x = c(-20, 20)), aes(x)) +
  stat_function(fun = unscaled.targ.2, geom = "line", aes(colour = "Target")) +
  stat_function(fun = scaled.candidate.2, geom="line", aes(colour = "Scaled Candidate")) +
  xlab("θ") + ylab("g(θ|y)") + ggtitle("Target Density vs. Scaled Candidate Density") + 
  scale_colour_manual(name="",
                      values=c(Target="red", "Scaled Candidate"="blue")) + theme_gdocs()

## Acceptance Rejection Approach for Reshaping Sample

## Drawing 100,000 values from N(4,3^2)
q2.sample <- rnorm(100000, 4, 3)
q2.accept <- c()
q2.reshaped.sample <- numeric()

for (i in 1:length(q2.sample)) {
    a = unscaled.targ.2(q2.sample[i])
    b = scaled.candidate.2(q2.sample[i])
    test.ratio.2 = a/b
    test.val.2 = runif(1,0,1) ## single draw from random uniform
    if (test.ratio.2 > test.val.2) {
      q2.accept[i] = 'Yes'
      q2.reshaped.sample <- c(q2.reshaped.sample, q2.sample[i])
    } else {
      q2.accept[i] = 'No'
    }
}

avg4 <- mean(q2.reshaped.sample)
variance.4 <- var(q2.reshaped.sample)

## Visualizing histogram from Acceptance Rejection approach

T2 = data.frame(q2.sample, q2.accept = factor(q2.accept, levels= c('Yes','No')))
hist(T2[,1][T2$q2.accept=='Yes'],breaks = -50:50, freq = FALSE, main = 'Histogram of X', xlab = 'X')
x = seq(-50,50,0.01)
lines(x, unscaled.targ.2(x), col="red")

print(qplot(q2.sample, data = T2, geom = 'density', color = q2.accept))

print(qplot(q2.sample, data = T2, geom = 'histogram', fill = q2.accept, binwidth=0.1))

## Seems to work fine

## SIR Approach for Reshaping Sample
sir.sample.2 = rnorm(100000,4,3)

unscaled.val.2 <- numeric()
candidate.val.2 <- numeric()
ratios.2 <- numeric()
weights.2 <- numeric()

for (i in 1:length(sir.sample.2)) {
  unscaled.val.2 <- c(unscaled.val.2, unscaled.targ.2(sir.sample.2[i]))
  candidate.val.2 <- c(candidate.val.2, scaled.candidate.2(sir.sample.2[i]))
  ratios.2 <- c(ratios.2, unscaled.targ.2(sir.sample.2[i])/scaled.candidate.2(sir.sample.2[i]))
}
## Storing sum of all ratios for normalization
sum.ratio.2 <- sum(ratios.2)
## calculating probabilty weights
for (i in 1:length(sir.sample.2)) {
  weights.2 <- c(weights.2, ratios.2[i]/sum.ratio.2)
}

## For debugging
# head(data.frame(sir.sample.2, unscaled.val.2, candidate.val.2, ratios.2, weights.2), 10)

sum(weights.2) ## Should add up to 1 (and it does)

## Sample of 10000 (10% of 100000) with replacement from starting sample.
## Sample probabilities are from the vector of weights calculated above
reshaped.sample.sir.2 <- sample(sir.sample.2, size = 10000, replace = TRUE, prob = weights.2)

avg5 <- mean(reshaped.sample.sir.2)
variance.5 <- var(reshaped.sample.sir.2)

## Visualizing SIR reshaped sample
hist(reshaped.sample.sir.2, breaks = seq(-20,20,0.5), freq = FALSE, main = 'Histogram of X', xlab = 'X')
x = seq(-20, 20, 0.5)
lines(x, unscaled.targ.2(x), col="red")

#####################################################################################################
