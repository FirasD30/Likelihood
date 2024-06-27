#---------------------------------------------------------------------------
# Calculate likelihoods (after observing a particular set of data)
# Have observed that a coin came up heads X times out of N coin tosses
# What does it say about the binomial parameter p (probability of heads)? 
# What is the likelihood of each value of p?
# How likely is any outcome given observed data
#----------------------------------------------------------------------------
library(tidyverse)

# Number coinflips
num_coinflips = 10

# Number of heads observed
num_heads = 6

# Possible values for p parameter (probability of heads)
p = seq(0,1,0.1)

# Vector to hold the likelihood
likelihood = rep(NA,length(p))

# Calculate the likelihood
for (ii in 1:length(p))
{
  likelihood[ii] = dbinom(num_heads, num_coinflips,p[ii])
  print(paste("Parameter value:", p[ii], "likelihood = ", likelihood[ii], sep = " "))
}

# Likelihood tells you how likely ("relative probability") each value of a 
# parameter is, given a statistical distribution and data (observations) - e.g. 
# the probability that the binomial parameter is p, 
# assuming that data come from a binomial distribution.
# It does not sum to one
print(paste("Summed likelihood is: ", sum(likelihood)))

# Vectorized version
likelihood = dbinom(num_heads,num_coinflips,p)

# Plot the likelihood
plot(p,likelihood, type = "b", pch = 19)

# Plot the maximum likelihood
plot(p,likelihood, type = "l", lwd = 2)
max_likelihood = which(likelihood == max(likelihood))
lines(c(p[max_likelihood], p[max_likelihood]), c(-1, max(likelihood)), lty = 2, lwd = 2, col = "red")

# Now try it with many more values of p
p = seq(0,1,0.1)
likelihood = dbinom(num_heads,num_coinflips,p)
plot(p,likelihood, type = "l")
p = seq(0,1,0.01)
likelihood = dbinom(num_heads, num_coinflips, p)
plot(p,likelihood, type = "l")
p = seq(0,1,0.001)
likelihood = dbinom(num_heads, num_coinflips, p)
plot(p,likelihood, type = "l")

# Note that this likelihood is different because it has been calculated 
# at a finer interval. Again, does not sum to one
print(paste("Summed likelihood is: ", sum(likelihood)))

