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
p = seq(0,1,0.001)

# Calculate the likelihood
likelihood = dbinom(num_heads,num_coinflips,p)

# Plot the likelihood
plot(p,likelihood, type = "b", pch = 19)

# Plot the maximum likelihood
plot(p,likelihood, type = "l", lwd = 2)
max_likelihood = which(likelihood == max(likelihood))
lines(c(p[max_likelihood], p[max_likelihood]), c(-1, max(likelihood)), lty = 2, lwd = 2, col = "red")

