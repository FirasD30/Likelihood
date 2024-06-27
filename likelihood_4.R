#---------------------------------------------------------------------------
# Calculate maximum likelihood for the coin-flip example from class
#----------------------------------------------------------------------------
library(stats4)
# Number coinflips
num_coinflips = 10

# Number of heads observed
num_heads = 6

#------------------------------------------------------------------------------------------
# Function to calculate the negative log-likelihood for your observation
# Calculate MLE for the above
negLogLik <- function(p)
{
  ll = sum(log(dbinom(num_heads,num_coinflips,p)))
  return(-ll)
}

# Optimization function to calculate maximum likelihood
mle(negLogLik,start = list(p = 0.5))

# Note that it gives a warning because when p is 0 or 1, the binomial density
# is 0, and hence the log of zero is zero. You can use bounds to avoid this
mle(negLogLik,start = list(p = 0.5),  lower = 0.0001, upper = 0.9999)

# Works with repeated sampling and vectorized data
num_trials = 10000
num_heads = rbinom(num_trials, num_coinflips, 0.6)
mle(negLogLik, start = list(p = 0.5))




# Of course, you can write your own likelihood function for any model - that is its power

