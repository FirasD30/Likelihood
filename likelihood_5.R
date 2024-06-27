#-------------------------------------------------------------------------------
# Green crab example from Fulton et al. (2013) . Calculate likelihoods and find 
# maximum likelihood estimates for the crab sample, assuming that the carapace 
# sizes are normally distributed
#-------------------------------------------------------------------------------
library(stats4)

# Number of crabs sampled
sample_size = 50

# Mean carapace size
mean_crab_size = 61

# SD size
sd_crab_size = 6

# Generate random sample using those parameters
crab_sample <- rnorm(sample_size,mean_crab_size,sd_crab_size)

# Calculate the negative log likelihood. This is doing it the explicit way - 
# you can just look up the specific form of the function for normal likelihood
# on wikipedia. In reality, you wouldn't write this code as there are simple 
# functions to calculate MLEs for common distributions - see code at the end of 
# this file
negLogLikNormal <- function(pars)
{
  # Pars[1] is mean, pars[2] is sigma
  temp = length(crab_sample) / 2 * log(2 * pi * pars[2]^2) + sum((1 / (2 * pars[2]^2)) * ((crab_sample - pars[1])^2))
  return(temp)
}

# Get the MLE
find_likelihood = optim(c(50,5), negLogLikNormal, method = "L-BFGS-B", lower = c(-Inf, 0.00001))
mle_results = tibble(mean = find_likelihood$par[1], sd = find_likelihood$par[2])
mle_results

# Plot the likelihood profile for sigma
sigma_search = seq(3,9,0.05)
mu = find_likelihood$par[1]
sigma_dist = vector(length = length(sigma_search))  
for (ii in 1:length(sigma_search))
  sigma_dist[ii] = negLogLikNormal(c(mu, sigma_search[ii]))

plot(sigma_search, -sigma_dist, type = "l", lwd= 2, xlab = "Sigma", ylab = "Log likelihood", cex = 1.5)

# Do a likelihood ratio test to calculate the  95% profile CIs
min_ll = min(sigma_dist)
test_statistic = vector(length = length(sigma_dist))
in_cis = vector(length = length(sigma_dist))
for (ii in 1:length(sigma_dist))
{
  # Calculate 2 * the log likelihood ratio 
  test_statistic[ii] = -2 * (min_ll - sigma_dist[ii])
  
  # Calculate whether within the Chi-square distribution confidence bounds
  # qchisq(0.95,1) distributed when the sample size is large
  if (test_statistic[ii] < 3.841459)
    in_cis[ii] = TRUE
  else
    in_cis[ii] = FALSE
  
}

# Plot the likelihood surface contour
mu_search = seq(40,80,0.05)
sigma_search = seq(3,9,0.05)

xmat <- matrix(NA,length(sigma_search), length(mu_search))

for (ii in 1:length(sigma_search))
{
  for (jj in 1:length(mu_search))
    xmat[ii,jj] = negLogLikNormal(c(mu_search[jj], sigma_search[ii]))

}

filled.contour(sigma_search, mu_search, -xmat, levels = seq(-400,-150,15), 
               plot.title = title(main = "Log likelihood",
                                  xlab = "Sigma", ylab = "Mu"))#, level = c(min_ll + 3.84/2, min_ll + 10, min_ll + 20, min_ll + 30, min_ll + 40, min_ll + 50), xlab = "Sigma", ylab = "Mu")


# In reality, can calculate likelihood much more easily for common distributions using mle function in r
# and probability density of the distribution. Just be careful if the models are non-nested and using AIC
# because you may (depends on the situation) need to include the likelihood constant c.
# This is equivalent to the log likelihood function written out above
NegLogLik1=function(mu,sigma) {
  -sum(dnorm(crab_sample,mean=mu,sd=sigma,log=TRUE))
}

mlefit = mle(minuslogl = NegLogLik1, start = list(mu = 0, sigma = 1), method = "L-BFGS-B", lower = c(-Inf, 0.00001))
summary(mlefit)


