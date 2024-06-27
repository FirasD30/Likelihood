#--------------------------------------------------------
# Calculate probabilities for coin flips
# assuming that these data follow a binomial distribution
# (essentially iid trials, with successes and failures)
#--------------------------------------------------------
library(tidyverse)

# Number coinflips
num_coinflips = 10

# Number of potential heads
num_heads = seq(0:num_coinflips)

# Track total probability
probability_total = 0

# Probability of coin landing on heads
prob_heads = 0.5

# Loop through probabilities for total number of heads
# Assuming an equal coin probability
print("Number of trials is ", num_coinflips)
for (ii in 1:length(num_heads))
{
  # Probability density for each result
  prob_result = dbinom(ii, num_coinflips, prob_heads)
  print(paste("Probability", ii, "heads = ", prob_result, sep=" "))
  probability_total = probability_total + prob_result
}

print(paste("Summed probability is: ", probability_total))

# Vectorized version
prob_result = dbinom(0:num_coinflips, num_coinflips, prob_heads)
probability_observations = tibble(num_heads = 0:num_coinflips, probability = prob_result)

# Plot these probabilities
plot(probability_observations, type = "b", pch = 20)


# Probability gives you the relative chance of seeing each outcome
# Can test this by generating random samples from the binomial distribution directly
# Repeat the experiment many many times
multiple_experiments = rbinom(20000,num_coinflips,prob_heads)

# Plot a histogram for the summed frequencies of each outcome
hist(multiple_experiments, breaks = seq(-0.5,num_coinflips + 0.5), col = "black")

# Try doing the same thing for a continuous distribution (e.g. normal)