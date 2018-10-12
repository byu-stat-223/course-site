# Load trees data
data(trees)

# Establish the number of bootstrap samples
n_samples <- 5000

# Calculate ratios based on 5000 bootstrap samples
ratios <- replicate(n_samples, {
  # Bootstrap sample rows of trees
  bs_trees <- trees[sample(1:nrow(trees), replace = TRUE), ]
  
  # Build linear model based on bootstrapped data
  bs_lm_out <- lm(Volume ~ Height + Girth, data = bs_trees)
  
  # Calculate ratio
  bs_lm_out$coef[2] / bs_lm_out$coef[3]
})

# Build a confidence interval on ratios using quantile()
quantile(ratios, c(0.025, 0.975))
