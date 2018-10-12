roommate <- c(6, 6.25, 5.75, 6.5, 6, 15, 35, 20, 4, 5.25, 5.75)
you <- c(3.5, 5.5, 4, 4, 4, 6, 11, 12, 3, 4, 4.25, 7, 3.25)

t.test(roommate, you, conf.level = 0.9)

# Define bootstrap function to calculate difference in means of bootstrap samples
bootstrap <- function(x1, x2) {
  mean(sample(x1, replace = TRUE)) - mean(sample(x2, replace = TRUE))
}

# Create 10000 bootstrap samples
n_samples <- 10000
b_samples <- replicate(n_samples, bootstrap(roommate, you))

# Calculate the confidence interval with the resulting vector using quantile()
ci <- quantile(b_samples, probs = c(0.05, 0.95))
ci

# Based on a bootstrap 90% confidence interval, we can reject the null hypothesis
# and conclude that there is a difference in shower times.