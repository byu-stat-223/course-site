w <- c(82, 107, 93)

# What is the mean weight of the three mice?
mean(w)

# How many possible bootstrap samples are there from this sample of three mice?
3 ^ 3

# List all possible bootstrap samples and compute the mean of each.
bs <- expand.grid(w, w, w)
bs_means <- apply(bs, 1, mean)

# Compute the mean of the bootstrap means. How does this compare with the mean
# from before?  Answer: Its the same value.
mean(bs_means)

# Calculate a percentile-based bootstrap confidence interval with level of
# confidence at least 95%. What information does this interval provide? How
# confident are you in reporting this interval? Explain.
quantile(bs_means, c(0.025, 0.975))

# With only only 27 bootstrap samples the precision of the interval and coverage
# should be questioned.
