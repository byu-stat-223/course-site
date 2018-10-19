ilec <- c(1,1,1,1,2,2,1,1,1,1,2,2,1,1,1,1,2,2,1,1,1,1,2,3,1,1,1,1,2,3,1,1,1,1,2,3,1,1,1,1,2,3,1,1,1,1,2,3,1,1,1,1,2,3,1,1,1,1,2,4,1,1,1,1,2,5,1,1,1,1,2,5,1,1,1,1,2,6,1,1,1,1,2,8,1,1,1,1,2,15,1,1,1,2,2) 

clec <- c(1, 1, 5, 5, 5, 1, 5, 5, 5, 5)

# Test statistic of ratio of variances with p-value (given by the hw question)
var_clec <- var(clec)
var_ilec <- var(ilec)

n_1 <- length(clec)
n_2 <- length(ilec)

observed_test_statistic <- var_clec / var_ilec
pf(observed_test_statistic, n_1 - 1, n_2 - 1, lower.tail = FALSE)

# Now to carry out the permutation test associated with this null.

all_data <- c(ilec, clec)

n_permutations <- 10000
samples <- replicate(n_permutations, {
  clec_perm <- sample(1:length(all_data), n_1)
  var_clec_perm <- var(all_data[clec_perm])
  var_ilec_perm <- var(all_data[-clec_perm])
  var_clec_perm / var_ilec_perm
})

hist(samples, breaks = 40, freq = FALSE)
abline(v = observed_test_statistic, col = 'red')
p_value <- mean(samples >= observed_test_statistic)
ci <- p_value + c(-1, 1) * pnorm(0.975) * sqrt(p_value * (1 - p_value) / length(samples))
c(lower = ci[1],
  p_value = p_value,
  upper = ci[2])

# The p-values are different. I would place more confidence in the
# permutation test that does not rely on the normality assumption.
