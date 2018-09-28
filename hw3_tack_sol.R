# Create a function that simulates flipping a tack as described in the homework
# question
flip_tack <- function(n, prob_up, n_reps) {
  probs <- rbinom(n_reps, n, prob_up) / n
  # Build confidence intervals
  cis <- qnorm(0.975) * sqrt(probs * (1 - probs) / n)
  # Calculate coverage
  mean((probs - cis <= prob_up) & (probs + cis >= prob_up))
}

n_reps <- 1000000
coverage <- flip_tack(50, 0.4, n_reps)
coverage_ci <- coverage + c(-1, 1) * qnorm(0.975) * sqrt(coverage * (1 - coverage) / n_reps)
c(
  lower = coverage_ci[1],
  coverage = coverage,
  upper = coverage_ci[2]
)
