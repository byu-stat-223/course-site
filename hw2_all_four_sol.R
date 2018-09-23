# This function simulates buying cereal until all n prizes are found. The function
# takes parameter prob which defines the probablities for each item. The number
# of total items is derived from the length of prob.
buy_cereal <- function(prob) {
  n_items <- length(prob)
  counts <- numeric(n_items)
  while (any(counts == 0)) {
    new_box <- sample(1:n_items, 1, prob = prob)
    counts[new_box] <- counts[new_box] + 1
  }
  sum(counts)
}

# Using the function above, the function below simulates buying cereal n_reps
# times.
simulate <- function(prob = rep(.25, 4), cutoff = 14, n_reps = 1000) {
  z <- qnorm(0.05/2, lower.tail = FALSE)
  all_counts <- replicate(n_reps, buy_cereal(prob))
  average_boxes <- mean(all_counts)
  ci_mean <- z * sd(all_counts) / sqrt(n_reps)
  cutoff_prob <- mean(all_counts >= cutoff)
  ci_prob <- z * sqrt(cutoff_prob * (1 - cutoff_prob) / n_reps)
  list(
    expected_boxes = c(
      lower = average_boxes - ci_mean,
      expected_boxes = average_boxes,
      upper = average_boxes + ci_mean
    ),
    cutoff = c(
      lower = cutoff_prob - ci_prob,
      cutoff_prob = cutoff_prob,
      upper = cutoff_prob + ci_prob
    )
  )
}

simulate()
simulate(c(0.10, 0.25, 0.25, 0.40))