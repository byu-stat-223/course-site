# The birthday function defined here takes two parameters: n (the number of 
# people in the room) and n_reps (the number of repititions for the simulation)
birthday <- function(n, n_reps = 1000) {
  results <- replicate(n_reps, length(unique(sample(1:365, n, replace = TRUE))) != n)
  estimate <- mean(results)
  ci <- qnorm(0.975) * sqrt(estimate * (1 - estimate) / n_reps)
  c(lower = estimate - ci,
    # The true value can be calculated using the formula given on Wikipedia
    truth = 1 - choose(365, n) * factorial(n) / 365 ^ n,
    estimate = estimate,
    upper = estimate + ci)
}

# Using the function above, calculate the data necessary to create the plot
x <- 2:75
plot_data <- sapply(x, birthday)

# Plot the data
plot(x, plot_data["estimate",], type = "l")
lines(x, plot_data["lower",], type = "l", lty = 2)
lines(x, plot_data["upper",], type = "l", lty = 2)
lines(x, plot_data["truth",], type = "l", col = "red")