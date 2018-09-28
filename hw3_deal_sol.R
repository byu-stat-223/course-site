# The simulate function is defined in terms of a singal parameter, switch, which
# is a logical value indicating which game strategy is used.
simulate <- function(switch = TRUE) {
  winning_door <- sample(1:3, 1)
  first_chosen_door <- sample(1:3, 1)
  shown_door <- ifelse(winning_door == first_chosen_door,
                       sample(setdiff(1:3, first_chosen_door)),
                       setdiff(1:3, c(first_chosen_door, winning_door)))
  final_door <- ifelse(switch,
                       setdiff(1:3, c(first_chosen_door, shown_door)),
                       first_chosen_door)
  winning_door == final_door
}

# Alternate solution
simulate <- function(switch = TRUE) {
  my_choice <- sample(1:3, 1)
  winner <- sample(1:3, 1)
  ifelse(switch, my_choice != winner, my_choice == winner)
}

# Using the function above, play the game many times and collect the results
n_reps <- 1000

switch_results <- replicate(n_reps, simulate())
no_switch_results <- replicate(n_reps, simulate(FALSE))

# Create confidence interval function
calculate_ci <- function(x, alpha = .05) {
  estimate <- mean(x)
  ci <- qnorm(1 - alpha / 2) * sqrt(estimate * (1 - estimate) / length(x))
  c(
    lower = estimate - ci,
    estimate = estimate,
    upper = estimate + ci
  )
}

switch_win_prob <- calculate_ci(switch_results)
switch_win_prob
no_switch_win_prob <- calculate_ci(no_switch_results)
no_switch_win_prob
