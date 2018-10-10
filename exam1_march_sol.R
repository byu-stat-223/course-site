## Part 1 ----
### Solution 1
tournament <- function(odds) {
  total_years <- 0
  wins <- 0
  while (wins == 0) {
    total_years <- total_years + 1
    wins <- sum(sample(c(0, 1), 4, replace = TRUE, prob = c(1 - odds, odds)))
  }
  total_years
}

## Part 2 ----
n_reps <- 10000

### Solution 1
historical_odds <- numeric(n_reps)
for (i in 1:n_reps) {
  historical_odds[i] <- tournament(1/136)
}

### Solution 2
historical_odds <- replicate(n_reps, tournament(1/136))

## Part 3 ----
calculated_odds <- numeric(n_reps)
for (i in 1:n_reps) {
  calculated_odds[i] <- tournament(1/54)
}

### Solution 2
calculated_odds <- replicate(n_reps, tournament(1/54))

## Part 4 ----
ci <- function(x, level = 0.95) {
  estimate <- mean(x)
  ci <- estimate + c(-1, 1) * qt(1 - (1 - level)/2, df = length(x) - 1) * sd(x)/sqrt(length(x))
  c(lower = ci[1],
    estimate = estimate,
    upper = ci[2]
  )
}

ci(historical_odds)
ci(calculated_odds)
