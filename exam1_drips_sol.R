## Part 1 ----
### Solution 1
drip <- function(rate = 1, minutes = 120) {
  total_drips <- 0
  for (i in 1:minutes) {
    total_drips <- total_drips + rpois(1, rate)
    rate <- rate * (1 + runif(1, 0, 5)/100)
  }
  total_drips
}

### Solution 2
drip <- function(rate = 1, minutes = 120) {
  sum(replicate(minutes, {
    rpois(1, rate)
    rate <<- rate * (1 + runif(1, 0, 5)/100)
  }))
}

### Solution 3
drip <- function(rate = 1, minutes = 120) {
  rates <- c(rate, replicate(minutes - 1, {
    rate <<- rate * (1 + runif(1, 0, 5)/100)
  }))
  
  sum(rpois(minutes, rates))
}

## Part 2 ----
n_reps <- 10000

### Solution 1
drip_counts <- numeric(n_reps)
for (i in 1:n_reps) {
  drip_counts[i] <- drip()
}

### Solution 2
drip_counts <- replicate(n_reps, drip())

### Solution 3
drip_counts <- unlist(purrr::rerun(n_reps, drip()))

## Part 3 ----
ci <- function(x, level = 0.95) {
  estimate <- mean(x)
  ci <- estimate + c(-1, 1) * qnorm(1 - (1 - level)/2) * sqrt(estimate*(1 - estimate)/n_reps)
  c(lower = ci[1],
    estimate = estimate,
    upper = ci[2])
}

ci(drip_counts > 700)

## Part 4 ----
observed_drips <- 876
ci(drip_counts >= observed_drips)

## Part 5 ----
# Fail to reject the null hypothesis since the p_value and it's confidence interval
# are not less than the alpha level of 0.05