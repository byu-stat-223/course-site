### Part 1
anova_sim <- function(populations, n_reps = 5000) {
  n_populations <- length(populations)
  p_values <- numeric(n_reps)
  for (i in 1:n_reps) {
    # Create a data.frame to hold population values
    data <- data.frame(numeric(0),numeric(0))
    for (j in 1:n_populations) {
      # Generate data using the functions passed in as part of the populations
      # list (poulations[[j]]() evaluates the jth function in the list)
      data <- rbind(data, data.frame(populations[[j]](),j))
    }
    # Assign names to the data.frame
    names(data) <- c("response", "population")
    # Build linear model
    fm <- lm( response ~ as.factor(population), data = data)
    # Extrac p-value from Anova
    p_values[i] <- anova(fm)$"Pr(>F)"[1]
  }
  # Calculate simulation estimate of the probability of rejecting the null
  # hypothesis when alpha = 0.05
  estimate <- mean(p_values <= 0.05)
  # Build confidence interval around the estimate
  ci <- estimate + c(-1, 1)*qnorm(1 - 0.05/2)*sqrt(estimate*(1 - estimate)/n_reps)
  # Combine estimate with confidence interval
  result <- c(estimate = estimate,lower = ci[1], upper = ci[2])
  # Create list output
  list(prob_rejection = result, p_values = p_values)
}

### Part 2

# Yes, all the ANOVA assumptions are met in this setting.
# Yes, the null hypothesis is true is this setting.

x <- anova_sim(list(function() rnorm(10),
                    function() rnorm(10), 
                    function() rnorm(10)))
hist(x$p_value)
x$prob_rejection

### Part 3

# The normality assumption is violated in this setting.

do3 <- function(n, shape1 = 0.05, shape2 = 0.2, n_reps = 5000) {
  # Calculate metrics necessary for pop3 function
  mean <- shape1/(shape1 + shape2)
  sd <- sqrt((shape1*shape2)/((shape1 + shape2)^2*(shape1 + shape2 + 1)))
  pop3 <- function() (rbeta(n, shape1, shape2) - mean)/sd
  
  # Run simulation with pop3 function
  anova_sim(list(function() rnorm(n), function() rnorm(n), pop3), n_reps)
}

do3(2)$prob_rejection
do3(4)$prob_rejection
do3(10)$prob_rejection

# For small sample sizes, the proportion of p.values less than 0.05 is more
# than 0.05.  That is, the size of the test is bigger than the desired 0.05.
# The consequences of the violation of the normality assumption is mitigated by
# a large sample size as a consequence of the Central Limit Theorem.  When
# n=10, the size of the test is indeed the desired value of 0.05 (up to Monte
# Carlo error).
