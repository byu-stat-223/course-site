fib <- function(n) {
  n <- floor(n[1])
  seq <- rep(1, times = n)
  if (n <= 2) seq
  else for (i in 3:n) {
    seq[i] <- seq[i - 1] + seq[i - 2]
  }
  seq
}
