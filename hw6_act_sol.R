act <- read.table("act.txt", header = TRUE)

class_1 <- act[act$class == "class1", -3]
class_2 <- act[act$class == "class2", -3]

perm_test <- function(data, n_reps = 10000) {
  replicate(n_reps, {
    p_data <- apply(data, 1, sample)
    mean(p_data[1, ]) - mean(p_data[2, ])
  })
}

cl1_observed <- mean(class_1$post - class_1$pre)
cl2_observed <- mean(class_2$post - class_2$pre)

cl1_observed
cl2_observed

cl1_perm <- perm_test(class_1)
cl2_perm <- perm_test(class_2)

p_1 <- mean(cl1_perm >= cl1_observed)
ci_1 <- p_1 + c(-1, 1) * qnorm(1 - 0.05 / 2) * sqrt(p_1 * (1 - p_1) / length(cl1_perm))
c(lower = ci_1[1],
  p_value = p_1,
  upper = ci_1[2])

p_2 <- mean(cl2_perm >= cl2_observed)
ci_2 <- p_2 + c(-1, 1) * qnorm(1 - 0.05 / 2) * sqrt(p_2 * (1 - p_2) / length(cl2_perm))
c(lower = ci_2[1],
  p_value = p_2,
  upper = ci_2[2])
