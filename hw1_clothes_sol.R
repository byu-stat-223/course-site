# Read in the data from "clothes.csv" file into a data.frame called "clothes".
# Remove all rows from the data.frame where the variable "Price" is empty,
# i.e., equal to "". 
# 2 pts

clothes <- read.csv("hw/hw1/files/clothes.csv", header = TRUE, as.is = TRUE)
clothes <- clothes[clothes$Price != "",]

# Tidyverse
library(tidyverse)
clothes_tbl <- read_csv("hw/hw1/files/clothes.csv") %>% 
  filter(Price != "")

# Write a function called "num" that takes a character vector of prices as
# input, removes the dollar sign, and returns a numeric vector of prices.
# 5 pts

num <- function(string) {
  as.numeric(substr(string, 2, 7))
}

num <- function(string) {
  s <- strsplit(string, "$", fixed = TRUE)
  p <- sapply(s, function(x) x[2])
  as.numeric(p)
}

num <- function(string) {
  as.numeric(gsub("[^0-9.]", "", string))
}

# Using the "num" function you just wrote, convert the variable "Price" in the
# clothes data.frame from a character vector to a numeric vector.
# 1 pt

clothes$Price <- num(clothes$Price)

# Tidyverse
clothes_tbl <- clothes_tbl %>% 
  mutate(Price = num(Price))

# Determine the standard deviation of the price of clothes for each part of the
# body, as indicated by the variable "Body.Location".
# 4 pts

aggregate(Price ~ Body.Location, clothes, sd)

# Tidyverse
clothes_tbl %>% 
  group_by(Body.Location) %>% 
  summarise(sd = sd(Price))

