# Read into R the data in the files "cheese1.csv" and "cheese2.csv".  These
# datasets contain the per capita consumption of different cheeses from 1995 to
# 2014. The two data sets cover the same years, although the names for the
# variable holding the year differ.  Merge the data together into a single
# data.frame in which all the consumption data for a given year is one
# observation.  Name your merged data.frame "cheese".  The dimension of this
# data.frame should be 20 rows and 7 columns.
# 5 pts

cheese1 <- read.csv("hw/hw1/files/cheese1.csv", header = TRUE, stringsAsFactors = FALSE)
cheese2 <- read.csv("hw/hw1/files/cheese2.csv", header = TRUE, stringsAsFactors = FALSE)
cheese <- merge(x = cheese1,y = cheese2, by.x = "Year", by.y = "Time")
dim(cheese)
names(cheese)

# Tidyverse
library(tidyverse)
cheese1_tbl <- read_csv("hw/hw1/files/cheese1.csv")
cheese2_tbl <- read_csv("hw/hw1/files/cheese2.csv")

cheese_tbl <- cheese1_tbl %>% 
  inner_join(cheese2_tbl, by = c("Year" = "Time"))

# Which kind of cheese has the highest average consumption per capita over all
# years?
# 4 pts

avgs <- apply(cheese[,names(cheese) != "Year"], 2, mean)
avgs <- apply(cheese[,-1], 2, mean)
avgs <- apply(cheese[,2:ncol(cheese)], 2, mean)
avgs <- apply(cheese[,2:7], 2, mean)

names(which.max(avgs))
names(sort(avgs, decreasing = TRUE))[1]

# Tidyverse
avgs_tbl <- cheese_tbl %>% 
  select(-Year) %>% 
  summarise_all(mean)

avgs_tbl %>% 
  gather(key = "Cheese", value = "Average") %>% 
  filter(Average == max(Average)) %>% 
  select(Cheese)

# Which year saw the largest total consumption per capita of all cheeses?
# 4 pts

totals <- apply(cheese[,names(cheese) != "Year"], 1, sum)
cheese$Year[which.max(totals)]
cheese$Year[totals == max(totals)]

# Tidyverse
totals_tbl <- cheese_tbl %>% 
  gather(key = "Cheese", value = "Consumption", -Year) %>% 
  group_by(Year) %>% 
  summarise(total_consumption = sum(Consumption))

totals_tbl %>% 
  filter(total_consumption == max(total_consumption))
