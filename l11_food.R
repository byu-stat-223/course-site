# The text file FOOD_DES.txt contains long and short descriptions and four
# digit food group codes for over 8,000 food items. Create a data frame named
# 'food' with three columns: 1) the code for the food group an item belongs to,
# 2) the long description of the food item, and 3) the short description of the
# food item. These columns should be named 'code','long_desc', and 'short_desc'
# respectively. For example, for the first observation, the four digit food
# group code is '0100', the long description is 'Butter, salted', and the short
# description is 'BUTTER,WITH SALT'.

path <- "FOOD_DES.txt"
f <- file(path,"rb")
rawText <- readLines(f)

rawText <- readLines("FOOD_DES.txt")

split <- strsplit(rawText,"~\\^~")
smaller <- sapply(split,function(x) x[2:4])
food <- data.frame(code=smaller[1,],long_desc=smaller[2,],short_desc=smaller[3,])

# How many foods are described as including cheese? 

food$cheese <- grepl("[Cc]heese",food$long_desc)
sum(food$cheese)

# The food group code '0100' includes dairy and egg products. What proportion
# of foods with cheese are in this food group? 

sum( food$code=='0100' & food$cheese ) / sum(food$cheese)

# How many foods are described as fat free?

## The hard way.
x3 <- grepl("fat-free",food$long_desc)
x4 <- grepl("fat free",food$long_desc)
x5 <- grepl("Fat-free",food$long_desc)
x2 <- grepl("Fat free",food$long_desc)

food$fat.free <- x2 | x3 | x4 | x5
sum(food$fat.free)

## The easy way.
x6 <- grepl("[Ff]at[- ]free",food$long_desc)
food$fat.free <- x6
sum(food$fat.free)

# Certain foods in the dataset contain a brand name; for example, observation
# 8764 is Post Honey Bunches of Oats, honey roasted. Create a logical vector
# 'brand' which takes on a value of TRUE if the food item is associated with a
# specific brand. For our purposes, brand name products start with all capital
# letters in their long description.

food$brand <- grepl("[A-Z]{2}",food$long_desc)
mean(food$brand)

