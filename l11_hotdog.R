d <- read.table("hotdog.txt",skip=8,sep=",")


# R requires escaping the backslash

raw <- readLines("hotdog.txt")
linesToSkip <- grep("^[\\|\\+]",raw)    # This gives the same result as
linesToSkip <- grep("^(\\+|\\|)",raw)   # this.
dataLines <- raw[-linesToSkip]

linesToKeep <- grep("^[a-zA-Z0-9]",raw)
dataLines <- raw[linesToKeep]

d <- read.table(textConnection(dataLines),sep=",")
names(d) <- c("type","calories","sodium")
boxplot(calories ~ type, data=d)

