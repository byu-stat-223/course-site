# The data file read in is the box score for the opening day game between the
# Arizona Diamondbacks and Colorado Rockies in 2016. This code reads in the
# data and combines the stats for both teams.

download.file("https://espn.go.com/mlb/boxscore?gameId=360404129", destfile = "azd.html", method = "curl")
    
library(XML)

dat <- readHTMLTable("azd.html")
dat

# The Rockies stats are the 5th element of the list and the D-backs stats are
# the 7th element. We can combine the two data sets with a rbind. Everything is
# already labeled well.

stats <- rbind(dat[[5]],dat[[7]])
names(stats)
w <- setdiff(names(stats)[sapply(stats,is.factor)],"Hitters")  # Which variables (other than "Hitters") are factors?

for ( var in w ) {
  stats[,var] <- as.numeric(as.character(stats[,var]))  # Convert the factors to numerics.
}
sapply(stats,is.factor)

# Some of the records have footnotes. These are denoted by lowercase letters
# and a dash in front of the name. Remove these annotations.

stats$Hitters <- sub("^[a-z]-","",stats$Hitters)  # This is equivalent
stats$Hitters <- sub("^\\w-","",stats$Hitters)    # to this.

# To see how well the pitchers hit in the game?  In particular, find all
# hitters whose name ends with "P" (which designates pitchers) and compute the
# average of the recorded baseball statistics.

pitchers <- grepl(" P$",stats$Hitters)
apply(stats[pitchers,-1],2,mean)

# Do the same for all the outfielders. These will have "CF", "LF", or "RF" in
# the name.

outfielders <- grepl(" [CLR]F$",stats$Hitters)
apply(stats[outfielders,-1],2,mean)

