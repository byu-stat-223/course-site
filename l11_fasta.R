d <- readLines("f1a9we_.aln.afasta")
d

w <- grepl("^>",d)  # Logical indicating which elements in d match the regular expression: ^>
w

x <- d[w]
x

x <- sub("^>","",x)       # Drop off the > character
x <- sub("_\\.pdb","",x)  # Drop off the suffix
x


### Get the sequences without the dashes

w <- grepl("^>",d) | grepl("^\\s*$",d)   # Which start with > or is empty?
y <- d[!w]

sub("-","",y)        # Oops, only the first dash is replaced
y <- gsub("-","",y)  # Use 'gsub' instead of 'sub'
y

e <- data.frame(id=x,seq=paste(y[1:length(y)%%3 == 0],y[1:length(y)%%3 == 1],y[1:length(y)%%3 == 2],sep=""))
names(e)
dim(e)


