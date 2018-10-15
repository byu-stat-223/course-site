all <- scan("EEH_clus4_331.rpt",what=character(),sep="\n")  # This line does the same
all <- readLines("EEH_clus4_331.rpt")                       # thing as this line.

thinned <- all[grepl("^Cen",all)]                           # Only keep the lines that match.
thinned <- all[grep("^Cen",all)]                            # Only keep the lines that match.
cleaned <- gsub("Cen","",thinned)                           # Search and replace "Cen" with ""
data <- read.table(textConnection(cleaned))                 # Read the data into a data.frame
data

